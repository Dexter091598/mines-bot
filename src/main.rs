#[macro_use]
extern crate itertools;
#[macro_use]
extern crate clap;
#[macro_use]
extern crate num_derive;
#[macro_use]
extern crate log;

use autopilot::{
	bitmap::{capture_screen, Bitmap},
	geometry::{Point, Rect, Size},
	mouse::{click, move_to, Button},
};
use core::hash::BuildHasherDefault;
use image::open;
use itertools::Itertools;
use ndarray::prelude::*;
use num_traits::cast::{FromPrimitive, ToPrimitive};
use rand::prelude::*;
use rayon::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
use std::{cmp::min, collections::HashSet, thread::sleep, time::Duration};

type Pos = (usize, usize);

#[derive(Eq, Clone)]
struct Detector {
	pos: Pos,
	mines_left: usize,
	unexplored: FxHashSet<Pos>,
}
impl Detector {
	fn new(pos: Pos, mines_left: usize, unexplored: FxHashSet<Pos>) -> Self {
		Self {
			pos,
			mines_left,
			unexplored,
		}
	}
}
impl std::hash::Hash for Detector {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.pos.hash(state);
		self.mines_left.hash(state);
	}
}
impl PartialEq for Detector {
	fn eq(&self, other: &Self) -> bool {
		self.pos == other.pos && self.mines_left == other.mines_left
	}
}
impl std::fmt::Debug for Detector {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Detector{:?}", self.pos)
	}
}

#[derive(FromPrimitive, ToPrimitive, Copy, Clone, PartialEq, Eq)]
enum Square {
	Empty,
	One,
	Two,
	Three,
	Four,
	Five,
	Six,
	Seven,
	Eight,
	Unexplored,
	Mine,
}
impl std::ops::Sub<u8> for Square {
	type Output = u8;

	fn sub(self, other: u8) -> u8 {
		self.to_u8().unwrap() - other
	}
}
/*
impl PartialEq<u8> for Square {
	fn eq(&self, other: &u8) -> bool {
		self.to_u8().unwrap() == *other
	}
}
*/

#[allow(clippy::cognitive_complexity)]
fn main() {
	pretty_env_logger::init();
	let matches = clap_app!((
		crate_name!()
			.split('-')
			.map(|w| {
				let mut c = w.chars();
				c.next().unwrap().to_uppercase().collect::<String>() + c.as_str()
			})
			.collect::<String>()
		) =>
		(version: crate_version!())
		(author: crate_authors!())
		(about: crate_description!())
		(@arg cooldown: -c --cooldown
			+takes_value
			default_value("0")
			value_name[ms]
			"Sets cooldown in milliseconds between every grid update."
		)
		(@arg mines: -m --mines
			+takes_value
			value_name[int]
			"Total number of mines on the grid (Requred for custom games.)"
		)
		(@arg no_flags: -f --("no-flags") "Don't lose time on flagging mines.")
		(@arg tiles: -t --tiles
			+takes_value
			default_value("default")
			value_name("folder_name")
			"Sets custom tiles directory."
		)
		(@arg smooth_move: -s --smooth
			+takes_value
			value_name("speed")
			"Smooth mouse movement."
		)
		(@arg instant: -i --instant)
	)
	.get_matches();

	let (smooth, smooth_speed) = {
		if matches.is_present("smooth_move") {
			(
				true,
				matches
					.value_of("smooth_move")
					.and_then(|val| val.parse::<f64>().ok()),
			)
		} else {
			(false, None)
		}
	};
	let instant = matches.is_present("instant");

	let cooldown = matches
		.value_of("cooldown")
		.map_or(0, |val| val.parse::<u64>().unwrap());
	let no_flags = matches.is_present("no_flags");
	let tiles_dir = format!("tiles/{}", matches.value_of("tiles").unwrap_or("default"));

	// Pre-init
	let mut rng = thread_rng();
	let unexplored_tile = Bitmap::new(open(format!("{}/9.png", tiles_dir)).unwrap(), None);

	let mut tiles = (0..=8)
		.map(|n| {
			(
				Square::from_u8(n).unwrap(),
				Bitmap::new(open(format!("{}/{}.png", tiles_dir, n)).unwrap(), None),
			)
		})
		.collect::<Vec<(Square, Bitmap)>>();
	tiles.reserve(1);
	tiles.push((
		Square::Mine,
		Bitmap::new(open(format!("{}/mine.png", tiles_dir)).unwrap(), None),
	));

	/*let point = Point::new(9.0, 7.0);
	let mut colors = (0..=8)
		.map(|n| {
			(
				Square::from_u8(n).unwrap(),
				Bitmap::new(open(format!("{}/{}.png", tiles_dir, n)).unwrap(), None).get_pixel(point),
			)
		})
		.collect::<Vec<(Square, Rgba<u8>)>>();
	colors.reserve(1);
	colors.push((
		Square::Mine,
		Bitmap::new(open(format!("{}/mine.png", tiles_dir)).unwrap(), None).get_pixel(point),
	));*/

	let tile_size = unexplored_tile.size;
	let full_tile = tile_size.width;
	let half_tile = full_tile / 2.0;

	// Init
	debug!("Capturing screen...");
	let screen = capture_screen().unwrap();
	debug!("Searching for unexplored tiles...");
	let all_unexplored = screen.find_every_bitmap(&unexplored_tile, None, None, None);
	if all_unexplored.is_empty() {
		panic!("Can't find game on screen");
	}

	debug!("Grouping...");
	let grouped = &all_unexplored
		.into_iter()
		.group_by(|p| p.x)
		.into_iter()
		.map(|g| (g.0, g.1.collect::<Vec<_>>()))
		.collect::<Vec<_>>();

	debug!("Creating coordinates grid...");
	let size = (grouped.len(), grouped[0].1.len());
	let (x_size, y_size) = size;

	let total_mines = match matches.value_of("mines") {
		Some(val) => val.parse().unwrap(),
		None => {
			match size {
				(8, 8) => 10,
				(9, 9) => 10,
				(16, 16) => 40,
				(30, 16) => 99,
				_ => panic!("Can't detect number of mines automaticly for shape {:?}. Use (-m, --mines) parameter to specify it.", size),
			}
		},
	};
	let mut mines_left = total_mines;

	let mut grid = Array2::<Square>::from_elem(size, Square::Unexplored);

	let coords = grouped
		.par_iter()
		.enumerate()
		.flat_map(|(x, (_k, g))| g.par_iter().enumerate().map(move |(y, &p)| ((x, y), p)))
		.collect::<FxHashMap<Pos, Point>>();

	debug!("--- Initial State Success ---");
	let start = (x_size / 2, y_size / 2);

	let c = coords[&start];
	move_to(Point::new(c.x + half_tile, c.y + half_tile))
		.unwrap_or_else(|_| panic!("Can't move cursor to {:?}", c));
	click(Button::Left, Some(0));

	let mut solved = FxHashSet::default();
	let mut flagged = FxHashSet::<Pos>::default();

	let mut to_update = FxHashSet::default();
	let mut to_update_reserve = FxHashSet::default();
	to_update.insert(start);

	let mut to_solve = FxHashSet::default();

	// Main loop
	let mut success = false;
	loop {
		// Updating grid
		trace!("Updating grid...");
		if !to_update.is_empty() {
			let finished = update(
				&to_update,
				&mut to_solve,
				&mut grid,
				&coords,
				&solved,
				&tiles,
				tile_size,
				// &colors,
				size,
				cooldown,
			);
			if finished {
				debug!("Game finished: lose.");
				break;
			}
			to_update_reserve.par_extend(&to_update);
			to_update.clear();
		} else if !to_update_reserve.is_empty() {
			let finished = update(
				&to_update_reserve,
				&mut to_solve,
				&mut grid,
				&coords,
				&solved,
				&tiles,
				tile_size,
				// &colors,
				size,
				cooldown,
			);
			if finished {
				debug!("Game finished: lose.");
				break;
			}
		}

		// Solving state
		trace!("Solving state...");
		let mut to_open = FxHashSet::default();
		let mut to_flag = FxHashSet::default();
		let mut clicked = false;

		// Simple solver
		to_solve = to_solve
			.drain()
			.filter(|&pos| {
				/*let mut flagged_mines = 0;
				let mut unexplored = Vec::with_capacity(8);
				neighbors(pos, size).into_iter().for_each(|npos| {
					if flagged.contains(&npos) {
						flagged_mines += 1;
					} else if grid[npos] == Square::Unexplored {
						unexplored.push(npos);
					}
				});*/

				let (flagged_mines, unexplored): (u8, Vec<Pos>) = neighbors(pos, size)
					.into_par_iter()
					.fold(
						|| (0, Vec::with_capacity(8)),
						|(mut flagged_mines, mut unexplored), npos| {
							if flagged.contains(&npos) {
								flagged_mines += 1;
							} else if grid[npos] == Square::Unexplored {
								unexplored.push(npos);
							}
							(flagged_mines, unexplored)
						},
					)
					.reduce(
						|| (0, Vec::with_capacity(8)),
						|(flagged_mines, mut unexplored), (n_flagged_mines, n_unexplored)| {
							unexplored.par_extend(n_unexplored);
							(flagged_mines + n_flagged_mines, unexplored)
						},
					);

				if !unexplored.is_empty() {
					let mines_left_around = grid[pos] - flagged_mines;
					if mines_left_around == 0 {
						if instant {
							unexplored.iter().for_each(|pos| {
								open_square(*pos, &coords, half_tile);
							});
							clicked = true;
						} else {
							to_open.par_extend(&unexplored);
						}
						to_update.par_extend(unexplored);
						solved.insert(pos);
						return false;
					} else if mines_left_around == unexplored.len() as u8 {
						mines_left -= unexplored.len();
						if !no_flags {
							if instant {
								unexplored.iter().for_each(|pos| {
									flag_square(*pos, &coords, half_tile);
								});
								clicked = true;
							} else {
								to_flag.par_extend(&unexplored);
							}
						}
						flagged.par_extend(&unexplored);
						solved.par_extend(unexplored);
						solved.insert(pos);
						return false;
					}
					return true;
				}
				// don't try to solve again if all neighbors already explored
				false
			})
			.collect();

		// End game solver
		if mines_left == 0 {
			success = true;

			debug!("All mines founded - opening remaining squares...");
			if instant {
				iproduct!(0..x_size, 0..y_size)
					.filter(|p| grid[*p] == Square::Unexplored && !solved.contains(p))
					.for_each(|pos| {
						open_square(pos, &coords, half_tile);
					});
				debug!("Game finished: win.");
				break;
			} else {
				to_open.extend(
					iproduct!(0..x_size, 0..y_size)
						.filter(|p| grid[*p] == Square::Unexplored && !solved.contains(p)),
				);
			}

		// Complex solver
		} else if !clicked || (!instant && (to_open.is_empty() && to_flag.is_empty())) {
			trace!("Calculating probabilities...");

			// separating to sectors
			trace!("separating to sectors");
			// let mut sectors: Vec<FxHashSet<Detector>> = Vec::new();
			let mut sectors: Vec<Vec<Detector>> = Vec::new();

			let mut in_sector = FxHashSet::<Pos>::default();
			let mut s = 0;
			let get_neighbors = |pos| {
				let mut flagged_mines = 0;
				let mut unexplored =
					HashSet::with_capacity_and_hasher(8, BuildHasherDefault::<FxHasher>::default());
				let mut detectors =
					IndexSet::with_capacity_and_hasher(8, BuildHasherDefault::<FxHasher>::default());

				neighbors(pos, size).into_iter().for_each(|npos| {
					if flagged.contains(&npos) {
						flagged_mines += 1;
					} else {
						match grid[npos] {
							Square::Unexplored => {
								unexplored.insert(npos);
							}
							Square::Empty => {}
							_ => {
								let mut n_flagged_mines = 0;
								let mut n_unexplored = HashSet::with_capacity_and_hasher(
									8,
									BuildHasherDefault::<FxHasher>::default(),
								);
								neighbors(npos, size).into_iter().for_each(|n_npos| {
									if flagged.contains(&n_npos) {
										n_flagged_mines += 1;
									} else if grid[n_npos] == Square::Unexplored {
										n_unexplored.insert(n_npos);
									}
								});
								if !unexplored.is_disjoint(&n_unexplored) {
									detectors.insert(Detector::new(
										npos,
										(grid[npos] - n_flagged_mines) as usize,
										n_unexplored,
									));
								}
							}
						}
					}
				});

				(
					Detector::new(pos, (grid[pos] - flagged_mines) as usize, unexplored),
					detectors,
				)
			};

			to_solve.iter().for_each(|&pos| {
				if in_sector.contains(&pos) {
					return;
				}

				let (detector, pos_neighbors) = get_neighbors(pos);

				if pos_neighbors.is_empty() {
					in_sector.insert(pos);
				} else {
					let mut seed_set = pos_neighbors;
					seed_set.insert(detector.clone());

					match sectors.get_mut(s) {
						Some(sector) => sector.push(detector),
						None => sectors.push(vec![detector]),
					}

					while let Some(q) = seed_set.pop() {
						if !in_sector.contains(&q.pos) {
							let q_neighbors = get_neighbors(q.pos).1;
							seed_set.par_extend(q_neighbors);

							sectors.get_mut(s).unwrap().push(q.clone());
							in_sector.insert(q.pos);
						}
					}
					s += 1;
				}
			});

			// creating all possible solutions for each sector

			trace!("creating all possible solutions for each sector");
			// let mut probabilities = FxHashMap::default();
			// sectors.into_iter().for_each(|s| {
			let probabilities = sectors
				.par_iter()
				.filter_map(|s| {
					trace!("sector: {:?}", s);
					let s_unexplored = s
						.par_iter()
						.flat_map(|dt| dt.unexplored.clone())
						.collect::<HashSet<Pos>>();
					let solutions = (1..=min(s_unexplored.len(), mines_left))
						.into_par_iter()
						.flat_map(|n| {
							trace!("iteration: {}", n);
							s_unexplored
								.iter()
								.combinations(n)
								.filter(|sol| {
									s.par_iter().all(|dt| {
										dt.unexplored
											.intersection(
												&sol.par_iter().copied().copied().collect::<FxHashSet<Pos>>(),
											)
											.count() == dt.mines_left
									})
								})
								.collect::<Vec<_>>()
						})
						.collect::<Vec<_>>();
					if !solutions.is_empty() {
						let mut weights = s_unexplored
							.par_iter()
							.map(|&p| (p, 0))
							.collect::<FxHashMap<Pos, usize>>();

						let solutions_count = solutions.len() as f64;
						/*solutions.iter().for_each(|sol| {
							sol.iter().for_each(|&pos| {
								*weights.entry(*pos).or_default() += 1;
							});
						});*/
						solutions.into_iter().flatten().for_each(|pos| {
							*weights.entry(*pos).or_default() += 1;
						});

						/*weights.into_iter().for_each(|(p, w)| {
							probabilities.insert(p, (w as f64) / solutions_count);
						});*/
						return Some(
							weights
								.into_par_iter()
								.map(move |(p, w)| (p, (w as f64) / solutions_count)),
						);
					}
					None
				})
				.flatten()
				.collect::<FxHashMap<Pos, f64>>();

			if !probabilities.is_empty() {
				/*let mut all_zero = Vec::new();
				let mut all_one = Vec::new();
				probabilities.iter().for_each(|(&pos, &probability)| {
					if probability < f64::EPSILON {
						all_zero.push(pos);
					} else if (probability - 1.0).abs() < f64::EPSILON {
						all_one.push(pos);
					}
				});*/
				let (all_zero, all_one) = probabilities
					.par_iter()
					.fold(
						|| (Vec::new(), Vec::new()),
						|(mut all_zero, mut all_one), (&pos, &probability)| {
							if probability < f64::EPSILON {
								all_zero.push(pos);
							} else if (probability - 1.0).abs() < f64::EPSILON {
								all_one.push(pos);
							}
							(all_zero, all_one)
						},
					)
					.reduce(
						|| (Vec::new(), Vec::new()),
						|(mut all_zero, mut all_one), (f_all_zero, f_all_one)| {
							all_zero.par_extend(f_all_zero);
							all_one.par_extend(f_all_one);
							(all_zero, all_one)
						},
					);

				if all_zero.is_empty() && all_one.is_empty() {
					let (choosen, probability) = probabilities
						.into_par_iter()
						.min_by(|(_pos1, probability1), (_pos2, probability2)| {
							probability1.partial_cmp(probability2).unwrap()
						})
						.unwrap();
					if instant {
						open_square(choosen, &coords, half_tile);
					} else {
						to_open.insert(choosen);
					}
					to_update.insert(choosen);

					debug!(
						"Opening square with minimum probability of being mine - {:?}: {}.",
						choosen, probability
					);
				} else {
					if instant {
						all_zero.iter().for_each(|pos| {
							open_square(*pos, &coords, half_tile);
						});
					} else {
						to_open.par_extend(&all_zero);
					}

					to_update.par_extend(all_zero);

					mines_left -= all_one.len();

					if !no_flags {
						if instant {
							all_one.iter().for_each(|pos| {
								flag_square(*pos, &coords, half_tile);
							});
						} else {
							to_flag.par_extend(&all_one);
						}
					}

					flagged.par_extend(&all_one);
					solved.par_extend(all_one);

					debug!("Opening squares with 0% and flagging with 100% probability.");
				}
			} else if to_update.is_empty() {
				let mut all_unexplored = Vec::new();
				all_unexplored.extend(
					iproduct!(0..x_size, 0..y_size)
						.filter(|p| grid[*p] == Square::Unexplored && !solved.contains(p)),
				);
				if all_unexplored.len() == mines_left {
					debug!("Only mines left - flagging remaining squares...");
					mines_left -= all_unexplored.len();

					if !no_flags {
						if instant {
							all_unexplored.iter().for_each(|pos| {
								flag_square(*pos, &coords, half_tile);
							});
						} else {
							to_flag.par_extend(&all_unexplored);
						}
					}

					flagged.par_extend(&all_unexplored);
					solved.par_extend(all_unexplored);
				} else if !all_unexplored.is_empty() {
					debug!("Mission impossible - opening random square.");
					let choosen = *all_unexplored.choose(&mut rng).unwrap();

					if instant {
						open_square(choosen, &coords, half_tile);
					} else {
						to_open.insert(choosen);
					}

					to_update.insert(choosen);
				}
			}
		}

		let click_all = |positions: FxHashSet<Pos>, button| {
			positions.into_iter().for_each(|pos| {
				let c = coords[&pos];
				let destination = Point::new(c.x + half_tile, c.y + half_tile);

				move_to(destination).unwrap_or_else(|_| panic!("Can't move cursor to {:?}", destination));
				click(button, Some(0));
			});
		};

		if !instant {
			if smooth {
				let (mut loc_x, mut loc_y) = {
					let loc = location();
					(loc.x as usize, loc.y as usize)
				};

				let mut positions: FxHashSet<(Pos, bool)> = {
					if no_flags {
						to_open.into_iter().map(|p| (p, true)).collect()
					} else {
						to_open
							.into_iter()
							.map(|p| (p, true))
							.chain(to_flag.into_iter().map(|p| (p, false)))
							.collect()
					}
				};

				while let Some((&pos, c, &left)) = positions
					.iter()
					.map(|(pos, left)| (pos, coords[&pos], left))
					.min_by_key(|(_, pos, _)| {
						let (x, y) = (pos.x as usize, pos.y as usize);
						let dx = loc_x.checked_sub(x).unwrap_or_else(|| x - loc_x);
						let dy = loc_y.checked_sub(y).unwrap_or_else(|| y - loc_y);
						dx * dx + dy * dy
					}) {
					positions.remove(&(pos, left));

					let destination = Point::new(c.x + half_tile, c.y + half_tile);

					smooth_move2(destination, smooth_speed)
						.unwrap_or_else(|_| panic!("Can't move cursor to {:?}", destination));
					click(if left { Button::Left } else { Button::Right }, Some(0));

					loc_x = destination.x as usize;
					loc_y = destination.y as usize;
				}
			} else {
				click_all(to_open, Button::Left);
				if !no_flags {
					click_all(to_flag, Button::Right);
				}
			}
		}

		if success {
			debug!("Game finished: win.");
			break;
		}
		trace!("Loop success.");
	}
}

// use image::Rgba;
use indexmap::IndexSet;

type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;

#[allow(clippy::too_many_arguments)]
fn update(
	to_update: &FxHashSet<Pos>,
	to_solve: &mut FxHashSet<Pos>,
	grid: &mut Array2<Square>,
	coords: &FxHashMap<Pos, Point>,
	solved: &FxHashSet<Pos>,
	tiles: &[(Square, Bitmap)],
	tile_size: Size,
	// colors: &[(Square, Rgba<u8>)],
	size: Pos,
	cooldown: u64,
) -> bool {
	let mut checked = FxHashSet::default();
	// let mut to_retry = to_update.into_par_iter().copied().collect::<FxIndexSet<Pos>>();

	// while !to_retry.is_empty() {
	// let mut positions = to_retry.drain(..).into_par_iter().collect::<FxIndexSet<Pos>>();
	let mut positions = to_update.into_par_iter().copied().collect::<FxIndexSet<Pos>>();

	sleep(Duration::from_millis(cooldown));
	let mut screen = capture_screen().unwrap();

	while let Some(pos) = positions.pop() {
		checked.insert(pos);

		if grid[pos] == Square::Unexplored && !solved.contains(&pos) {
			// let mut retry = true;

			let tile = screen.cropped(Rect::new(coords[&pos], tile_size)).unwrap();
			// let coord = coords[&pos];
			// let color = screen.get_pixel(Point::new(coord.x + 9.0, coord.y + 7.0));

			// for (n, c) in colors {
			for (n, t) in tiles {
				// if color == *c {
				if tile.bitmap_eq(t, None) {
					grid[pos] = *n;
					match n {
						Square::Empty => positions.par_extend(
							neighbors(pos, size)
								.into_par_iter()
								.filter(|npos| !checked.contains(npos)),
						),
						Square::Mine => return true,
						_ => {
							to_solve.insert(pos);
						}
					}

					// retry = false;
					// break;
				}
			}

			/*if retry {
				to_retry.insert(pos);
			}*/
		}
		// }
	}

	false
}

#[inline]
fn neighbors(pos: Pos, size: Pos) -> HashSet<Pos> {
	let mut result = HashSet::with_capacity(8);

	let (x, y) = pos;
	let (x_size, y_size) = size;

	let x1 = x.checked_add(1).filter(|r| r < &x_size);
	let x_1 = x.checked_sub(1);
	let y1 = y.checked_add(1).filter(|r| r < &y_size);
	let y_1 = y.checked_sub(1);

	if let Some(x1) = x1 {
		result.insert((x1, y));
		if let Some(y1) = y1 {
			result.insert((x1, y1));
		}
		if let Some(y_1) = y_1 {
			result.insert((x1, y_1));
		}
	}
	if let Some(x_1) = x_1 {
		result.insert((x_1, y));
		if let Some(y1) = y1 {
			result.insert((x_1, y1));
		}
		if let Some(y_1) = y_1 {
			result.insert((x_1, y_1));
		}
	}
	if let Some(y1) = y1 {
		result.insert((x, y1));
	}
	if let Some(y_1) = y_1 {
		result.insert((x, y_1));
	}

	result
}

#[inline]
fn open_square(pos: Pos, coords: &FxHashMap<Pos, Point>, half_tile: f64) {
	click_square(pos, Button::Left, coords, half_tile)
}

#[inline]
fn flag_square(pos: Pos, coords: &FxHashMap<Pos, Point>, half_tile: f64) {
	click_square(pos, Button::Right, coords, half_tile)
}

#[inline]
fn click_square(pos: Pos, button: Button, coords: &FxHashMap<Pos, Point>, half_tile: f64) {
	let c = coords[&pos];
	move_to(Point::new(c.x + half_tile, c.y + half_tile))
		.unwrap_or_else(|_| panic!("Can't move cursor to {:?}", c));
	click(button, Some(0));
}

use autopilot::{
	mouse::{location, MouseError},
	screen,
};
use std::time::Instant;

// moves mouse cursor to destination with constant speed (in pixels per second) [default: 1000]
pub fn smooth_move2(destination: Point, speed: Option<f64>) -> Result<(), MouseError> {
	if !screen::is_point_visible(destination) {
		return Err(MouseError::OutOfBounds);
	}

	let start_position = location();

	let dx = destination.x - start_position.x;
	let dy = destination.y - start_position.y;
	let distance = dx * dx + dy * dy;
	let mult = speed.map_or(1.0, |speed| speed / 1000.0) / distance.sqrt();

	// direction vector multiplied on speed
	let vec_x = dx * mult;
	let vec_y = dy * mult;

	let time = Instant::now();
	loop {
		let delta = time.elapsed().as_millis() as f64;

		let position = Point::new(start_position.x + vec_x * delta, start_position.y + vec_y * delta);
		// checks if cursor is close enough to destination, so we can just move it in
		if (position.x - start_position.x).powi(2) + (position.y - start_position.y).powi(2) >= distance {
			move_to(destination)?;
			break;
		}
		move_to(position)?;
	}

	Ok(())
}
