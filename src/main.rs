#[macro_use]
extern crate itertools;
#[macro_use]
extern crate clap;
#[macro_use]
extern crate num_derive;
#[macro_use]
extern crate log;

use autopilot::{
	bitmap::{capture_screen, capture_screen_portion, Bitmap},
	geometry::{Point, Rect, Size},
	mouse::{click, move_to, Button},
};
use image::open;
use itertools::Itertools;
use ndarray::prelude::*;
use num_traits::cast::{FromPrimitive, ToPrimitive};
use rand::prelude::*;
use rayon::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
use std::{cmp::min, collections::HashSet, hash::BuildHasherDefault, thread::sleep, time::Duration};

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

	let cooldown = matches
		.value_of("cooldown")
		.map_or(0, |val| val.parse::<u64>().unwrap());
	let no_flags = matches.is_present("no_flags");
	let tiles_dir = format!("tiles/{}", matches.value_of("tiles").unwrap_or("default"));

	// Pre-init
	let mut rng = thread_rng();
	let unexplored_tile = Bitmap::new(open(format!("{}/9.png", tiles_dir)).unwrap(), None);

	let tiles = (0..=8)
		.map(|n| Bitmap::new(open(format!("{}/{}.png", tiles_dir, n)).unwrap(), None))
		.collect::<Vec<Bitmap>>();

	/*let point = Point::new(9.0, 7.0);
	let colors = (0..=8)
		.map(|n| Bitmap::new(open(format!("{}/{}.png", tiles_dir, n)).unwrap(), None).get_pixel(point))
		.collect::<Vec<Rgba<u8>>>();*/

	let tile_size = unexplored_tile.size;
	let full_tile = tile_size.width;
	let half_tile = full_tile / 2.0;
	let smiley_win = Bitmap::new(open(format!("{}/smiley_win.png", tiles_dir)).unwrap(), None);
	let smiley_lose = Bitmap::new(open(format!("{}/smiley_lose.png", tiles_dir)).unwrap(), None);

	// Init
	debug!("Capturing screen...");
	let screen = capture_screen().unwrap();
	debug!("Searching for unexplored tiles...");
	let all_unexplored = screen.find_every_bitmap(&unexplored_tile, None, None, None);
	if all_unexplored.is_empty() {
		panic!("Can't find game on screen");
	}
	let smiley_location = Rect::new(
		screen
			.find_bitmap(
				&Bitmap::new(open(format!("{}/smiley.png", tiles_dir)).unwrap(), None),
				None,
				None,
				None,
			)
			.expect("Can't find smiley on screen"),
		smiley_win.size,
	);

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

	/*
	let mut coords: HashMap<Pos, Point> = HashMap::new();
	grouped.iter().enumerate().for_each(|(x, (_k, g))| {
		g.iter().enumerate().for_each(|(y, &p)| {
			coords.insert((x, y), p);
		});
	});
	*/
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
	let mut flagged = FxHashSet::default();

	let mut to_update = FxHashSet::default();
	let mut to_update_reserve = FxHashSet::default();
	to_update.insert(start);

	let mut to_solve = FxHashSet::default();

	// Main loop
	loop {
		// End game check
		let smiley = capture_screen_portion(smiley_location).unwrap();
		if smiley.bitmap_eq(&smiley_win, None) || smiley.bitmap_eq(&smiley_lose, None) {
			debug!("Game finished.");
			break;
		}

		// Updating grid
		trace!("Updating grid...");
		if !to_update.is_empty() {
			sleep(Duration::from_millis(cooldown));
			update(
				&mut to_update,
				&mut to_solve,
				&mut grid,
				&coords,
				&solved,
				&tiles,
				// &colors,
				tile_size,
				size,
			);
			to_update_reserve = to_update.clone();
			to_update.clear();
		} else if !to_update_reserve.is_empty() {
			sleep(Duration::from_millis(cooldown));
			update(
				&mut to_update_reserve,
				&mut to_solve,
				&mut grid,
				&coords,
				&solved,
				&tiles,
				// &colors,
				tile_size,
				size,
			);
		}

		// Solving state
		trace!("Solving state...");
		let mut to_open = FxHashSet::default();
		let mut to_flag = FxHashSet::default();

		// Simple solver
		to_solve = to_solve
			.iter()
			.copied()
			.filter(|&pos| {
				let mut flagged_mines = 0;
				let mut unexplored = Vec::with_capacity(8);
				neighbors(pos, size).into_iter().for_each(|npos| {
					if flagged.contains(&npos) {
						flagged_mines += 1;
					} else if grid[npos] == Square::Unexplored {
						unexplored.push(npos);
					}
				});

				if !unexplored.is_empty() {
					let mines_left_around = grid[pos] - flagged_mines;
					if mines_left_around == 0 {
						unexplored.into_iter().for_each(|p| {
							to_open.insert(p);
							to_update.insert(p);
						});
						solved.insert(pos);
						return false;
					} else if mines_left_around == unexplored.len() as u8 {
						unexplored.into_iter().for_each(|p| {
							to_flag.insert(p);
							flagged.insert(p);
							solved.insert(p);
							mines_left -= 1;
						});
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
			debug!("All mines founded - opening remaining squares...");
			iproduct!(0..x_size, 0..y_size).for_each(|p| {
				if grid[p] == Square::Unexplored && !solved.contains(&p) {
					to_open.insert(p);
				}
			});
		// Complex solver
		} else if to_open.is_empty() && to_flag.is_empty() {
			trace!("Calculating probabilities...");

			let mut probabilities = FxHashMap::default();

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
					HashSet::with_capacity_and_hasher(8, BuildHasherDefault::<FxHasher>::default());
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

					while !seed_set.is_empty() {
						seed_set.clone().into_iter().for_each(|q| {
							if !in_sector.contains(&q.pos) {
								let q_neighbors = get_neighbors(q.pos).1;

								if !q_neighbors.is_empty() {
									q_neighbors.iter().for_each(|n| {
										seed_set.insert(n.clone());
									});
								}

								sectors.get_mut(s).unwrap().push(q.clone());
								in_sector.insert(q.pos);
							}
							seed_set.remove(&q);
						});
					}
					s += 1;
				}
			});

			// creating all possible solutions for each sector
			trace!("creating all possible solutions for each sector");
			sectors.iter().for_each(|s| {
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
					solutions.iter().for_each(|sol| {
						sol.iter().for_each(|&pos| {
							*weights.entry(*pos).or_insert(0) += 1;
						});
					});
					let solutions_amount = solutions.len() as f64;
					weights.into_iter().for_each(|(p, w)| {
						probabilities.insert(p, (w as f64) / solutions_amount);
					});
				}
			});

			if !probabilities.is_empty() {
				let mut all_zero = Vec::new();
				let mut all_one = Vec::new();
				probabilities.iter().for_each(|(&pos, &probability)| {
					if probability < f64::EPSILON {
						all_zero.push(pos);
					} else if (probability - 1.0).abs() < f64::EPSILON {
						all_one.push(pos);
					}
				});
				if all_zero.is_empty() && all_one.is_empty() {
					let (choosen, probability) = probabilities
						.into_par_iter()
						.min_by(|(_pos1, probability1), (_pos2, probability2)| {
							probability1.partial_cmp(probability2).unwrap()
						})
						.unwrap();
					to_open.insert(choosen);
					to_update.insert(choosen);
					debug!(
						"Opening square with minimum probability of being mine - {:?}: {}.",
						choosen, probability
					);
				} else {
					all_zero.into_iter().for_each(|pos| {
						to_open.insert(pos);
						to_update.insert(pos);
					});
					all_one.into_iter().for_each(|pos| {
						to_flag.insert(pos);
						flagged.insert(pos);
						solved.insert(pos);
						mines_left -= 1;
					});
					debug!("Opening squares with 0% and flagging with 100% probability.");
				}
			} else if to_open.is_empty() && to_flag.is_empty() && to_update.is_empty() {
				let mut all_unexplored = Vec::new();
				iproduct!(0..x_size, 0..y_size).for_each(|p| {
					if grid[p] == Square::Unexplored && !solved.contains(&p) {
						all_unexplored.push(p);
					}
				});
				if all_unexplored.len() == mines_left {
					debug!("Only mines left - flagging remaining squares...");
					all_unexplored.into_iter().for_each(|p| {
						to_flag.insert(p);
						flagged.insert(p);
						solved.insert(p);
						mines_left -= 1;
					});
				} else if !all_unexplored.is_empty() {
					debug!("Mission impossible - opening random square.");
					let choosen = *all_unexplored.choose(&mut rng).unwrap();
					to_open.insert(choosen);
					to_update.insert(choosen);
				}
			}
		}

		let click_all = |positions: FxHashSet<Pos>, button| {
			positions.into_iter().for_each(|pos| {
				let c = coords[&pos];

				let destination = Point::new(c.x + half_tile, c.y + half_tile);
				if smooth {
					smooth_move2(destination, smooth_speed)
				} else {
					move_to(destination)
				}
				.unwrap_or_else(|_| panic!("Can't move cursor to {:?}", destination));

				click(button, Some(0));
			});
		};

		click_all(to_open, Button::Left);
		if !no_flags {
			click_all(to_flag, Button::Right);
		}
		trace!("Loop success.");
	}
}

#[allow(clippy::too_many_arguments)]
fn update(
	positions: &mut FxHashSet<Pos>,
	to_solve: &mut FxHashSet<Pos>,
	grid: &mut Array2<Square>,
	coords: &FxHashMap<Pos, Point>,
	solved: &FxHashSet<Pos>,
	tiles: &[Bitmap],
	// colors: &[Rgba<u8>],
	tile_size: Size,
	size: Pos,
) {
	while !positions.is_empty() {
		positions.clone().into_iter().for_each(|pos| {
			if grid[pos] == Square::Unexplored && !solved.contains(&pos) {
				let tile = capture_screen_portion(Rect::new(coords[&pos], tile_size)).unwrap();
				for (n, t) in tiles.iter().enumerate() {
					// for (n, c) in colors.iter().enumerate() {
					if tile.bitmap_eq(t, None) {
						// if tile.get_pixel(Point::new(9.0, 7.0)) == *c {
						grid[pos] = Square::from_usize(n).unwrap();
						if n == 0 {
							neighbors(pos, size).into_iter().for_each(|np| {
								positions.insert(np);
							});
						} else {
							to_solve.insert(pos);
						}
						break;
					}
				}
			}
			positions.remove(&pos);
		});
	}
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

/*
#[inline]
fn open_square(pos: Pos, coords: &HashMap<Pos, Point>, half_tile: f64) {
	let c = coords[&pos];
	move_to(Point::new(c.x + half_tile, c.y + half_tile))
		.unwrap_or_else(|_| panic!("Can't move cursor to {:?}", c));
	click(Button::Left, Some(0));
}

#[inline]
fn flag_square(pos: Pos, coords: &HashMap<Pos, Point>, half_tile: f64) {
	let c = coords[&pos];
	move_to(Point::new(c.x + half_tile, c.y + half_tile))
		.unwrap_or_else(|_| panic!("Can't move cursor to {:?}", c));
	click(Button::Right, Some(0));
}
*/

use autopilot::{
	mouse::{location, MouseError},
	screen,
};
use std::time::Instant;

pub fn smooth_move2(destination: Point, speed: Option<f64>) -> Result<(), MouseError> {
	if !screen::is_point_visible(destination) {
		return Err(MouseError::OutOfBounds);
	}

	let start_position = location();

	let dx = destination.x - start_position.x;
	let dy = destination.y - start_position.y;
	let distance = dx * dx + dy * dy;
	let mult = speed.map_or(1.0, |speed| speed / 1000.0) / distance.sqrt();

	let vec_x = dx * mult;
	let vec_y = dy * mult;

	let time = Instant::now();
	loop {
		let delta = time.elapsed().as_millis() as f64;

		let position = Point::new(start_position.x + vec_x * delta, start_position.y + vec_y * delta);
		if (position.x - start_position.x).powi(2) + (position.y - start_position.y).powi(2) >= distance {
			move_to(destination)?;
			break;
		}
		move_to(position)?;
	}

	Ok(())
}
