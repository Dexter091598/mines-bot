extern crate autopilot;
extern crate image;
#[macro_use]
extern crate itertools;
#[macro_use]
extern crate clap;
#[macro_use]
extern crate num_derive;

use autopilot::{
	bitmap::{capture_screen, capture_screen_portion, Bitmap},
	geometry::{Point, Rect, Size},
	mouse::{click, move_to, Button},
};
use image::open;
use itertools::Itertools;
use ndarray::prelude::*;
use num_traits::cast::{FromPrimitive, ToPrimitive};
use rand::prelude::{thread_rng, IteratorRandom};
use rayon::prelude::*;
use std::{
	cmp::min,
	collections::{HashMap, HashSet},
	thread::sleep,
	time::Duration,
};

type Pos = (usize, usize);

#[derive(Eq, Clone)]
struct Detector {
	pos: Pos,
	mines_left: usize,
	unexplored: HashSet<Pos>,
}
impl Detector {
	fn new(pos: Pos, mines_left: usize, unexplored: HashSet<Pos>) -> Self {
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

fn main() {
	let matches = clap_app!((
		crate_name!()
			.split("-")
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
		(@arg no_flags: -f long("no-flags") "Don't lose time on flagging mines.")
		(@arg tiles: -t --tiles
			+takes_value
			default_value("default")
			value_name("folder_name")
			"Sets custom tiles directory."
		)
	)
	.get_matches();

	let cooldown: u64 = match matches.value_of("cooldown") {
		Some(val) => val.parse().unwrap(),
		None => 0,
	};
	let no_flags = matches.is_present("no_flags");
	let tiles_dir = format!(
		"tiles/{}",
		(match matches.value_of("tiles") {
			Some(val) => val.parse().unwrap(),
			None => "default".to_string(),
		})
	);

	// Pre-init
	let mut rng = thread_rng();
	let unexplored_tile = Bitmap::new(open(format!("{}/9.png", tiles_dir)).unwrap(), None);
	let tiles = (0..=8)
		.map(|n| Bitmap::new(open(format!("{}/{}.png", tiles_dir, n)).unwrap(), None))
		.collect::<Vec<Bitmap>>();
	let tile_size = unexplored_tile.size;
	let full_tile = tile_size.width;
	let half_tile = full_tile / 2.0;
	let smiley_win = Bitmap::new(open(format!("{}/smiley_win.png", tiles_dir)).unwrap(), None);
	let smiley_lose = Bitmap::new(open(format!("{}/smiley_lose.png", tiles_dir)).unwrap(), None);

	// Init
	println!("Capturing screen...");
	let screen = capture_screen().unwrap();
	println!("Searching for unexplored tiles...");
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

	println!("Grouping...");
	let grouped = &all_unexplored
		.into_iter()
		.group_by(|p| p.x)
		.into_iter()
		.map(|g| (g.0, g.1.collect::<Vec<_>>()))
		.collect::<Vec<_>>();

	println!("Creating coordinates grid...");
	let x_size = grouped.len();
	let y_size = grouped[0].1.len();
	let shape = (x_size, y_size);

	let total_mines = match matches.value_of("mines") {
		Some(val) => val.parse().unwrap(),
		None => {
			match shape {
				(8, 8) => 10,
				(9, 9) => 10,
				(16, 16) => 40,
				(30, 16) => 99,
				_ => panic!("Can't detect number of mines automaticly for shape {:?}. Use (-m, --mines) parameter to specify it.", shape),
			}
		},
	};
	let mut mines_left = total_mines;

	let mut grid = Array2::<Square>::from_elem(shape, Square::Unexplored);

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
		.collect::<HashMap<Pos, Point>>();

	println!("--- Initial State Success ---");
	let start = (x_size / 2, y_size / 2);

	let c = coords[&start];
	move_to(Point::new(c.x + half_tile, c.y + half_tile))
		.unwrap_or_else(|_| panic!("Can't move cursor to {:?}", c));
	click(Button::Left, Some(0));

	let mut solved = HashSet::new();
	let mut flagged = HashSet::new();

	let mut to_update = HashSet::new();
	let mut to_update_reserve = HashSet::new();
	to_update.insert(start);

	let mut to_solve = HashSet::new();

	// Main loop
	loop {
		// End game check
		let smiley = capture_screen_portion(smiley_location).unwrap();
		if smiley.bitmap_eq(&smiley_win, None) || smiley.bitmap_eq(&smiley_lose, None) {
			println!("Game finished.");
			break;
		}

		// Updating grid
		println!("Updating grid...");
		if !to_update.is_empty() {
			sleep(Duration::from_millis(cooldown));
			update(
				&mut to_update,
				&mut to_solve,
				&mut grid,
				&coords,
				&solved,
				&tiles,
				tile_size,
				x_size,
				y_size,
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
				tile_size,
				x_size,
				y_size,
			);
		}

		// Solving state
		println!("Solving state...");
		let mut to_open = HashSet::new();
		let mut to_flag = HashSet::new();

		// Simple solver
		to_solve = to_solve
			.iter()
			.copied()
			.filter(|&pos| {
				let mut flagged_mines = 0;
				let mut unexplored = Vec::with_capacity(8);
				neighbors(pos, x_size, y_size).into_iter().for_each(|npos| {
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
			println!("All mines founded - opening remaining squares...");
			iproduct!(0..x_size, 0..y_size).for_each(|p| {
				if grid[p] == Square::Unexplored && !solved.contains(&p) {
					to_open.insert(p);
				}
			});
		// Complex solver
		} else if to_open.is_empty() && to_flag.is_empty() {
			println!("Calculating probabilities...");

			let mut probabilities: HashMap<Pos, f64> = HashMap::new();
			// separating to sectors
			let mut sectors: Vec<HashSet<Detector>> = Vec::new();
			to_solve = to_solve
				.iter()
				.copied()
				.filter(|&pos| {
					let mut flagged_mines = 0;
					let mut unexplored = HashSet::with_capacity(8);
					neighbors(pos, x_size, y_size).into_iter().for_each(|npos| {
						if flagged.contains(&npos) {
							flagged_mines += 1;
						} else if grid[npos] == Square::Unexplored {
							unexplored.insert(npos);
						}
					});

					if !unexplored.is_empty() {
						let detector =
							Detector::new(pos, (grid[pos] - flagged_mines) as usize, unexplored.clone());
						for sector in &mut sectors {
							for dt in sector.iter().cloned() {
								if !dt.unexplored.is_disjoint(&unexplored) {
									sector.insert(detector);
									return true;
								}
							}
						}
						let mut new_sector = HashSet::new();
						new_sector.insert(detector);
						sectors.push(new_sector);
						return true;
					}
					false
				})
				.collect();
			// union some sectors
			if sectors.len() >= 2 {
				let mut unioned = sectors
					.clone()
					.iter()
					.filter(|s| s.len() >= 2)
					.enumerate()
					.combinations(2)
					.filter_map(|c| {
						let c1 = c[0];
						let c2 = c[1];
						let s1 = c1.1;
						let s2 = c2.1;
						if !s1.is_disjoint(&s2) {
							sectors.remove(c1.0);
							sectors.remove(c2.0);
							return Some(s1.union(&s2).cloned().collect());
						}
						None
					})
					.collect::<Vec<HashSet<Detector>>>();
				if !unioned.is_empty() {
					sectors.append(&mut unioned);
				}
			}
			// creating all possible solutions for each sector
			sectors.iter().for_each(|s| {
				let s_unexplored = s
					.par_iter()
					.flat_map(|dt| dt.unexplored.clone())
					.collect::<HashSet<Pos>>();
				let solutions = (1..=min(s_unexplored.len(), mines_left))
					.into_par_iter()
					.flat_map(|n| {
						s_unexplored
							.iter()
							.combinations(n)
							.filter(|sol| {
								s.par_iter().all(|dt| {
									dt.unexplored
										.intersection(
											&sol.par_iter().copied().copied().collect::<HashSet<Pos>>(),
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
						.collect::<HashMap<Pos, usize>>();
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
					if probability.eq(&0.0) {
						all_zero.push(pos);
					} else if probability.eq(&1.0) {
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
					println!(
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
					println!("Opening squares with 0% and flagging with 100% probability.");
				}
			} else if to_open.is_empty() && to_flag.is_empty() && to_update.is_empty() {
				let mut all_unexplored = Vec::new();
				iproduct!(0..x_size, 0..y_size).for_each(|p| {
					if grid[p] == Square::Unexplored && !solved.contains(&p) {
						all_unexplored.push(p);
					}
				});
				if all_unexplored.len() == mines_left {
					println!("Only mines left - flagging remaining squares...");
					all_unexplored.into_iter().for_each(|p| {
						to_flag.insert(p);
						flagged.insert(p);
						solved.insert(p);
						mines_left -= 1;
					});
				} else {
					println!("Mission impossible - opening random square.");
					let choosen = all_unexplored.into_iter().choose(&mut rng).unwrap();
					to_open.insert(choosen);
					to_update.insert(choosen);
				}
			}
		}

		to_open.iter().for_each(|pos| {
			let c = coords[&pos];
			move_to(Point::new(c.x + half_tile, c.y + half_tile))
				.unwrap_or_else(|_| panic!("Can't move cursor to {:?}", c));
			click(Button::Left, Some(0));
		});
		if !no_flags {
			to_flag.iter().for_each(|pos| {
				let c = coords[&pos];
				move_to(Point::new(c.x + half_tile, c.y + half_tile))
					.unwrap_or_else(|_| panic!("Can't move cursor to {:?}", c));
				click(Button::Right, Some(0));
			});
		}
		println!("Loop success.");
	}
}

fn update(
	positions: &mut HashSet<Pos>,
	to_solve: &mut HashSet<Pos>,
	grid: &mut Array2<Square>,
	coords: &HashMap<Pos, Point>,
	solved: &HashSet<Pos>,
	tiles: &[Bitmap],
	tile_size: Size,
	x_size: usize,
	y_size: usize,
) {
	while !positions.is_empty() {
		positions.clone().into_iter().for_each(|pos| {
			if grid[pos] == Square::Unexplored && !solved.contains(&pos) {
				let tile = capture_screen_portion(Rect::new(coords[&pos], tile_size)).unwrap();
				for (n, t) in tiles.iter().enumerate() {
					if tile.bitmap_eq(t, None) {
						grid[pos] = Square::from_usize(n).unwrap();
						if n == 0 {
							neighbors(pos, x_size, y_size).into_iter().for_each(|np| {
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
fn neighbors(pos: Pos, x_size: usize, y_size: usize) -> HashSet<Pos> {
	let mut result = HashSet::with_capacity(8);

	let x = pos.0;
	let y = pos.1;

	let x1 = if let Some(r) = x.checked_add(1) {
		if r < x_size {
			Some(r)
		} else {
			None
		}
	} else {
		None
	};
	let x_1 = x.checked_sub(1);
	let y1 = if let Some(r) = y.checked_add(1) {
		if r < y_size {
			Some(r)
		} else {
			None
		}
	} else {
		None
	};
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
