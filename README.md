# MinesBot
Solves minesweeper in 3 steps:
1. Solves obvious squares;
1. Calculates probabilities and solves squares with 100% probability of being mine or being empty;
1. If there aren't any 100%-squares, opens square with minimum probability of being mine.

## Usage
Launch with `-h, --help` to get list of arguments.

Can work in any minesweeper game, you just need to:
- Collect bitmaps of tiles (see "tiles" folder for examples) to subfolder inside "tiles", and then run bot with `-t, --tiles <folder_name>`
- If playing custom game, set total number of mines with `-m, --mines <number>`

In addition:
- If bot updates grid faster than needed, you can set delay between updates with `-c, --cooldown <milliseconds>`
- You can activate no flags mode with `-f, --no-flags`
- Smooth mouse movement can be enabled via `-s, --smooth <speed>`, where speed is in *pixels per second*

## Tile sets
All tile sets must be located in `root/tiles/`

Pre-gathered tile sets:
- `default`: "Minesweeper X" or "Minesweeper Clone";
- `online`: minesweeperonline.com.
