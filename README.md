# Minesweeper

This is a console version of Minesweeper written in Haskell.  

## Install
```
git clone git@github.com:DKurilo/minesweeper.git
cd ./minesweeper
stack build
stack install
```

## Run

```
> minesweeper --help
Minesweeper
Default options: --width 10 --height 10 --mines 10

Usage: minesweeper [--width INT] [--height INT] [--mines INT]

Available options:
  -h,--help                Show this help text
  --width INT              Board width
  --height INT             Board height
  --mines INT              Mines amount
```

Or just  

```
minesweeper --width 30 --height 16 --mines 100
```

## Controls  

`h`, `j`, `k`, `l` or arrows to move the cursor  
`m` to mark/unmark a cell as mined  
`Space` to open a cell  
`?`, `/` to mark/unmark a cell as suspicious  
`Enter` or `d` to open cells around
