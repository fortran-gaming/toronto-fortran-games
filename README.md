# Toronto Fortran Games

![Actions Status](https://github.com/fortran-gaming/toronto-fortran-games/workflows/ci_meson/badge.svg)
![Actions Status](https://github.com/fortran-gaming/toronto-fortran-games/workflows/ci_cmake/badge.svg)

From <http://freshmeat.sourceforge.net/projects/fortran-games>

Authors include:

    PROGRAMMERS: SID HEIDT,GLENN FADEN
    CONTRIBUTOR: GLENN FADEN
    MINOR CONTRIBUTOR: MICHAEL ELLIS

    THIS PROGRAM WAS FIRST WRITTEN FOR THE 86 IN
    FALL, 1975. IT WAS UPGRADED FOR THE 32 IN FEBRUARY, 1977.
    THE CURRENT VERSION WAS COMPLETED IN FEBRUARY, 1978.

## Build

```sh
cmake -B build

cmake --build build
```

## Play

Tic tac toe: `./tictac`

Chess has a bug where the numbers for rows are reversed if you don't take the first move:

```sh
./chess
```

Demonstration of Ncurses from C: `./fireworks`

## Notes

Sadly, the numerous other games in this package rely entirely on a non-functioning Fortran precursor to Curses, carefully written for 6 different terminal types, before terminal emulation was popular.
One would likely have to port the games not included here to use Ncurses, no small feat.
