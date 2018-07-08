.. image:: https://travis-ci.com/scivision/toronto-fortran-games.svg?branch=master
    :target: https://travis-ci.com/scivision/toronto-fortran-games

=====================
Toronto Fortran Games
=====================

From  http://freshmeat.sourceforge.net/projects/fortran-games

Authors include::


    C         PROGRAMMERS: SID HEIDT,GLENN FADEN
    C         CONTRIBUTOR: GLENN FADEN
    C         MINOR CONTRIBUTOR: MICHAEL ELLIS
    C
    C         THIS PROGRAM WAS FIRST WRITTEN FOR THE 86 IN
    C         FALL, 1975. IT WAS UPGRADED FOR THE 32 IN FEBRUARY, 1977.
    C         THE CURRENT VERSION WAS COMPLETED IN FEBRUARY, 1978.

Build
=====
::

    cd bin
    
    cmake ..
    
    cmake --build .
    
Play
====
Run any of the executables created in `bin/` for example::

    ./tictac
  
Chess has a bug where the numbers for rows are reversed if you don't take the first move::

    ./chess
 
and a demonstration of Ncurses from C::

    ./fireworks
  
  
Notes
=====

Sadly, the numerous other games in this package rely entirely on a non-functioning Fortran precursor to Curses, carefully written for 6 different terminal types, before terminal emulation was popular. 
One would likely have to port the games not included here to use Ncurses, no small feat.
