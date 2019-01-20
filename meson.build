project('TorontoFortranGames', 'fortran', 'c', default_options : ['default_library=static'])

#--------------
tictac_exe = executable('tictac', 'tictac.f')

chess_exe = executable('chess', 'chess.f')

curses = dependency('ncurses')
if curses.found()
  fireworks_exe = executable('fireworks', 'fireworks.c', dependencies : curses)
endif
