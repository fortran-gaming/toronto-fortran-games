project('TorontoFortranGames', 'fortran', 'c',
  default_options : ['default_library=static', 'buildtype=release'])

add_project_arguments('-std=legacy', language: 'fortran')

#--------------
tictac_exe = executable('tictac', 'tictac.f')

chess_exe = executable('chess', 'chess.f')

curses = dependency('ncurses', required: false)
if curses.found()
  fireworks_exe = executable('fireworks', 'fireworks.c', dependencies : curses)
endif
