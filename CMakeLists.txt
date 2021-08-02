cmake_minimum_required (VERSION 3.14...3.21)

project(TorontoFortranGames
LANGUAGES C Fortran)

enable_testing()

include(cmake/compilers.cmake)
include(cmake/curses.cmake)

#--------------
add_executable(tictac tictac.f)
set(tin ${CMAKE_CURRENT_BINARY_DIR}/tictac.stdin)
if(NOT EXISTS ${tin})
  file(WRITE ${tin} "233")
endif()
add_test(NAME TicTacToe COMMAND ${CMAKE_COMMAND} -Dexe:FILEPATH=$<TARGET_FILE:tictac> -Dinput_file:FILEPATH=${tin} -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/test_tictac.cmake)

add_executable(chess chess.f)

if(Curses_FOUND)
  add_executable(fireworks fireworks.c)
  target_link_libraries(fireworks ${CURSES_LIBRARIES})
  target_include_directories(fireworks PRIVATE ${CURSES_INCLUDE_DIRS})
  install(TARGETS fireworks)
endif()

install(TARGETS tictac chess)
