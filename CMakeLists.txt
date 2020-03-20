cmake_minimum_required (VERSION 3.7)
project(TorontoFortranGames
LANGUAGES C Fortran)
enable_testing()

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/compilers.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/curses.cmake)

#--------------
add_executable(tictac tictac.f)
if(UNIX)
  add_test(NAME TicTacToe COMMAND bash -c "../tests/test_tictac.sh")
endif()

add_executable(chess chess.f)


find_package(Curses)
if(Curses_FOUND)
  add_executable(fireworks fireworks.c)
  target_link_libraries(fireworks ${CURSES_LIBRARIES})
  target_include_directories(fireworks PRIVATE ${CURSES_INCLUDE_DIRS})
  install(TARGETS fireworks
          RUNTIME DESTINATION bin)
endif()

install(TARGETS tictac chess
        RUNTIME DESTINATION bin)

