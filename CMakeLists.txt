cmake_minimum_required (VERSION 3.15...3.29)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "In-source builds are not allowed.
  cmake -Bbuild")
endif()

project(TorontoFortranGames
LANGUAGES C Fortran)

enable_testing()

include(cmake/compilers.cmake)
include(cmake/curses.cmake)

#--------------
add_executable(tictac tictac.f)

set(tin ${CMAKE_CURRENT_BINARY_DIR}/tictac.stdin)
file(GENERATE OUTPUT ${tin} CONTENT "233")

add_test(NAME TicTacToe COMMAND ${CMAKE_COMMAND} -Dexe:FILEPATH=$<TARGET_FILE:tictac> -Dinput_file:FILEPATH=${tin} -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/test_tictac.cmake)

add_executable(chess chess.f)

if(CURSES_FOUND)
  add_executable(fireworks fireworks.c)
  target_link_libraries(fireworks PRIVATE ${CURSES_LIBRARIES})
  target_include_directories(fireworks PRIVATE ${CURSES_INCLUDE_DIRS})
  install(TARGETS fireworks)
endif()

install(TARGETS tictac chess)

file(GENERATE OUTPUT .gitignore CONTENT "*")
