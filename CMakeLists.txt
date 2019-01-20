cmake_minimum_required (VERSION 3.7)
project(TorontoFortranGames Fortran C)
enable_testing()

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  if(UNIX)
    set(CMAKE_INSTALL_PREFIX "~/.local/bin" CACHE PATH "..." FORCE)
  endif()
endif()

include(cmake/compilers.cmake)

#--------------
add_executable(tictac tictac.f)
target_compile_options(tictac PRIVATE ${FLAGS} ${FFLAGS})
if(NOT WIN32)
  add_test(NAME TicTacToe COMMAND bash -c "../tests/test_tictac.sh")
endif()

add_executable(chess chess.f)
target_compile_options(chess PRIVATE ${FLAGS} ${FFLAGS})


find_package(Curses)
if(Curses_FOUND)
  add_executable(fireworks fireworks.c)
  target_compile_options(fireworks PRIVATE ${FLAGS} ${CFLAGS})
  target_link_libraries(fireworks ${CURSES_LIBRARIES})
install(TARGETS fireworks
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})
endif()

install(TARGETS tictac chess
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})

