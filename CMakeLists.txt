cmake_minimum_required (VERSION 3.7)
project(FortranGames Fortran C)
enable_testing()

if(CMAKE_BUILD_TYPE STREQUAL Debug)
  add_compile_options(-g -O0)
else()
  add_compile_options(-O3)
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  list(APPEND FFLAGS -Werror=array-bounds -fbacktrace -fcheck=all
  -fexceptions -ffpe-trap=invalid,zero,overflow 
  -finit-real=nan
  -Wline-truncation) #-Warray-temporaries
  
  list(APPEND FLAGS -march=native)
  list(APPEND CFLAGS -Wall -Wextra -Wpedantic)
  
#  link_libraries(-fsanitize=address) Only for Linux, don't bother on CI for now.
  
  if(NOT CYGWIN AND NOT WIN32)
    add_compile_options(-fstack-protector-all)
  endif()
  

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
  list(APPEND FFLAGS -fpe0 -traceback)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Flang)  # https://github.com/flang-compiler/flang/wiki/Fortran-2008
  list(APPEND FFLAGS -Mallocatable=03 -nomp)
  link_libraries(-static-flang-libs)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL PGI)
  list(APPEND FFLAGS -Mallocatable=03)
endif()

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
endif()
