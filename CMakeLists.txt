cmake_minimum_required (VERSION 3.7)
project(FortranGames Fortran)
enable_testing()

if(CMAKE_BUILD_TYPE STREQUAL Debug)
  add_compile_options(-g -O0)
else()
  add_compile_options(-O3)
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  add_compile_options(-march=native -Werror=array-bounds -fbacktrace -fcheck=all
  -fexceptions -ffpe-trap=invalid,zero,overflow 
  -finit-real=nan
  -Wline-truncation) #-Warray-temporaries
  
#  link_libraries(-fsanitize=address) Only for Linux, don't bother on CI for now.
  
  if(NOT CYGWIN AND NOT WIN32)
    add_compile_options(-fstack-protector-all)
  endif()
  

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
  add_compile_options(-warn -fpe0 -traceback -debug extended -check all)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Flang)  # https://github.com/flang-compiler/flang/wiki/Fortran-2008
  add_compile_options(-Mallocatable=03 -nomp)
  set(FFLAGS -static-flang-libs)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL PGI)
  set(FFLAGS -Mallocatable=03)
endif()

#--------------

add_executable(tictac tictac.f)
