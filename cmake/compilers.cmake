if(CMAKE_BUILD_TYPE STREQUAL Debug)
  add_compile_options(-g -O0)
else()
  add_compile_options(-O3)
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  list(APPEND FFLAGS -Werror=array-bounds -fcheck=all
  -fexceptions -ffpe-trap=invalid,zero,overflow 
  #-fimplicit-none
  -Wline-truncation) #-Warray-temporaries
  
  list(APPEND FLAGS -march=native)
  list(APPEND CFLAGS -Wall -Wextra -Wpedantic)
  
#  link_libraries(-fsanitize=address) Only for Linux, don't bother on CI for now.
  
  if(UNIX AND NOT (APPLE OR CYGWIN))
    add_compile_options(-fstack-protector-all)
  endif()
  
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 8.1)
    list(APPEND FFLAGS -std=legacy)
  endif()
  
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
  list(APPEND FFLAGS -fpe0 -traceback ) #-implicitnone
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Flang)  # https://github.com/flang-compiler/flang/wiki/Fortran-2008
  list(APPEND FFLAGS -Mallocatable=03)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL PGI)
endif()
