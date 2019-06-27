
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  if(CMAKE_BUILD_TYPE STREQUAL Debug)
    list(APPEND FFLAGS -Werror=array-bounds -fcheck=all
    -fexceptions -ffpe-trap=invalid,zero,overflow
    #-fimplicit-none
    ) #-Warray-temporaries
  endif()

  list(APPEND FLAGS -march=native -Wline-truncation)
  list(APPEND CFLAGS -Wall -Wextra -Wpedantic)

  if(UNIX AND NOT (APPLE OR CYGWIN))
    add_compile_options(-fstack-protector-all)
  endif()

  list(APPEND FFLAGS -std=legacy)

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
  if(CMAKE_BUILD_TYPE STREQUAL Debug)
    list(APPEND FFLAGS -fpe0 -traceback) #-implicitnone
  endif()
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Flang)

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL PGI)

endif()
