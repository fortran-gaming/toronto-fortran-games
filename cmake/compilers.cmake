
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  string(APPEND CMAKE_Fortran_FLAGS " -w -std=legacy -Wline-truncation")
  add_compile_options(-mtune=native)
endif()
