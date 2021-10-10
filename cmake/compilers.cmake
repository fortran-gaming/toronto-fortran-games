
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(-mtune=native
    "$<$<COMPILE_LANGUAGE:Fortran>:-w;-std=legacy;-Wline-truncation>")
endif()
