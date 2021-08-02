execute_process(COMMAND ${exe} -d 6
  INPUT_FILE ${input_file}
  TIMEOUT 5
  RESULT_VARIABLE ret
  OUTPUT_VARIABLE out)

if(NOT ret EQUAL 0)
  message(FATAL_ERROR "TicTacToe run failed")
endif()

file(READ ${CMAKE_CURRENT_LIST_DIR}/../tests/loss.log ref_loss)
file(READ ${CMAKE_CURRENT_LIST_DIR}/../tests/tictac.log ref_win)

if(out STREQUAL ref_win OR out STREQUAL ref_loss)
  return()
else()
  message(FATAL_ERROR "Test failed
  ${out}")
endif()
