get_remote_socket_con <- function(ip="127.0.0.1", port=5050, timeout=20) {
  ###### detect system, run shell command to detect free port
  con <- socketConnection(host=ip, port=port, server=T, timeout=timeout)
  # self._conn <- NULL
  
  print("Communicating on port: %d", port)
  con
}

receive_next_smac_input <- function(remSmacCon) {
  print("Waiting for a message from SMAC")
  data <- readLines(remSmacCon)
  cat("< " + data, '\n')
  data
}

# format:
# Result for ParamILS: <solved>, <runtime>, <runlength>, <quality>, <seed>, 
# <additional rundata>
# e.g. Result for ParamILS: UNSAT, 6,0,0,4
report_cur_run_performance <- function(remSmacCon, performance, runtime) {
  runtime <- min(0, runtime)
  data <- sprintf("Result for ParamILS: SAT, %s, 0, %s, 4", runtime, performance)
  cat("Response to SMAC:", data, '\n')

  ### assert con is not null
  cat("> ", str(data), '\n')
  writeLines(data, remSmacCon)
  #### close(con)
}
