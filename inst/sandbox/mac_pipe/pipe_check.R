# http://stackoverflow.com/questions/17766391/how-to-get-output-from-a-pipe-connection-before-closing-it-in-r/17767169#17767169

could_not_fix_simple_pipe <- function() {
  pip <- pipe('python -i R/sand.py 2>&1', 'r')
  pid <- as.numeric(readLines(pip, 1))
  # capture.output(tools::pskill(pid), file='/dev/null')
  system(paste('kill -9', pid, '2>&1 1>/dev/null'), ignore.stderr = T, ignore.stdout = T)
  closeAllConnections()
}

read_write_pipe_case <- function() {
  stopifnot(capabilities("fifo"))
  system('mkfifo /tmp/Rpython.fifo')
  output <- fifo('/tmp/Rpython.fifo', 'r')
  readLines(output)  # if fifo file was not empty
  input  <- pipe('python -i > /tmp/Rpython.fifo 2>&1', 'w')
  
  while (!length(nchar(x <- readLines(output)))) {
    Sys.sleep(1)
    cat('.')
  }
  cat(x)
  
  for (i in 1:2) {
    python_code <- paste("import time",
                         "time.sleep(3)",
                         sprintf("print %s\n", i*100), sep='\n')
    print(python_code)
    cat(python_code, file = input)
    flush(input)
    
    while (!length(nchar(x <- readLines(output)))) {
      Sys.sleep(0.1)
      cat('.')
    }
    print(x)
  }
  
  close(input)
  close(output)
}

fifo_file_name <- '/tmp/Rpython.fifo'
output <- fifo(fifo_file_name, 'r')
pip <- pipe(sprintf('python -i R/sand.py > %s 2>&1', fifo_file_name),  'w')
pid <- readLines(output, 1)
closeAllConnections()
