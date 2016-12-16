tester <- setRefClass(
  "tester",
  
  fields = list(
    a = "numeric"),
  
  methods = list(
    initialize=function(...) {
      # .self$glob <- glob
      callSuper(...)
      # addGlobs()
      print('init')
    },
    finalize=function() {
      print('fin')
    },
    addGlobs=function() {
      glob <- 'glob'
    },
    printAddOne = function(x) {
      print(a + 1)
    },
    add = function(x) {
      a <<- a + x
    }))
x <- tester(a=1)

x$a
x$add(4)
x$glob
