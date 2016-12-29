# extracts the next set of parameters
# e.g. input: "cvfold-0 0 18000.0 2147483647 4 -x0 '6.98467896812' -x1 '13.431402644'"
parse_smac_param_string <- function (param_string) {
  # param_pieces <- param_string.strip().split()[5:]
  param_pieces <- param_string %>% trimws %>% 
    strsplit(" ") %>% `[[`(1) %>% tail(-5)
  stopifnot(length(param_pieces) %% 2 == 0)
  param_pairs <- matrix(param_pieces, 2, byrow=T)
  param_pairs[,1] <- sapply(param_pairs[,1], substring, 2)  # -x0 -> x0
  params <- param_pairs_to_params(param_pairs)
  params
}

# extracts the next fold. Returns the index of the next fold
# e.g. input: "cvfold-0 0 18000.0 2147483647 4 -x0 '6.98467896812' -x1 '13.4314026444'"
parse_smac_cv_fold <- function(param_string) {
  fold_descriptor <- (param_string %>% trimws %>% strsplit(" "))[[1]][1]
  stopifnot(startsWith(fold_descriptor, "cvfold-"))
  as.numeric(strsplit(fold_descriptor, '-')[[1]][2])
}

# returns list (names(.) == c('x', 'x_int', 'x_categorical')) with vectors
# categorical vector has names
param_pairs_to_params <- function(param_pairs) {
  res_df <- apply(param_pairs, 1, function(row) {
    name <- row[1]
    val <- row[2] %>% gsub("^'|'$", "", .)
    
    if (startsWith(name, "x_categorical"))
      c("x_categorical", name, val)
    else if (startsWith(name, "x_int")) 
      c("x_int", substring(name, nchar('x_int')+1), val)
    else if (startsWith(name, "x")) 
      c("x", substring(name, nchar('x')+1), val)
    else {
      browser()
      stop('undef type')
    }
  }) %>% t %>% data.frame(stringsAsFactors=F)
  
  # res_df e.g.:
  #     X1  X2                    X3
  # 1  'x'  '1' '6.9846789681200185'
  # 2  'x'  '0'  '13.43140264469383'
  
  sapply(unique(res_df[1]), function(type) {
    type_df <- res_df[res_df$X1 == type, 2:ncol(res_df)]
    if (type == 'x_categorical') {
      # try to recognize a categorical value type and coerce it
      for (as in c('numeric', 'logical')) {
        attempt <- suppressWarnings(do.call(paste0('as.', as), 
                                            list(tolower(type_df[,2]))))
        if (all(!is.na(attempt))) {
          type_df[,2] <- attempt
          break
        }
      }
      res <- type_df[,2]
      names(res) <- type_df[,1]
      res
    } else {
      type_df <- data.frame(lapply(type_df, as.numeric))
      type_df <- type_df[order(type_df[,1]),]
      type_df[,2]
    }
  }, simplify=F)
}

# input e.g. "12.959609, 0.542677, 2.103, 100, 2.959983000, x0='3.234329', x1='1.88202'"
parse_smac_trajectory_string <- function (param_string) {
  columns <- param_string %>% trimws %>% strsplit(", *") %>% `[[`(1)
  fval <- as.numeric(columns[2])
  params_raw <- columns %>% tail(-5)
  param_pairs <- params_raw %>% strsplit('=') %>% unlist %>% matrix(2, byrow=T)
  output_params <- param_pairs_to_params(param_pairs)
  list(output_params, fval)
}
