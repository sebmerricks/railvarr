env <- new.env(parent = emptyenv())
env$raw_centrix <- NA

# 2 approaches possible here:
#    1. act as a helper function for wrangle_centrix()
#    2. act as a separate function that reads the raw data into a stream / saves
#       it locally so that wrangle_centrix() can use it when needed
# I think I prefer option 2

#' @export
read_centrix <- function(path) {
  env$raw_centrix <- path
}

# takes the raw data as input and returns the fully processed data

#' @export
wrangle_centrix <- function() {
  env$raw_centrix
}
