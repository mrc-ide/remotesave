`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}


vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}
