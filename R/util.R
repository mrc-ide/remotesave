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


unsuccessful <- function(x) {
  list(success = FALSE, value = NULL, error = x)
}


successful <- function(x) {
  list(success = TRUE, value = x, error = NULL)
}


with_success <- function(expr) {
  tryCatch(
    successful(expr),
    error = function(e) unsuccessful(e$message))
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}
