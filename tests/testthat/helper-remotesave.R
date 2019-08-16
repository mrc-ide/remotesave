TEST_ROOT <- "remotesave:testing"


remove_all_keys <- function() {
  tryCatch(
    redux::scan_del(redux::hiredis(), paste0(TEST_ROOT, ":*")),
    error = function(...) NULL)
}
