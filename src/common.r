library(dplyr)

rows_reset <- function(x, ...) {
    x |>
        rows_delete(...) |>
        rows_insert(...)
}
