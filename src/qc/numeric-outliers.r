library(dplyr)
library(tidyr)
library(writexl)

source("src/ufmn.r")

find_numeric_outliers <- function(data, ..., min_zscore = 3) {
    data |>
        select(..., where(is.numeric)) |>
        pivot_longer(
            where(is.numeric),
            names_to = "param_name",
            values_to = "param_value"
        ) |>
        group_by(param_name) |>
        mutate(param_zscore = c(scale(param_value))) |>
        ungroup() |>
        filter(abs(param_zscore) >= min_zscore) |>
        arrange(desc(abs(param_zscore)))
}

nutrition_outliers <- ufmn_nutrition |>
    find_numeric_outliers(pid, fecha_visita) |>
    left_join(ufmn_patients, by = "pid", multiple = "all") |>
    select(pid, nhc, cip, fecha_visita, starts_with("param_"))

respiratory_outliers <- ufmn_respiratory |>
    find_numeric_outliers(pid, fecha_visita) |>
    left_join(ufmn_patients, by = "pid", multiple = "all") |>
    select(pid, nhc, cip, fecha_visita, starts_with("param_"))
