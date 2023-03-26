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
        mutate(param_zscore = scale(param_value)) |>
        ungroup() |>
        filter(abs(param_zscore) >= min_zscore) |>
        arrange(desc(abs(param_zscore)))
}

nutr_outliers <- ufmn_nutrition |>
    find_numeric_outliers(pid, fecha_visita) |>
    left_join(ufmn_patients, by = "pid", multiple = "all") |>
    select(pid, nhc, cip, fecha_visita, starts_with("param_"))

resp_outliers <- ufmn_respiratory |>
    find_numeric_outliers(pid, fecha_visita) |>
    left_join(ufmn_patients, by = "pid", multiple = "all") |>
    select(pid, nhc, cip, fecha_visita, starts_with("param_"))

output_dir <- "output/qc"
dir.create(output_dir, showWarnings = FALSE)
output_path <- file.path(output_dir, "numeric-outliers.xlsx")

write_xlsx(list(
    "ufmn_nutrition" = nutr_outliers,
    "ufmn_respiratory" = resp_outliers
), output_path)
