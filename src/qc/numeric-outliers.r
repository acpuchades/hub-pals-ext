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
    find_numeric_outliers(id_paciente, id_visita, fecha_visita) |>
    left_join(ufmn_patients, by = "id_paciente", multiple = "all") |>
    select(id_paciente, nhc, cip, id_visita, fecha_visita, starts_with("param_"))

nutrition_outliers_ignore_path <- "src/qc/nutrition-outliers.ignore"
if (file.exists(nutrition_outliers_ignore_path)) {
    ignored <- read_csv(nutrition_outliers_ignore_path)
    nutrition_outliers <- nutrition_outliers |>
        anti_join(ignored, by = c("id_visita", "param_name"))
}

respiratory_outliers <- ufmn_respiratory |>
    find_numeric_outliers(id_paciente, id_visita, fecha_visita) |>
    left_join(ufmn_patients, by = "id_paciente", multiple = "all") |>
    select(id_paciente, nhc, cip, id_visita, fecha_visita, starts_with("param_"))

respiratory_outliers_ignore_path <- "src/qc/respiratory-outliers.ignore"
if (file.exists(respiratory_outliers_ignore_path)) {
    ignored <- read_csv(respiratory_outliers_ignore_path)
    respiratory_outliers <- respiratory_outliers |>
        anti_join(ignored, by = c("id_visita", "param_name"))
}
