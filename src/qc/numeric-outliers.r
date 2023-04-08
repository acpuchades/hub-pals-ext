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
        mutate(param_zscore = as.vector(scale(param_value))) |>
        ungroup() |>
        filter(abs(param_zscore) >= min_zscore)
}

omit_records_from <- function(data, path, by = NULL) {
    if (file.exists(path)) {
        ignored <- read_csv(path)
        by <- ifelse(!is.null(by), by, colnames(ignored))
        data <- data |> anti_join(ignored, by = by)
    }
    data
}

order_by_relevance <- function(data) {
    data |>
        group_by(id_paciente) |>
        mutate(max_abs_zscore = max(abs(param_zscore))) |>
        ungroup() |>
        arrange(desc(max_abs_zscore), id_paciente, desc(abs(param_zscore))) |>
        select(-max_abs_zscore)
}

nutrition_outliers <- ufmn_nutrition |>
    find_numeric_outliers(id_paciente, id_visita, fecha_visita) |>
    omit_records_from("src/qc/nutrition-outliers.ignore") |>
    left_join(ufmn_patients, by = "id_paciente", multiple = "all") |>
    select(id_paciente, nhc, cip, id_visita, fecha_visita, starts_with("param_")) |>
    order_by_relevance()

respiratory_outliers <- ufmn_respiratory |>
    find_numeric_outliers(id_paciente, id_visita, fecha_visita) |>
    omit_records_from("src/qc/respiratory-outliers.ignore") |>
    left_join(ufmn_patients, by = "id_paciente", multiple = "all") |>
    select(id_paciente, nhc, cip, id_visita, fecha_visita, starts_with("param_")) |>
    order_by_relevance()
