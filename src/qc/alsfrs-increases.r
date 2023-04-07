source("src/ufmn.r")

alsfrs_increases <- ufmn_functional |>
    left_join(ufmn_patients, by = "pid") |>
    select(pid, nhc, cip, fecha_visita, alsfrs_total) |>
    drop_na(pid, fecha_visita, alsfrs_total) |>
    group_by(pid) |>
    arrange(fecha_visita, .by_group = TRUE) |>
    mutate(
        fecha_visita_previa = lag(fecha_visita),
        alsfrs_total_previo = lag(alsfrs_total),
        alsfrs_diff = alsfrs_total - lag(alsfrs_total)
    ) |>
    ungroup() |>
    filter(alsfrs_diff > 0) |>
    arrange(desc(alsfrs_diff)) |>
    relocate(nhc:cip, .after = pid) |>
    relocate(fecha_visita_previa, .after = fecha_visita) |>
    relocate(alsfrs_total, .after = fecha_visita_previa) |>
    relocate(alsfrs_total_previo, .after = alsfrs_total) |>
    relocate(alsfrs_diff, .after = alsfrs_total_previo)

ignore_path <- "src/qc/alsfrs-increases.ignore"
if (file.exists(ignore_path)) {
    ignored <- read_csv(ignore_path, col_types = cols(
        pid = col_character(),
        fecha_visita = col_date("%d/%m/%Y")
    ))
    alsfrs_increases <- alsfrs_increases |>
        anti_join(ignored, by = c("pid", "fecha_visita"))
}
