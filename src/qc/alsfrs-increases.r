source("src/ufmn.r")

alsfrs_increases <- ufmn_functional |>
    left_join(ufmn_patients, by = "id_paciente") |>
    select(id_paciente, nhc, cip, id_visita, fecha_visita, alsfrs_total) |>
    drop_na(id_paciente, fecha_visita, alsfrs_total) |>
    group_by(id_paciente) |>
    arrange(fecha_visita, .by_group = TRUE) |>
    mutate(
        id_visita_previa = lag(id_visita),
        fecha_visita_previa = lag(fecha_visita),
        alsfrs_total_previo = lag(alsfrs_total),
        alsfrs_diff = alsfrs_total - lag(alsfrs_total)
    ) |>
    ungroup() |>
    filter(alsfrs_diff > 0) |>
    arrange(desc(alsfrs_diff)) |>
    relocate(nhc:cip, .after = id_paciente) |>
    relocate(alsfrs_total, .before = alsfrs_total_previo)

ignore_path <- "src/qc/alsfrs-increases.ignore"
if (file.exists(ignore_path)) {
    ignored <- read_csv(ignore_path)
    alsfrs_increases <- alsfrs_increases |>
        anti_join(ignored, by = "id_visita")
}
