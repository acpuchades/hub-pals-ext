library(writexl)

source("src/ufmn.r")

alsfrs_increases <- ufmn_functional |>
    left_join(
        ufmn_patients |> select(pid, nhc, cip),
        by = "pid"
    ) |>
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

output_dir <- "output/qc"
output_path <- file.path(output_dir, "alsfrs-increases.xlsx")
dir.create(output_dir, showWarnings = FALSE)
write_xlsx(alsfrs_increases, output_path)
