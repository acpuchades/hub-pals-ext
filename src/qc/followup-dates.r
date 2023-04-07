source("src/ufmn.r")

followups_before_onset <- ufmn_followups |>
    left_join(ufmn_patients, by = "pid", multiple = "all") |>
    left_join(ufmn_clinical, by = "pid", multiple = "all") |>
    filter(fecha_visita < fecha_inicio_clinica) |>
    select(pid, nhc, cip, origin, fecha_visita, fecha_inicio_clinica)

followups_after_death <- ufmn_followups |>
    left_join(ufmn_patients, by = "pid", multiple = "all") |>
    filter(fecha_visita > fecha_exitus) |>
    select(pid, nhc, cip, origin, fecha_visita, fecha_exitus)
