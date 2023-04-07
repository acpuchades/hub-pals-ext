source("src/ufmn.r")

followups_before_onset <- ufmn_followups |>
    left_join(ufmn_patients, by = "id_paciente", multiple = "all") |>
    left_join(ufmn_clinical, by = "id_paciente", multiple = "all") |>
    filter(fecha_visita < fecha_inicio_clinica) |>
    select(id_paciente, nhc, cip, id_visita, fecha_visita, tipo_visita, fecha_inicio_clinica)

followups_after_death <- ufmn_followups |>
    left_join(ufmn_patients, by = "id_paciente", multiple = "all") |>
    filter(fecha_visita > fecha_exitus) |>
    select(id_paciente, nhc, cip, id_visita, fecha_visita, tipo_visita, fecha_exitus)
