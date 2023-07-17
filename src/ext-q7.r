library(dplyr)
library(readxl)

source("src/ufmn.r")

as_education_level <- function(x) {
    factor(x, ordered = TRUE, levels = c(
        "Illiterate",
        "Primary education (incomplete)",
        "Primary education (complete)",
        "Secondary education",
        "Higher education",
        "University degree",
        "Doctorate"
    ))
}

pals_patient_ids <- read_excel("data/pals-patient-ids.xlsx") %>%
    rename(ufmn_id = "patient_id", pals_id = `P-ALS ID`)

q7_weight_baseline <- ufmn_nutrition %>%
    slice_min(fecha_visita, n = 1, by = id_paciente) %>%
    inner_join(pals_patient_ids, by = c(id_paciente = "ufmn_id")) %>%
    select(
        pals_id,
        weight_premorbid = peso_premorbido,
        weight_baseline = peso,
        baseline_date = fecha_visita
    )

q7_weight_assessments <- ufmn_nutrition %>%
    inner_join(pals_patient_ids, by = c(id_paciente = "ufmn_id")) %>%
    select(
        pals_id,
        assessment_date = fecha_visita,
        weight = peso
    ) %>%
    drop_na() %>%
    arrange(pals_id, assessment_date)

q7_education <- ufmn_patients %>%
    inner_join(pals_patient_ids, by = c(id_paciente = "ufmn_id")) %>%
    transmute(
        pals_id,
        education_level = as_education_level(case_match(
            estudios,
            "Analfabeto" ~ "Illiterate",
            "Primarios incompletos" ~ "Primary education (incomplete)",
            "Primarios completos" ~ "Primary education (complete)",
            "Secundarios" ~ "Secondary education",
            "Formacion profesional" ~ "Higher education",
            "Universitarios" ~ "University degree",
            "Doctorado" ~ "Doctorate"
        ))
    )
