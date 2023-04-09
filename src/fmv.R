library(dplyr)
library(stringr)

fmv_data_path <- "data/fmv-20221130.xlsx"

fmv_recode_boolean <- function(data) {
    data |>
        str_to_upper() |>
        recode(
            SI = TRUE,
            NO = FALSE,
            .default = NA
        )
}

fmv_recode_workingstatus <- function(data) {
    data |>
        str_to_upper() |>
        recode_factor(
            PENDENT = NA_character_,
            `PENDENT INCAPACITAT` = NA_character_,
            RES = "No ingresos",
            ATUR = "En paro",
            ACTIU = "Trabaja",
            BAIXA = "Baja laboral",
            `BAIXA LABORAL` = "Baja laboral",
            JUBILACIO = "Jubilacion",
            INCAPCITAT = "Incapacidad",
            INCAPACITAT = "Incapacidad",
            PNC = "Pension no contributiva",
            VIUDETAT = "Pension de viudedad",
            SOVI = "Seguro obligatorio vejez e invalidez",
        )
}

fmv_recode_status <- function(data) {
    data |>
        str_to_upper() |>
        recode_factor(
            `-` = NA_character_,
            SI = "Tramitada",
            NO = "No tramitada",
            PENDENT = "Pendiente"
        )
}

fmv_recode_disability <- function(data) {
    data %>% recode_factor(
        TEMPORAL = "IT",
        PARCIAL = "IPP",
        TOTAL = "IPT",
        ABSOLUTA = "IPA",
        GI = "GI"
    )
}

fmv_recode_date <- function(data) {
    data |>
        str_replace(r"(^(\d{2})/(\d{2})$)", r"(01/\1/\2)") |>
        str_replace(r"(^principis? (\d{4})$)", r"(01/01/\1)") |>
        str_replace(r"(^mitjans (\d{4})$)", r"(01/06/\1)") |>
        str_replace(r"(^(\d{4})$)", r"(01/01/\1)") |>
        lubridate::dmy(quiet = TRUE)
}

fmv_data <- readxl::read_excel(fmv_data_path) |>
    select(
        nif = NIF,
        cuidador_profesional = `Cuidador professional`,
        cuidador_familiar = `Cuidador familiar`,
        estado_discapacidad = GD,
        fecha_tramite_discapacidad = `Data solicitud GD`,
        grado_discapacidad = `% GD`,
        estado_lapad = `Llei de la dependencia`,
        grado_lapad = `Grau de dependencia`,
        fecha_tramite_lapad = `Data solicitud dependencia`,
        situacion_laboral = `Estat laboral`,
        estado_incapacidad = `Incapacitat`,
        fecha_tramite_incapacidad = `Data solicitud incapacitat`,
        tipo_incapacidad = `Tipus incapacitat`,
    ) |>
    mutate(
        across(starts_with("fecha_"), fmv_recode_date),
        across(starts_with("estado_"), fmv_recode_status),
        across(starts_with("cuidador_"), fmv_recode_boolean),
        nif = str_replace_all(nif, r"(-|\s)", ""),
        situacion_laboral = fmv_recode_workingstatus(situacion_laboral),
        grado_lapad = factor(grado_lapad, levels = c("I", "II", "III"), ordered = TRUE),
        tipo_incapacidad = fmv_recode_disability(tipo_incapacidad),
    )
