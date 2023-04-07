library(dplyr)
library(magrittr)
library(purrr)
library(stringr)
library(stringi)
library(readr)
library(tidyr)

source("src/utils.r")

ufmn_data_path <- "data/ufmn-2023_01_18.sqlite"

ufmn_parse_na <- function(data, na_empty = FALSE) {
  if (na_empty) {
    data %<>% na_if("")
  }

  data %>%
    na_if("-") %>%
    na_if("NS/NC")
}

ufmn_parse_date <- function(data) {
  data %>%
    ufmn_parse_na(na_empty = TRUE) %>%
    as.Date(format = "%d-%m-%Y")
}

ufmn_parse_logical <- function(data, true, false) {
  case_when(
    data %in% true ~ TRUE,
    data %in% false ~ FALSE,
  )
}

ufmn_parse_factor <- function(data, ...) {
  data %>%
    ufmn_parse_na(na_empty = TRUE) %>%
    factor(...)
}

ufmn_parse_studies <- function(data) {
  data %>%
    recode(
      `No sabe leer ni escribir` = "Analfabeto",
      `Secundarios (ESO, BUP, COU, etc)` = "Secundarios",
      `FP (grado medio o superior)` = "Formacion profesional",
      Universidad = "Universitarios",
      Otro = "Otros"
    ) %>%
    ufmn_parse_factor(
      levels = c(
        "Otros",
        "Analfabeto",
        "Primarios incompletos",
        "Primarios completos",
        "Secundarios",
        "Formacion profesional",
        "Universitarios",
        "Doctorado"
      ),
      ordered = TRUE
    )
}

ufmn_parse_working_status <- function(data) {
  data %>%
    recode(
      Parado = "En paro (sin subsidio)",
      `Parado con subsidio | Prestación` = "En paro (con subsidio)",
      `Incapacitado (o con invalidez permanente)` = "Incapacitado",
      Otra = "Otros"
    ) %>%
    ufmn_parse_factor()
}

ufmn_parse_genetic_result <- function(data, ...) {
  data %>%
    ufmn_parse_factor(levels = c("Normal", "Alterado"), ...)
}

ufmn_parse_municipality <- function(data) {
  data %>%
    ufmn_parse_na() %>%
    str_replace("L'\\s+", "L'") %>%
    recode(
      `FERROL (2)` = "FERROL",
      `MOLÃƒÂ D'AVALL` = "MOLI D'AVALL",
      `PRAT DE LLOBREGAT` = "EL PRAT DE LLOBREGAT",
      `SANT JOAN DE VILATORRSANT JOAN DE VILASANT JOAN DE VILATORRADATORRADAADA` = "SANT JOAN DE VILATORRADA",
      `SANT SADURNÃ D'ANOIA` = "SANT SADURNI D'ANOIA",
      `SANT LLORENÃƒÆ’Ã¢â‚¬Â¡ D'HORTONS` = "SANT LLORENÇ D'HORTONS"
    ) %>%
    stri_trans_general(id = "Latin-ASCII")
}

ufmn_parse_cognitive <- function(data) {
  data %>%
    recode(
      `Deterioro Cognitivo Leve cognitivo (DCL cognitivo)` = "DCL-Cognitivo",
      `Deterioro Cognitivo Leve conductual (DCL conductual)` = "DCL-Conductual",
      `Deterioro Cognitivo Leve mixto (DCL mixto)` = "DCL-Mixto",
      `Demencia frontotemporal` = "DFT",
      `Demencia tipo alzheimer` = "DTA"
    ) %>%
    ufmn_parse_factor(levels = c(
      "Otros", "Normal",
      "DCL-Cognitivo", "DCL-Conductual",
      "DCL-Mixto", "DTA", "DFT"
    ))
}

ufmn_parse_phenotype <- function(data) {
  data %>%
    recode(
      `Atrofia Muscular Progresiva (AMP)` = "AMP",
      `Esclerosis Lateral Primaria (ELP)` = "ELP",
      `Parálisis bulbar progresiva` = "PBP",
      `Flail arm` = "Flail-Arm",
      `Flail leg` = "Flail-Leg",
      `Hemipléjica (Mills)` = "Hemiplejica",
      `Monomiélica` = "Monomielica",
      `Pseudopolineurítica` = "Pseudopolineuritica"
    ) %>%
    ufmn_parse_factor()
}

ufmn_parse_distribution <- function(data) {
  data %>%
    str_split("@", n = 3) %>%
    map_chr(\(xs)
    case_when(
      any(xs == "MMSS") ~ "MMSS",
      any(xs == "EESS") ~ "MMSS",
      any(xs == "MMII") ~ "MMII",
      any(xs == "EEII") ~ "MMII",
      any(xs == "AMBAS") ~ "MMSS+MMII",
      any(xs == "MMSS Y MMII") ~ "MMSS+MMII",
      any(xs == "AMBAS IZQUIERDA") ~ "MMSS+MMII",
      any(str_detect(xs, "GENERALIZAD[OA]")) ~ "MMSS+MMII",
      any(xs == "BULBAR") ~ "Bulbar",
      any(str_detect(xs, "RESPIRATORI[OA]")) ~ "Respiratoria",
    )) %>%
    ufmn_parse_factor(levels = c(
      "Bulbar",
      "Respiratoria",
      "MMSS",
      "MMII",
      "MMSS+MMII"
    ))
}

ufmn_parse_involvement <- function(data) {
  data %>%
    str_split("@", n = 3) %>%
    map_chr(\(xs)
    case_when(
      any(xs == "BMN") ~ "MNS+MNI",
      any(xs == "UMN->BMN") ~ "MNS",
      any(xs == "UMN") ~ "MNS",
      any(xs == "LMN") ~ "MNI",
    )) %>%
    ufmn_parse_factor(levels = c("MNS", "MNI", "MNS+MNI"))
}

ufmn_parse_predominance <- function(data) {
  data %>%
    str_split("@", n = 3) %>%
    map_chr(\(xs) {
      xs <- rev(xs) # afectación @ predominio @ debilidad
      case_when(
        any(xs == "PREDOMINIO UMN") ~ "MNS",
        any(xs == "PREDOMINIO LMN") ~ "MNI",
        any(xs == "NINGUN PREDOMINIO") ~ "Ninguno",
        any(xs == "NINGUN PREDOMINIO+EXTRAPIRAMIDAL") ~ "Ninguno",
        any(xs == "PREDOMINIO LMN EN MMSS y PREDOMINIO UMN EN MMSS") ~ "Ninguno",
        any(xs == "UMN") ~ "MNS",
        any(xs == "LMN") ~ "MNI",
      )
    }) %>%
    ufmn_parse_factor(levels = c("Ninguno", "MNS", "MNI"))
}

ufmn_parse_resultado_estudio_atxn2 <- function(data) {
  atxn2_re <- regex("\\bATXN2\\b", ignore_case = TRUE)
  normal_re <- regex("\\bNORMAL\\b", ignore_case = TRUE)
  intermediate_re <- regex("\\bINTERMEDI[AO]?\\b", ignore_case = TRUE)

  data %>%
    str_split("@", n = 3) %>%
    map_chr(\(xs) case_when(
      any(str_detect(xs, atxn2_re) & str_detect(xs, normal_re)) ~ "Normal",
      any(str_detect(xs, atxn2_re) & str_detect(xs, intermediate_re)) ~ "Alterado"
    )) %>%
    ufmn_parse_genetic_result()
}

ufmn_parse_resultado_estudio_kennedy <- function(data) {
  kennedy_re <- regex("\\bKENNEDY\\b", ignore_case = TRUE)
  normal_re <- regex("\\bNORMAL\\b", ignore_case = TRUE)
  positive_re <- regex("\\bPOSITIVO\\b", ignore_case = TRUE)

  data %>%
    map_chr(\(xs) case_when(
      any(str_detect(xs, kennedy_re) & str_detect(xs, normal_re)) ~ "Normal",
      any(str_detect(xs, kennedy_re) & str_detect(xs, positive_re)) ~ "Alterado",
    )) %>%
    ufmn_parse_genetic_result()
}

ufmn_parse_dysphagia <- function(data) {
  data %>%
    recode(
      `Sí sólidos` = "Solidos",
      `Sí líquidos` = "Liquidos",
      `Sí líquidos y sólidos` = "Mixta"
    ) %>%
    ufmn_parse_factor(levels = c("No", "Solidos", "Liquidos", "Mixta"))
}

ufmn_parse_peg_usage <- function(data) {
  data %>%
    recode(
      HidratacionMeditacion = "Hidratacion + Medicacion",
      `HidratacionMedicaciónNutricionParcial` = "Hidratacion + Medicacion + Nutricion parcial",
      NutricionCompleta = "Nutricion completa"
    ) %>%
    ufmn_parse_factor(levels = c(
      "Hidratacion",
      "Hidratacion + Medicacion",
      "Hidratacion + Medicacion + Nutricion parcial",
      "Nutricion completa"
    ), ordered = TRUE)
}

ufmn_db <- DBI::dbConnect(RSQLite::SQLite(), ufmn_data_path)

ufmn_patients <- DBI::dbReadTable(ufmn_db, "pacientes") %>%
  select(!c(id, created_datetime:updated_datetime)) %>%
  rename(
    id_paciente = pid,
    situacion_laboral_al_inicio = situacion_laboral_actual,
    situacion_laboral_al_inicio_otro = situacion_laboral_actual_otra,
    ultima_ocupacion_al_inicio = ultima_ocupacion,
    estudios_otros = estudios_otro_cual
  ) %>%
  rows_delete(tibble(id_paciente = "9342fe7c-d949-11e9-842a-ebf9c1d8fdac"), by = "id_paciente") %>% # no data
  mutate(
    across(everything(), ufmn_parse_na),
    across(nhc, parse_integer),
    fecha_nacimiento = ufmn_parse_date(fecha_nacimiento),
    sexo = ufmn_parse_factor(sexo),
    exitus = ufmn_parse_logical(exitus, true = "Sí", false = "No"),
    fecha_exitus = ufmn_parse_date(fecha_exitus),
    estudios = ufmn_parse_studies(estudios),
    situacion_laboral_al_inicio = ufmn_parse_working_status(situacion_laboral_al_inicio),
    municipio_residencia = ufmn_parse_municipality(municipio_residencia)
  ) %>%
  relocate(cip, .after = nhc) %>%
  structure(class = c("ufmn", class(.)))

ufmn_clinical <- DBI::dbReadTable(ufmn_db, "datos_clinicos") %>%
  select(!c(estudio_genetico_c9:estudio_genetico_sod1, created_datetime:updated_datetime)) %>%
  rename(
    id_paciente = pid,
    fecha_primera_visita = fecha_visita_datos_clinicos,
    fecha_diagnostico = fecha_diagnostico_ELA,
    antecedentes_otros = otros_antecedentes_de_interes,
    historia_familiar_otros = historia_familiar_cual,
    historia_familiar_alzheimer_grado = historia_familiar_alzheimer_quien,
    historia_familiar_parkinson_grado = historia_familiar_parkinson_quien,
    historia_familiar_motoneurona_grado = historia_familiar_motoneurona_quien,
    resultado_estudio_cognitivo = estudio_cognitivo,
    resultado_estudio_cognitivo_otros = estudio_cognitivo_otros,
    estudio_gen_c9 = resultado_estudio_c9,
    estudio_gen_sod1 = resultado_estudio_sod1,
    estudio_gen_otros = estudio_genetico_otro
  ) %>%
  mutate(across(everything(), ufmn_parse_na),
    across(starts_with("fecha"), ufmn_parse_date),
    across(c(
      historia_familiar,
      historia_familiar_motoneurona,
      historia_familiar_alzheimer,
      historia_familiar_parkinson,
      deterioro_cognitivo,
      riluzol
    ), \(x) {
      ufmn_parse_logical(x, true = "Sí", false = "No")
    }),
    fumador = ufmn_parse_factor(fumador),
    resultado_estudio_cognitivo = ufmn_parse_cognitive(resultado_estudio_cognitivo),
    fenotipo_al_diagnostico = ufmn_parse_phenotype(fenotipo_al_diagnostico),
    fenotipo_al_exitus = ufmn_parse_phenotype(fenotipo_al_exitus),
    patron_debilidad_inicial = ufmn_parse_distribution(distribucion_al_inicio),
    afectacion_motoneurona_inicial = ufmn_parse_involvement(distribucion_al_inicio),
    predominio_motoneurona_inicial = ufmn_parse_predominance(distribucion_al_inicio),
    estudio_gen_c9 = ufmn_parse_genetic_result(estudio_gen_c9),
    estudio_gen_sod1 = ufmn_parse_genetic_result(estudio_gen_sod1),
    estudio_gen_atxn2 = ufmn_parse_resultado_estudio_atxn2(estudio_gen_otros),
    estudio_gen_kennedy = ufmn_parse_resultado_estudio_kennedy(estudio_gen_otros)
  ) %>%
  select(!c(id, distribucion_al_inicio)) %>%
  relocate(fumador, .after = fenotipo_al_exitus_otro) %>%
  relocate(starts_with("historia_familiar_motoneurona"),
    .after = historia_familiar
  ) %>%
  relocate(starts_with("historia_familiar_alzheimer"),
    .after = historia_familiar_alzheimer
  ) %>%
  relocate(starts_with("historia_familiar_parkinson"),
    .after = historia_familiar_parkinson
  ) %>%
  relocate(historia_familiar_otros,
    .after = historia_familiar_parkinson_grado
  ) %>%
  relocate(patron_debilidad_inicial:predominio_motoneurona_inicial,
    .after = everything()
  ) %>%
  relocate(starts_with("estudio_gen_") & !ends_with("_otros"),
    .after = everything()
  ) %>%
  relocate(estudio_gen_otros, .after = everything()) %>%
  relocate(ends_with("riluzol"), .after = everything())

ufmn_nutrition <- DBI::dbReadTable(ufmn_db, "datos_antro") %>%
  rename(
    id_visita = id,
    id_paciente = pid,
    fecha_visita = fecha_visita_datos_antro,
    imc = imc_actual,
    fecha_inicio_supl_oral = fecha_suplementacion_nutricional,
    fecha_inicio_supl_enteral = fecha_inicio_suplementacion_nutricional_entera,
    estreñimiento = restrenimiento,
    retirada_peg = retirada,
    supl_oral = suplementacion_nutricional_oral,
    supl_enteral = suplementacion_nutricional_entera,
  ) %>%
  rows_update(tibble(id_visita = "40c68842-eeb1-4cd2-a0d8-c5cbc839730c", fecha_visita = NA), by = "id_visita") %>% # was '99-99-9999'
  rows_update(tibble(id_visita = "67e615f4-5f01-11eb-a21b-8316bff80df0", fecha_visita = "03-12-2019"), by = "id_visita") %>% # was 03-12-20219
  rows_update(tibble(id_visita = "f9054526-1dcc-11eb-bb4a-9745fc970131", fecha_indicacion_peg = "23-10-2020"), by = "id_visita") %>% # was 23-10-20020
  rows_update(tibble(id_visita = "8c5b0f46-df7a-11e9-9c30-274ab37b3217", fecha_indicacion_peg = "20-07-3018"), by = "id_visita") %>% # was 20-07-3018
  rows_update(tibble(id_visita = "eb700688-3dfe-11eb-9383-d3a3b2195eff", fecha_complicacion_peg = "22-11-2020"), by = "id_visita") %>% # was 22-11-202
  mutate(across(starts_with("fecha_"), \(x) ifelse(x == "29-02-2015", "28-02-2015", x))) %>%
  mutate(across(everything(), ufmn_parse_na),
    across(starts_with("fecha"), ufmn_parse_date),
    across(c(
      estatura, peso, peso_premorbido,
      peso_colocacion_peg, imc
    ), parse_double),
    across(c(
      indicacion_peg, portador_peg, complicacion_peg,
      retirada_peg, espesante, supl_oral, supl_enteral,
      estreñimiento, laxante,
    ), \(x) {
      ufmn_parse_logical(x, true = "Sí", false = "No")
    }),
    across(starts_with("motivo_indicacion_"), \(x) {
      ufmn_parse_logical(x, true = "TRUE", false = "FALSE")
    }),
    imc = ifelse(!is.na(imc), imc, round(peso / (estatura / 100)^2, 2)),
    uso_peg = ufmn_parse_peg_usage(uso_peg),
    disfagia = ufmn_parse_dysphagia(disfagia)
  ) %>%
  select(!c(created_datetime:updated_datetime)) %>%
  arrange(id_paciente, fecha_visita)

ufmn_respiratory <- DBI::dbReadTable(ufmn_db, "fun_res") %>%
  rename(
    id_visita = id,
    id_paciente = pid,
    fecha_visita = fecha_visita_fun_res,
    sintomas_hipoventilacion_nocturna = sintomas_sintomas_de_hipoventilacion_nocturna,
    tipo_patologia_respiratoria_intersticial = tipo_patologia_respiratoria_patologia_instersticial,
    sas_apneas_no_claramente_obstructivas = sas_apneas_no_claramanete_obstructivas,
    indicacion_vmni = vmni_indicacion,
  ) %>%
  rows_update(tibble(id_visita = "c2049bdf-4a91-43e0-b6c4-f770881b7499", fecha_visita = NA), by = "id_visita") %>% # was 99-99-9999
  rows_update(tibble(id_visita = "a3608f72-82eb-11e9-aed7-57f320d0dba4", fecha_realizacion_polisomnografia = NA), by = "id_visita") %>% # was 14
  rows_update(tibble(id_visita = "f508e4b8-db93-11e9-b372-090a91bd3693", fecha_realizacion_polisomnografia = NA), by = "id_visita") %>% # was 14
  rows_update(tibble(id_visita = "31f94d2a-fb08-11e9-b780-81f732616a71", odi3 = NA), by = "id_visita") %>% # was 17/7
  rows_update(tibble(id_visita = "a0a869b6-526b-44f3-9137-044c30b29551", fvc_estirado_absoluto = "2450"), by = "id_visita") %>% # was 24500
  rows_update(tibble(id_visita = "1915a65d-ecd0-4390-b60f-a87ea269898d", ct90 = NA, sao2_media = NA), by = "id_visita") %>% # multiple errors
  rows_update(tibble(id_visita = "0a668c6e-89ee-11e8-8428-29c4852a0217", hco3 = "33"), by = "id_visita") %>% # was 3
  rows_update(tibble(id_visita = "172665b8-47b4-11e8-915c-1db1c3288c02", fvc_sentado_absoluto = "920", fvc_estirado_absoluto = "920"), by = "id_visita") %>% # both were 9100
  rows_update(tibble(id_visita = "2786aece-9a1e-11e8-9eb2-af099be7b978", ct90 = "6"), by = "id_visita") %>% # was 99.7
  rows_update(tibble(id_visita = "3f94f6b5-0478-413c-96a4-86607e149447", pao2 = "102", paco2 = "32", hco3 = "21"), by = "id_visita") %>% # values were flipped or missing
  rows_update(tibble(id_visita = "7ee47f8a-bdf0-4db2-88b2-5f1d2ac28061", ct90 = "0.8", sao2_media = "95.98"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "7dcc96e9-7c4d-4535-a069-916574678a2c", ph_sangre_arterial = "7.46", hco3 = "27"), by = "id_visita") %>% # values were missing
  rows_update(tibble(id_visita = "613d31ec-fd2f-42c2-8ef8-fb90b0704bea", ph_sangre_arterial = NA, pao2 = NA, paco2 = NA, hco3 = NA), by = "id_visita") %>% # abg not performed
  rows_update(tibble(id_visita = "e8d20b44-9d34-4f8a-b076-2c1eb51473fc", ph_sangre_arterial = NA, pao2 = NA, paco2 = NA, hco3 = NA), by = "id_visita") %>% # abg not performed
  rows_update(tibble(id_visita = "7dd109f6-ce38-11e9-8beb-d1a9b58e35a5", fvc_estirado = "89"), by = "id_visita") %>%
  rows_update(tibble(id_visita = "f8ed6057-1f8f-4364-b39d-af298e032b8d", ph_sangre_arterial = NA, pao2 = NA, paco2 = NA, hco3 = NA), by = "id_visita") %>% # abg not performed
  rows_update(tibble(id_visita = "41b51e5e-ce28-11e9-8beb-d1a9b58e35a5", pim = "18"), by = "id_visita") %>% # was 218
  rows_update(tibble(id_visita = "1d3044ed-6c4b-4c87-9a8f-522c92e4bbe4", ph_sangre_arterial = NA, pao2 = NA, paco2 = NA, hco3 = NA), by = "id_visita") %>% # abg not performed
  rows_update(tibble(id_visita = "ef41001c-8e86-4d5f-a340-cec5d0291c53", ph_sangre_arterial = NA, pao2 = NA, paco2 = NA, hco3 = NA), by = "id_visita") %>% # abg not performed
  rows_update(tibble(id_visita = "66462823-8dfd-4654-a9ef-2200c618b13f", ph_sangre_arterial = NA, pao2 = NA, paco2 = NA, hco3 = NA), by = "id_visita") %>% # abg not performed
  rows_update(tibble(id_visita = "e45cdc18-1da2-11ea-a82e-af00877d97ef", fvc_sentado_absoluto = "4120"), by = "id_visita") %>% # was 41200
  rows_update(tibble(id_visita = "38156092-e761-11e8-807a-7b0007981048", hco3 = "26"), by = "id_visita") %>% # was 2
  rows_update(tibble(id_visita = "5ebd2a1e-4945-11e8-a423-f10b658f4c3d", ct90 = "6"), by = "id_visita") %>% # was 90.6
  rows_update(tibble(id_visita = "2e051a84-837e-11e9-9ede-f367b00f4244", ct90 = "3"), by = "id_visita") %>% # was 90.3
  rows_update(tibble(id_visita = "f53b17fe-837c-11e9-9ede-f367b00f4244", ct90 = "0"), by = "id_visita") %>% # was 90.0
  rows_update(tibble(id_visita = "3931c054-8380-11e9-9ede-f367b00f4244", ct90 = "1"), by = "id_visita") %>% # was 90.1
  rows_update(tibble(id_visita = "008a5ad4-8382-11e9-9ede-f367b00f4244", ct90 = "5"), by = "id_visita") %>% # was 90.5
  rows_update(tibble(id_visita = "9158e8d6-dc44-11e8-87ef-efc75e1e34cc", paco2 = "41"), by = "id_visita") %>% # was 4
  rows_update(tibble(
    id_visita = "3d3bfffa-3120-11e9-80d3-0938853a2539",
    ph_sangre_arterial = NA, pao2 = NA, paco2 = NA, # abg not performed
    sao2_media = "91", ct90 = "24", odi3 = "10" # values were wrong
  ), by = "id_visita") %>%
  rows_update(tibble(id_visita = "acef89f8-47c8-11e8-a7d0-b52d1edb9069", pao2 = "109", paco2 = "36"), by = "id_visita") %>% # values were wrong
  rows_update(tibble(id_visita = "56e5a8d0-2acb-11e9-8d8b-6bd4a449ee38", pao2 = "93", paco2 = "34"), by = "id_visita") %>% # values were wrong
  rows_update(tibble(id_visita = "afa0cef0-2acb-11e9-8d8b-6bd4a449ee38", pao2 = "92", paco2 = "36"), by = "id_visita") %>% # values were wrong
  rows_update(tibble(id_visita = "052f693e-0605-11ea-89b0-17e7b5a20645", sao2_media = "93", ct90 = "3"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "03bb7c9e-838a-11e9-9ede-f367b00f4244", pao2 = "81", paco2 = "43"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "cc79f9d2-52c9-11e8-880e-7fe41889cc89", fvc_sentado = "39", fvc_sentado_absoluto = "1420"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "ec7f50e6-1dca-11eb-bb4a-9745fc970131", ct90 = "95.8"), by = "id_visita") %>% # was 98.8
  rows_update(tibble(id_visita = "943071c6-ea12-11eb-8de9-5ff3128bc34d", sao2_media = NA, ct90 = NA, odi3 = NA), by = "id_visita") %>% # values were from previous npo
  rows_update(tibble(id_visita = "77223818-305f-11e9-9393-2b87c41589f5", pao2 = "90", paco2 = "34"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "7eeb125e-89bb-11e8-9a8f-6ba51261727f", hco3 = "31"), by = "id_visita") %>% # was 3
  rows_update(tibble(id_visita = "d3b5bcf8-5dc9-11e8-9ce8-cf38f9497e56", pao2 = "73", paco2 = "41", hco3 = "27"), by = "id_visita") %>% # pao2/paco2 values were flipped, hco3 was missing
  rows_update(tibble(id_visita = "fe6dd472-d095-11e9-961d-d3f87ec1e7e1", ph_sangre_arterial = "7.43", pao2 = "85", paco2 = "45"), by = "id_visita") %>% # pao2/paco2 values were flipped, hco3 was missing
  rows_update(tibble(id_visita = "d9ecbe46-0a99-11e9-9997-9759725d7b76", ph_sangre_arterial = "7.47", pao2 = "101", paco2 = "42", hco3 = "31"), by = "id_visita") %>% # values were wrong
  rows_update(tibble(id_visita = "35dc5b76-0a9a-11e9-9997-9759725d7b76", hco3 = "29"), by = "id_visita") %>% # was 2
  rows_update(tibble(
    id_visita = "9c2051c6-0a9a-11e9-9997-9759725d7b76",
    pao2 = NA, paco2 = NA, hco3 = NA, # abg not performed
    sao2_media = "94.8", ct90 = "0.8", odi3 = "7" # values were flipped
  ), by = "id_visita") %>%
  rows_update(tibble(id_visita = "72cb1a14-12a4-11ea-badc-8b5db01dc70c", sao2_media = "92", ct90 = "17", odi3 = "27"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "c7fb0ae6-e06f-11ea-87ad-6151be442ee5", sao2_media = "93"), by = "id_visita") %>% # was 963
  rows_update(tibble(id_visita = "92575848-fafd-11e9-b780-81f732616a71", hco3 = "29"), by = "id_visita") %>% # was 47
  rows_update(tibble(id_visita = "cf16f606-b672-11e8-9abc-77a17ade9338", pao2 = "103", paco2 = "38", hco3 = "24"), by = "id_visita") %>% # pao2/paco2 were flipped, hco3 was missing
  rows_update(tibble(id_visita = "6ed7a648-b675-11e8-9abc-77a17ade9338", ct90 = "0.96"), by = "id_visita") %>% # was 93
  rows_update(tibble(id_visita = "a61a432c-ecdf-11e8-9848-4918586d805c", ct90 = "11"), by = "id_visita") %>% # was 171
  rows_update(tibble(id_visita = "57ddc50c-ece0-11e8-9848-4918586d805c", hco3 = "24"), by = "id_visita") %>% # was 247
  rows_update(tibble(id_visita = "20405d4a-fb07-11e9-b780-81f732616a71", pcf = "350", pns = NA), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "e18a485c-a5f5-11e8-b217-99d3df7458ce", sao2_media = "93.8", ct90 = "0.21"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "6112d056-883d-11e9-a996-85cf5f8a7da8", fvc_estirado = "86"), by = "id_visita") %>% # was 400
  rows_update(tibble(id_visita = "f4c4b0b8-81be-11eb-bdaf-35917006ea01", ct90 = NA, sao2_media = NA), by = "id_visita") %>% # not performed
  rows_update(tibble(
    id_visita = "62ee3b20-68ce-11e8-8bba-011ae3e18648",
    ph_sangre_arterial = NA, sao2_media = "95.35", # values were flipped
    ct90 = "3.17", odi3 = "10", # values were missing
    pao2 = NA, paco2 = NA, hco3 = NA # abg not performed
  ), by = "id_visita") %>%
  rows_update(tibble(id_visita = "921815ee-eb6f-11ea-8eab-918bb380203d", fvc_sentado_absoluto = "3310"), by = "id_visita") %>% # was 33310
  rows_update(tibble(id_visita = "4308f4ec-d272-11eb-b08c-851f5b5d129e", fvc_sentado_absoluto = "5420"), by = "id_visita") %>% # was 54200
  rows_update(tibble(id_visita = "d2755b2a-91b0-11e9-8658-35a5d3539e15", hco3 = "30", paco2 = "41"), by = "id_visita") %>% # wrong values
  rows_update(tibble(id_visita = "f1e2bc96-fef7-11e9-b2bc-2bd7e660e45b", hco3 = "23"), by = "id_visita") %>% # was 213
  rows_update(tibble(id_visita = "ccf8c196-22e4-11e8-bd92-937d2f7100b4", ct90 = NA), by = "id_visita") %>% # contained sao2_media
  rows_update(tibble(id_visita = "c5a16d28-1332-11e9-8be2-e38b9cc14280", pao2 = "81", paco2 = "42"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "70746d6c-9a35-11e8-b94f-4f39f1d733d1", hco3 = "27"), by = "id_visita") %>% # was 72
  rows_update(tibble(id_visita = "cdc2459c-9a36-11e8-b94f-4f39f1d733d1", pim = "86"), by = "id_visita") %>% # was 183
  rows_update(tibble(id_visita = "09d93355-45d1-11eb-ab70-71abb58c311a", pim = NA, pem = NA), by = "id_visita") %>% # data not found
  rows_update(tibble(id_visita = "12d3a6e0-138d-11eb-a114-cddc6e0437ad", ct90 = "1.8"), by = "id_visita") %>% # was 138
  rows_update(tibble(id_visita = "ab8aaa2a-53f5-11eb-a974-9774facb76d4", ct90 = "0"), by = "id_visita") %>% # was 90
  rows_update(tibble(id_visita = "b10e0910-f347-11ea-9794-a9e1a185db06", ct90 = "0"), by = "id_visita") %>% # was 90
  rows_update(tibble(id_visita = "62b28c1a-7099-11e8-bffa-fbbf07f4b8c5", pao2 = "94", paco2 = "39"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "20b0cd86-e695-11e9-9a0f-0fdab8b37fe7", pao2 = NA, paco2 = NA), by = "id_visita") %>% # data not found
  rows_update(tibble(id_visita = "b31ada4e-838b-11e9-9ede-f367b00f4244", pao2 = "127", paco2 = "40"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "7f99fe5c-e073-11ea-87ad-6151be442ee5", sao2_media = "93.2", ct90 = "0.8"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "f996787c-aad1-4e66-8335-3964ca55aa7d", pns = NA, pcf = "440"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "3cc517ba-311f-11e9-80d3-0938853a2539", pns = NA, pcf = "185"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "e83a3ad6-b667-11e8-9abc-77a17ade9338", pns = NA, pcf = "450"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "cfbdc066-05fa-11ea-89b0-17e7b5a20645", ct90 = "3.8"), by = "id_visita") %>% # was 90.3
  rows_update(tibble(id_visita = "9b4ef030-cee2-11e7-8d7f-c9c4ce89f9c5", ct90 = "0"), by = "id_visita") %>% # was 90
  rows_update(tibble(id_visita = "a5e3dcc4-d707-11ea-bb6b-31ec986d6c42", ct90 = "0"), by = "id_visita") %>% # was 90
  rows_update(tibble(id_visita = "14c6782e-d5ce-11ec-ab47-f14c5265e448", ct90 = "0.6", sao2_media = "95.8", odi3 = "8"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "a74f9472-c3dc-11e7-b405-7fa326ba76f2", odi3 = "5", ct90 = "1", sao2_media = "93"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "16f87e1c-d7ce-11ea-8786-6f85542dc338", odi3 = "5.8", sao2_media = "94.27", ct90 = "0.26"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "ae050874-8c49-11e9-8c23-a5c3d8474f8f", pao2 = "97", paco2 = "42"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "6913089e-ee98-11ea-aa63-851e06783497", pao2 = "110", paco2 = "39"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "79d5c1ea-4450-11eb-82b7-377ffbad2e56", paco2 = "46", hco3 = "28"), by = "id_visita") %>% # paco2 contained pao2, hco3 was missing
  rows_update(tibble(id_visita = "cd4ba262-2554-11e9-9820-09ea91f975fa", pao2 = "109", paco2 = "36"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "6e20d426-6333-11e8-8914-9b10f80dedd0", pao2 = "97", paco2 = "41"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "b7b20a44-e782-11ea-ba91-5d9fa569562f", ph_sangre_arterial = "7.44"), by = "id_visita") %>% # was 744
  rows_update(tibble(
    id_visita = "91cad63e-df08-11eb-876d-61d8cc6ff0fe",
    fvc_sentado = "103", # fvc_sentado contained FEV1
    fvc_sentado_absoluto = "5040", # fvc_sentado_abs contained fvc_sentado
    pcf = NA # pcf contained fvc_sentado_abs
  ), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "2ef0705c-2fe3-11eb-a414-cdc7c2599e4f",
    fvc_sentado = "98", # fvc_sentado contained absolute FEV1
    fvc_sentado_absoluto = "3330", # fvc_sentado_abs contained fvc_sentado
    fvc_estirado = NA, # fvc_estirado was missing
    fvc_estirado_absoluto = NA, # fvc_estirado_abs contained %FEV1
    pcf = NA, # pcf contained fvc_sentado_abs
  ), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "92553af8-5dcf-11e8-9ce8-cf38f9497e56",
    pao2 = "94", paco2 = "35", # values were flipped
    hco3 = "22" # was missing
  ), by = "id_visita") %>%
  rows_update(tibble(id_visita = "f493676c-ecf7-11ea-9b9d-9d9bdb02a59a", pao2 = "104", paco2 = "40"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "0b34cdbc-0f9e-11eb-9cdd-5b494bd8f088", paco2 = "37"), by = "id_visita") %>% # contained pao2
  rows_update(tibble(id_visita = "2617105e-221b-11ed-9e37-37dec3ae1adb", sao2_media = "91.78"), by = "id_visita") %>% # was 9178
  rows_update(tibble(id_visita = "20ce0a56-3df9-11eb-9383-d3a3b2195eff", pns = "64", pcf = "300"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "56cef280-12c2-11eb-bc61-6193a81164b0", pao2 = "101", paco2 = "43"), by = "id_visita") %>% # values were flipped
  rows_update(tibble(id_visita = "3681751c-fb7c-11ec-9f93-8170d5a8e848", paco2 = "37"), by = "id_visita") %>% # was 85
  rows_update(tibble(id_visita = "97728d44-838d-11e9-9ede-f367b00f4244", pao2 = "108"), by = "id_visita") %>% # was 47108
  rows_update(tibble(
    id_visita = "f3caa9f2-e4db-11e7-8222-4fcf3b7eb7b9",
    ph_sangre_arterial = "7.43", # was 96.2
    pao2 = "98", # contained paco2
    hco3 = "26" # was missing
  ), by = "id_visita") %>%
  rows_update(tibble(id_visita = "91019da8-4a56-11e9-9f98-cd726d473012", ph_sangre_arterial = "7.45"), by = "id_visita") %>% # was 45
  rows_update(tibble(id_visita = "0c57cd06-d936-11e9-87ad-ad3b45e595ef", pns = "28", pcf = "300"), by = "id_visita") %>% # values were flipped
  mutate(
    across(!c(ends_with("_cual"), cumplimiento_cpap), \(x) ufmn_parse_na(x, na_empty = TRUE)),
    across(starts_with("fecha"), ufmn_parse_date),
    across(c(
      starts_with("sintomas_"),
      starts_with("tipo_patologia_respiratoria_")
    ), ufmn_parse_logical, true = "TRUE", false = "FALSE"),
    across(c(
      patologia_respiratoria_previa,
      cpap,
      indicacion_vmni,
      portador_vmni,
      retirada_vmni,
      polisomnografia,
      complicacion_vmni,
    ), \(x) {
      ufmn_parse_logical(x, true = "Sí", false = "No")
    }),
    pcf_por_debajo_del_umbral = str_starts(pcf, "<"),
    pim_por_debajo_del_umbral = str_starts(pim, "<"),
    sao2_media_por_debajo_del_umbral = str_starts(sao2_media, "<"),
    across(c(
      pns, pcf, fvc_sentado, fvc_estirado, pim, pem, ph_sangre_arterial,
      pao2, paco2, hco3, sao2_media, ct90, odi3, ct90_polisomnografia, iah,
      fvc_sentado_absoluto, fvc_estirado_absoluto
    ), parse_number)
  ) %>%
  select(!c(created_datetime:updated_datetime)) %>%
  arrange(id_paciente, fecha_visita)

ufmn_functional <- DBI::dbReadTable(ufmn_db, "esc_val_ela") %>%
  rename(
    id_visita = id,
    id_paciente = pid,
    fecha_visita = fecha_visita_esc_val_ela,
    insuf_resp = insuficiencia_respiratoria,
    kings_r = kings
  ) %>%
  rows_reset(tibble(id_visita = "c762bfca-df50-11e7-913f-898db1444b28"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "7824e0ae-0b1b-11e8-ada5-c11bb16fd5d7"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "d950b7be-be53-11e7-854b-69bf7a307972"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "52a010f0-0b53-11e8-a739-3927728b190b"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "c490d08e-22e4-11e8-bd92-937d2f7100b4"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "b98eb8de-0b12-11e8-ada5-c11bb16fd5d7"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "31775b3a-df53-11e7-913f-898db1444b28"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "3290366e-0b17-11e8-ada5-c11bb16fd5d7"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "482ed0f2-22e6-11e8-bd92-937d2f7100b4"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "1e1b0230-bfaf-11e7-8973-f909c4af69c9"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "7b222648-e4d0-11e7-8222-4fcf3b7eb7b9"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "d48b24a2-e0cd-11e7-a41d-951fbf0a64ef"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "6216659c-4431-11eb-8819-1948421e8c8d"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "081ccd70-3c08-11e9-bacb-89acb4d69104"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "1142f7a0-8f63-11e8-90a3-ff2b0bfabaa2"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "277837de-208a-11e9-9341-45e3ce47fe90"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "664c7d1e-ee90-11ea-aa63-851e06783497"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "9e46525a-b305-11eb-96a7-27cdb1c6efb1"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "f877112a-59b2-11ec-97be-b1dc88f020df"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "7d33fa84-edbe-11ea-b1c3-e5404a292065"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "0418da4a-df50-11e7-913f-898db1444b28"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "40573a56-22e4-11e8-bd92-937d2f7100b4"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "a0a3bff2-e2cb-11ea-b03f-a7e8f227e03a"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "e9663f4a-df4e-11e7-913f-898db1444b28"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "de1f1d4a-22e3-11e8-bd92-937d2f7100b4"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "66532790-be5f-11e7-854b-69bf7a307972"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "6ce47da4-d99c-11e7-b681-6585f92f3d5c"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "5f4bc604-163a-11e8-a95d-b7c3df0bafe2"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "8025974c-163a-11e8-a95d-b7c3df0bafe2"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "89d6f9ac-163a-11e8-a95d-b7c3df0bafe2"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "c3c10f66-bfca-11e7-af09-51e9d3377771"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "f5997e1c-163a-11e8-a95d-b7c3df0bafe2"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "e9663f4a-df4e-11e7-913f-898db1444b28"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "8356b516-3df3-11eb-9383-d3a3b2195eff"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "c0792408-2b4d-11e8-a24c-e9b93d7ec40a"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "2b5c8a1c-1d70-11e8-824e-4f42ed25e7a5"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "31acd35a-22e2-11e8-bd92-937d2f7100b4"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "da5aa432-22e2-11e8-bd92-937d2f7100b4"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "5e969c7e-22e3-11e8-bd92-937d2f7100b4"), by = "id_visita") %>%
  rows_reset(tibble(id_visita = "de1f1d4a-22e3-11e8-bd92-937d2f7100b4"), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "3318ad62-e4db-11e7-8222-4fcf3b7eb7b9",
    # ALSFRS 4-4-4-2-1-2-3-3-3-4-4-4
    lenguaje = "4", salivacion = "4", deglucion = "4",
    escritura = "2", cortar_con_peg = "1", cortar_sin_peg = "1", vestido = "2",
    cama = "3", caminar = "3", subir_escaleras = "3",
    disnea = "4", ortopnea = "4", insuf_resp = "4"
  ), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "930344b6-9a30-11e8-9eb2-af099be7b978",
    # ALSFRS 3-3-3-come sola-corta con ayuda-se vista y asea sola lenta-anda con ayuda-
    # sube escaleras con ayuda-disnea reposo-no ortopnea-no ventilada
    lenguaje = "3", salivacion = "3", deglucion = "3",
    escritura = NA, cortar_con_peg = NA, cortar_sin_peg = "4", vestido = "3",
    cama = NA, caminar = "2", subir_escaleras = "1",
    disnea = "1", ortopnea = "4", insuf_resp = "4"
  ), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "14a1b498-ce8c-11eb-afd7-ed6fcecd9429",
    # ALSFRS 4-4-4-4-4-4-4-2-1-4-4-4
    lenguaje = "4", salivacion = "4", deglucion = "4",
    escritura = "4", cortar_con_peg = "4", cortar_sin_peg = "4", vestido = "4",
    cama = "4", caminar = "2", subir_escaleras = "1",
    disnea = "4", ortopnea = "4", insuf_resp = "4"
  ), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "bdbd9d42-98c1-11e9-9feb-8dcf40d9fa45",
    # ALSFRS 0-3-1-2-1-2-3-2-1-4-4-4
    lenguaje = "0", salivacion = "3", deglucion = "1",
    escritura = "2", cortar_con_peg = "1", cortar_sin_peg = "1", vestido = "2",
    cama = "3", caminar = "2", subir_escaleras = "1",
    disnea = "4", ortopnea = "4", insuf_resp = "4"
  ), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "c39cd9ec-12a3-11ea-badc-8b5db01dc70c",
    # ALSFRS 0-3-1-2-1-2-3-2-1-2-0-0
    lenguaje = "0", salivacion = "3", deglucion = "1",
    escritura = "2", cortar_con_peg = "1", cortar_sin_peg = "1", vestido = "2",
    cama = "3", caminar = "2", subir_escaleras = "1",
    disnea = "2", ortopnea = "0", insuf_resp = "0"
  ), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "796374aa-ee9c-11ea-aa63-851e06783497",
    # ALSFRS 4-4-4-2-0-0-0-0-0-3-4-2
    lenguaje = "4", salivacion = "4", deglucion = "4",
    escritura = "2", cortar_con_peg = "0", cortar_sin_peg = "0", vestido = "0",
    cama = "0", caminar = "0", subir_escaleras = "0",
    disnea = "3", ortopnea = "4", insuf_resp = "2"
  ), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "1a485a74-df68-11e9-9c30-274ab37b3217",
    # ALSFRS: habla 3, salivacion 3, deglucion 3, comer 0, escribir 0, higiene 0,
    # girarse cama 0, deambular 0, escaleras 0, no disnea ni ortopnea pero VMNI nocturna
    lenguaje = "3", salivacion = "3", deglucion = "3",
    escritura = "0", cortar_con_peg = "0", cortar_sin_peg = "0", vestido = "0",
    cama = "0", caminar = "0", subir_escaleras = "0",
    disnea = "0", ortopnea = "0", insuf_resp = "2"
  ), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "0aded8ac-4eaa-11ec-886f-6f0859135454",
    # ALSFRS 3-3-3-0-0-0-1-0-0-2-0-4
    lenguaje = "3", salivacion = "3", deglucion = "3",
    escritura = "0", cortar_con_peg = "0", cortar_sin_peg = "0", vestido = "0",
    cama = "1", caminar = "0", subir_escaleras = "0",
    disnea = "2", ortopnea = "0", insuf_resp = "4"
  ), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "cfd1d762-52c9-11e8-880e-7fe41889cc89",
    # ALSFRS 4-4-3-3-3-2-3-2-1-0-0-1
    lenguaje = "4", salivacion = "4", deglucion = "3",
    escritura = "3", cortar_con_peg = "3", cortar_sin_peg = "3", vestido = "2",
    cama = "3", caminar = "2", subir_escaleras = "1",
    disnea = "0", ortopnea = "0", insuf_resp = "1"
  ), by = "id_visita") %>%
  rows_update(tibble(
    id_visita = "1a485a74-df68-11e9-9c30-274ab37b3217",
    # ALSFRS habla 3, salivacion 3, deglucion 3, comer 0, escribir 0, higiene 0,
    # girarse cama 0, deambular 0, escaleras 0, no disnea ni ortopnea pero VNIV nocturna (0-0-2)
    lenguaje = "3", salivacion = "3", deglucion = "3",
    escritura = "0", cortar_con_peg = "0", cortar_sin_peg = "0", vestido = "0",
    cama = "0", caminar = "0", subir_escaleras = "0",
    disnea = "0", ortopnea = "0", insuf_resp = "2"
  ), by = "id_visita") %>%
  mutate(
    across(everything(), ufmn_parse_na),
    across(lenguaje:insuf_resp, parse_integer),
    fecha_visita = ufmn_parse_date(fecha_visita)
  ) %>%
  select(!c(total:total_bulbar, mitos, created_datetime:updated_datetime)) %>%
  arrange(id_paciente, fecha_visita)

ufmn_followups <- bind_rows(
  ufmn_functional |>
    mutate(tipo_visita = "functional"),
  ufmn_nutrition |>
    select(id_paciente, id_visita, fecha_visita, indicacion_peg, portador_peg) |>
    mutate(tipo_visita = "nutrition"),
  ufmn_respiratory |>
    select(id_paciente, id_visita, fecha_visita) |>
    mutate(tipo_visita = "respiratory")
) |>
  drop_na(id_paciente, id_visita, fecha_visita) |>
  group_by(id_paciente) |>
  arrange(fecha_visita, .by_group = TRUE) |>
  fill() |>
  ungroup() |>
  mutate(
    cortar = case_when(
      portador_peg == TRUE ~ cortar_con_peg,
      portador_peg == FALSE ~ cortar_sin_peg,
      cortar_con_peg == cortar_sin_peg ~ cortar_con_peg,
      is.na(cortar_con_peg) & !is.na(cortar_sin_peg) ~ cortar_sin_peg,
      is.na(cortar_sin_peg) & !is.na(cortar_con_peg) ~ cortar_con_peg,
      cortar_con_peg != 0 & cortar_sin_peg == 0 ~ cortar_con_peg,
      cortar_sin_peg != 0 & cortar_con_peg == 0 ~ cortar_sin_peg,
    ),
    kings_c = case_when(
      disnea == 0 | insuf_resp < 4 ~ "4B",
      indicacion_peg == TRUE ~ "4A",
      indicacion_peg == FALSE ~ {
        bulbar <- any(c(lenguaje, salivacion, deglucion) < 4)
        upper <- any(c(escritura, cortar_sin_peg) < 4)
        lower <- caminar < 4
        as.character(bulbar + upper + lower)
      }
    ),
    mitos = {
      walking_selfcare <- caminar <= 1 | vestido <= 1
      swallowing <- deglucion <= 1
      communication <- lenguaje <= 1 | escritura <= 1
      breathing <- disnea <= 1 | insuf_resp <= 2
      walking_selfcare + swallowing + communication + breathing
    }
  ) %>%
  select(id_paciente, id_visita, fecha_visita, tipo_visita, cortar, kings_c, mitos)

ufmn_functional %<>%
  left_join(
    ufmn_followups |>
      filter(tipo_visita == "functional") |>
      select(id_visita, cortar, kings_c, mitos),
    by = "id_visita", multiple = "all"
  ) %>%
  mutate(
    cortar_con_peg = ifelse(cortar == cortar_con_peg, cortar_con_peg, NA),
    cortar_sin_peg = ifelse(cortar == cortar_sin_peg, cortar_sin_peg, NA)
  ) %>%
  rowwise() %>%
  mutate(
    alsfrs_bulbar = sum(c_across(lenguaje:deglucion)),
    alsfrs_motor_fino = sum(c_across(escritura:vestido), na.rm = TRUE),
    alsfrs_motor_grosero = sum(c_across(cama:subir_escaleras)),
    alsfrs_respiratorio = sum(c_across(disnea:insuf_resp)),
    alsfrs_total = sum(c_across(lenguaje:insuf_resp), na.rm = TRUE)
  ) %>%
  filter(!if_all(lenguaje:insuf_resp, is.na)) %>%
  relocate(cortar, .before = cortar_sin_peg) %>%
  relocate(kings_r:mitos, .after = alsfrs_total)

DBI::dbDisconnect(ufmn_db)
