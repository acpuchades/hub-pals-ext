library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)

sectecnica_data_path <- "data/sectecnica-2022_03_15.xlsx"

sectecnica_hosp_sheet <- "TAULA HOSP"
sectecnica_hosp_skip <- 2

sectecnica_urg_sheet <- "taula urg"
sectecnica_urg_skip <- 3

sectecnica_parse_discharge_type <- function(data) {
  data %>%
    recode(
      `ALTA VOLUNTARIA` = "Alta voluntaria",
      `FUGIDA/ABANDONAMENT` = "Fuga",
      `ALTA A DOMICILI` = "Domicilio",
      `REMISIO A ATENCIO PRIMARIA` = "Domicilio",
      `A DOMICILI` = "Domicilio",
      `RESID. SOCIAL` = "Domicilio",
      ICO = "Traslado",
      `ALTA CONT.CENTR` = "Traslado",
      `DERIVACIO A UN ALTRE CENTRE` = "Traslado",
      `AGUTS/PSIQUIATRIC` = "Traslado",
      `H. DOMICILARIA` = "Hosp. domiciliaria",
      `NO VISITATS` = "No atendido",
      `SOCI SANITARI` = "Sociosanitario",
      `INGRES A L'HOSPITAL` = "Ingreso",
      EXITUS = "Exitus"
    ) %>%
    factor()
}

sectecnica_hosp <-
  read_excel(
    sectecnica_data_path,
    sheet = sectecnica_hosp_sheet,
    skip = sectecnica_hosp_skip,
    na = "N/D"
  ) %>%
  select(!`Marca diagnòstic principal`) %>%
  rename(
    nhc = `Pacient (NHC)`,
    episodio = `Episodi`,
    fecha_ingreso = `Data ingres`,
    fecha_alta = `Data hora alta`,
    cod_diagnostico = `Diagnòstic codi`,
    desc_diagnostico = `Diagnòstic desc`,
    destino_al_alta = `Classe alta desc`,
    centro_destino_al_alta = `Centre destí alta desc`,
    servicio_alta = `Servei alta desc`
  ) %>%
  fill(nhc, episodio) %>%
  mutate(
    across(starts_with("fecha"), ymd_hms),
    destino_al_alta = sectecnica_parse_discharge_type(destino_al_alta),
    codif_diagnostico =
      case_when(
        year(fecha_ingreso) < 2018 ~ "ICD-9",
        TRUE ~ "ICD-10"
      )
  ) %>%
  relocate(codif_diagnostico, .before = cod_diagnostico) %>%
  rows_delete(tibble(nhc = "Total general"), by = "nhc") %>%
  mutate(across(nhc, parse_integer)) %>%
  arrange(nhc, fecha_ingreso)

sectecnica_urg <- read_excel(
  sectecnica_data_path,
  sheet = sectecnica_urg_sheet,
  skip = sectecnica_urg_skip,
  na = "N/D"
) %>%
  select(`Pacient (NHC)`:`Centre destí desc`) %>%
  rename(
    nhc = `Pacient (NHC)`,
    episodio = Episodi,
    fecha_entrada = `Data hora entrada`,
    fecha_salida = `Data hora sortida`,
    cod_diagnostico = `Diagnòstic codi`,
    desc_diagnostico = `Diagnòstic descripció`,
    destino_al_alta = `Classe fi episodi desc`,
    centro_destino_al_alta = `Centre destí desc`,
  ) %>%
  fill(nhc, episodio, fecha_entrada, fecha_salida) %>%
  mutate(
    across(starts_with("fecha"), ymd_hms),
    across(
      c(cod_diagnostico, desc_diagnostico),
      ~ .x %>%
        na_if("(en blanc)") %>%
        na_if("(en blanco)")
    ),
    destino_al_alta = sectecnica_parse_discharge_type(destino_al_alta),
    codif_diagnostico =
      case_when(
        is.na(cod_diagnostico) ~ NA_character_,
        year(fecha_entrada) < 2018 ~ "ICD-9",
        TRUE ~ "ICD-10"
      )
  ) %>%
  relocate(codif_diagnostico, .before = cod_diagnostico) %>%
  rows_delete(tibble(nhc = "Total general"), by = "nhc") %>%
  mutate(across(nhc, parse_integer)) %>%
  arrange(nhc, fecha_entrada)

sectecnica_urg_episodios <- sectecnica_urg %>%
  group_by(episodio) %>%
  select(!ends_with("diagnostico"))

sectecnica_urg_diagnosticos <- sectecnica_urg %>%
  select(c(episodio, ends_with("diagnostico")))
