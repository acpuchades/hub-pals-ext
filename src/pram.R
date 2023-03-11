library(dplyr)
library(stringr)
library(readxl)

pram_data_path <- "data/pram-2022_12_09.xlsx"

pram_cases <- read_excel(pram_data_path) %>%
    select(
        expediente = "Número expedient",
        fecha = "Data expedient",
        estado = "Estat",
        fecha_informe1 = "Data informe 1r Doc",
        fecha_informe2 = "Data informe 2n Doc",
        nombre = "Nom Sol·licitant",
        apellido1 = "Primer Cognom Sol·licitant",
        apellido2 = "Segon Cognom Sol·licitant",
        nombre_mr = "Nom Metge Responsable",
        apellido1_mr = "Primer Cognom Metge Responsable",
        datos_paciente = "Introduir aquí el CIP del Sol·licitant",
        apellido2_mr = "Segon Cognom Metge Responsable",
        especialidad_mr = "Especialitat Metge Responsable",
        centro_mr = "Centre Sanitàri Metge Responsable",
        direccion_centro_mr = "Adreça del Centre Sanitari Metge Responsable",
        municipio_centro_mr = "Municipi Centre MR",
        fecha_primera_solicitud = "Data de presentació Primera Sol·licitud",
        centro_mc = "Centre Sanitàri Metge Consultor *",
        posicionamiento_mr = "Posicionament MR",
        posicionamiento_mc = "Posicionament MC",
        posicionamiento_dupla = "Posicionament dupla",
        posicionamiento_pleno = "Posicionament Ple",
        codigo_dx = "Codi CIM-10",
        descripcion_dx = "Descripció CIM10"
    ) %>%
    mutate(
        cip = str_extract(datos_paciente, R"([A-Z]{4}\d{10})"),
        codificacion_dx = "ICD-10",
        .keep = "unused"
    )
