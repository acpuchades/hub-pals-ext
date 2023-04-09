library(readxl)

vmi_data_path <- "data/vmi-20230409.xlsx"

vmi_data <- read_excel(vmi_data_path) |>
    mutate(across(fecha_inicio_vmi, as.Date))
