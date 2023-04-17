library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

dataset_dir_a <- "output/v1"
dataset_dir_b <- "output/v2"

data_files_a <- list(
    alsfrs = "precisionals-alsfrs_r.csv",
    comorbidities = "precisionals-comorbidities.csv",
    er_diagnoses = "precisionals-er_diagnoses.csv",
    er_episodes = "precisionals-er_episodes.csv",
    genesets = "precisionals-genesets.csv",
    genetics_ext = "precisionals-genetics_ext.csv",
    genetics = "precisionals-genetics.csv",
    # gpvars = "precisionals-gpvars.csv",
    # gpvisits = "precisionals-gpvisits.csv",
    hospitalizations = "precisionals-hospitalizations.csv",
    nutrition = "precisionals-nutrition.csv",
    patients = "precisionals-patients.csv",
    respiratory = "precisionals-respiratory.csv",
    social = "precisionals-social.csv"
    # treatments = "precisionals-treatments.csv"
)

data_files_b <- list(
    alsfrs = "pals-alsfrs.csv",
    comorbidities = "pals-comorbidities.csv",
    er_diagnoses = "pals-er_diagnoses.csv",
    er_episodes = "pals-er_episodes.csv",
    genesets = "pals-genesets.csv",
    genetics_ext = "pals-genetics_ext.csv",
    genetics = "pals-genetics.csv",
    # gpvars = "pals-gpvars.csv",
    # gpvisits = "pals-gpvisits.csv",
    hospitalizations = "pals-hospitalizations.csv",
    imv = "pals-imv.csv",
    nutrition = "pals-nutrition.csv",
    patients = "pals-patients.csv",
    respiratory = "pals-respiratory.csv",
    social = "pals-social.csv"
    # treatments = "pals-treatments.csv"
)

file_indexes <- list(
    alsfrs = c("patient_id", "assessment_date"),
    comorbidities = c("patient_id", "dx_date", "dx_code"),
    er_diagnoses = c("episode_id", "dx_code"),
    er_episodes = "episode_id",
    genesets = c("geneset", "gene"),
    genetics_ext = c("hospital_id", "geneset", "gene"),
    genetics = "patient_id",
    gpvars = c("patient_id", "date", "var_name"),
    gpvisits = c("patient_id", "date"),
    hospitalizations = "episode_id",
    imv = "patient_id",
    nutrition = c("patient_id", "assessment_date"),
    patients = "patient_id",
    respiratory = c("patient_id", "assessment_date"),
    social = "patient_id",
    treatments = c("patient_id", "product_code")
)

load_data_files <- function(dir_path, files) {
    result <- list()
    for (key in names(files)) {
        name <- files[[key]]
        path <- file.path(dir_path, name)
        data <- read_csv(path, col_types = "c")
        result[[key]] <- data
    }
    result
}

diff_data_files <- function(data_a, data_b, index) {
    if (!is.null(data_a)) {
        entries_a <- data_a |> pivot_longer(
            -index,
            names_to = "field",
            values_to = "old_value",
            values_transform = as.character
        )
    }

    if (!is.null(data_b)) {
        entries_b <- data_b |> pivot_longer(
            -index,
            names_to = "field",
            values_to = "new_value",
            values_transform = as.character
        )
    }

    if (is.null(data_a)) {
        return(data_b |> mutate(action = "added"))
    }

    if (is.null(data_b)) {
        return(data_a |> mutate(action = "removed"))
    }

    full_join(
        entries_a, entries_b,
        by = c(index, "field"),
        relationship = "many-to-many"
    ) |>
        filter(
            is.na(old_value) & !is.na(new_value) |
                !is.na(old_value) & is.na(new_value) |
                old_value != new_value
        ) |>
        mutate(
            action = case_when(
                is.na(old_value) ~ "added",
                is.na(new_value) ~ "removed",
                TRUE ~ "updated"
            )
        )
}

diff_datasets <- function(dataset_a, dataset_b, indexes) {
    result <- list()
    keys <- union(names(dataset_a), names(dataset_b))
    for (k in keys) {
        data_a <- if (exists(k, dataset_a)) dataset_a[[k]] else NULL
        data_b <- if (exists(k, dataset_b)) dataset_b[[k]] else NULL
        result[[k]] <- diff_data_files(data_a, data_b, indexes[[k]])
    }
    result
}

dataset_a <- load_data_files(dataset_dir_a, data_files_a)
dataset_b <- load_data_files(dataset_dir_b, data_files_b)
data_diff <- diff_datasets(dataset_a, dataset_b, file_indexes)

dataset_name_a <- basename(dataset_dir_a)
dataset_name_b <- basename(dataset_dir_b)
output_name <- str_glue("{dataset_name_a}-{dataset_name_b}-diff.xlsx")
output_path <- file.path("output", output_name)

dir.create("output", showWarnings = FALSE)
write_xlsx(data_diff, output_path)
