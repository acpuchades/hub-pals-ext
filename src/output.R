pals_version <- "v1"

source("src/precisionals.R")

output_dir <- str_glue("output/{pals_version}")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

exports <- list(
    "alsfrs_r" = pals_alsfrs,
    "comorbidities" = pals_comorbidities,
    "er_diagnoses" = pals_er_diagnoses,
    "er_episodes" = pals_er_episodes,
    "genesets" = pals_genesets,
    "genetics" = pals_genetics,
    "genetics_ext" = pals_genetics_ext,
    "hospitalizations" = pals_hosp,
    "gpvisits" = pals_gpvisits,
    "gpvars" = pals_gpvars,
    "nutrition" = pals_nutrition,
    "patients" = pals_patients,
    "respiratory" = pals_respiratory,
    "social" = pals_social,
    "treatments" = pals_treatments
)

for (key in names(exports)) {
    data <- exports[[key]]
    output_name <- str_glue("pals-{key}.csv")
    path <- file.path(output_dir, output_name)
    write_csv(data, path)
}
