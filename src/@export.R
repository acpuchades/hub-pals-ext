library(xfun)

source("src/precisionals.r")
source("src/qc/alsfrs-increases.r")
source("src/qc/followup-dates.r")
source("src/qc/numeric-outliers.r")

exports <- list(
    "alsfrs" = pals_alsfrs,
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

qc_exports <- list(
    "alsfrs-increases" = alsfrs_increases,
    "followups-before-onset" = followups_before_onset,
    "followups-after-death" = followups_after_death,
    "respiratory-outliers" = respiratory_outliers,
    "nutrition-outliers" = nutrition_outliers
)

output_dir <- "output/current"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (key in names(exports)) {
    data <- exports[[key]]
    output_name <- str_glue("pals-{key}.csv")
    path <- file.path(output_dir, output_name)
    write_csv(data, path)
}

qc_output_dir <- file.path(output_dir, "qc")
unlink(qc_output_dir, recursive = TRUE)
dir.create(qc_output_dir, recursive = TRUE, showWarnings = FALSE)

for (key in names(qc_exports)) {
    data <- qc_exports[[key]]
    output_name <- key |> with_ext(".csv")
    path <- file.path(qc_output_dir, output_name)
    if (nrow(data) > 0) {
        write_csv(data, path)
    }
}
