#!/usr/bin/env Rscript

library(readr)
library(dplyr)


filename <- c("lidocaine_toxic.csv", "lidocaine_lethal.csv")

# source("common_scripts.R", local = TRUE)
dir.create("rda", recursive = TRUE, showWarnings = FALSE)
dir.create("Fig", recursive = TRUE, showWarnings = FALSE)
dir.create("Table", recursive = TRUE, showWarnings = FALSE)
dir.create("csv", recursive = TRUE, showWarnings = FALSE)
dir.create("Supplement", recursive = TRUE, showWarnings = FALSE)



########################################
# read raw data
########################################

## Dose (mg/kg)
## logDose
## Time_sec: max 600 sec
## Time: max 10 min
## status: 0 (without response) / 1 (with response)
dat_raw <- lapply(filename, function(x) {
    read_csv(x, show_col_types = FALSE) |>
        rename(Time_sec = Time) |>
        mutate(logDose = log(Dose),
               Time = Time_sec / 60) |>
        dplyr::select(Dose, logDose, Time_sec, Time, status)
})



########################################
## save analyzed data
########################################

save(dat_raw, file = "rda/animal_data.RData")

