#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(xtable)


########################################
## load analyzed data
########################################

dir.create("Table", recursive = TRUE, showWarnings = FALSE)
load("rda/model2_simulation_BA.RData")



strMeanCI <- function(x1, x2, x3, str = "\\\\text{--}") {
    sign1 <- ifelse(x1 < 0, "$-$", "")
    sign2 <- ifelse(x2 < 0, "$-$", "")
    sign3 <- ifelse(x3 < 0, "$-$", "")
    sprintf("%s%.2f [%s%.2f, %s%.2f]",
            sign1, abs(x1), sign2, abs(x2), sign3, abs(x3)) |>
        gsub(pattern = "NA", replacement = str)
}


tbl_BA <- dat_BA_LA |>
    unnest(cols = data) |>
    ungroup() |>
    mutate(MeanCI = strMeanCI(Mean, lwr, upr)) |>
    select(reaction, Time, N, MeanCI)


## comsum of rows for hline
n_rows_cumsum <- tbl_BA |>
    group_by(reaction) |>
    count() |>
    pull(n) |>
    cumsum()

## set row labels
D50_labels <- paste0(c("TD", "LD"), "\\textsubscript{50}")
tbl_BA$reaction <- NA
tbl_BA$reaction[c(0, nrow(tbl_BA) / 2) + 1] <- D50_labels



## set col labels
colnames(tbl_BA) <- c(
    "",
    "Time",
    "$n$",
    "Mean [95\\% LoA]"
)

print(file = "Supplement/TableS2.tex",
    xtable(
        tbl_BA,
        digits = c(0, 0, 0, 0, 0),
        align = c("c", "c", "c", "c", "c")
    ),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(0, n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    timestamp = NULL
)

