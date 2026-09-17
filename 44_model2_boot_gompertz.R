#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(boot)
library(parallel)


########################################
## load analyzed data
########################################

load("rda/animal_data.RData")
source("common_scripts.R")



########################################
## stat function
##     to select the same sample size as original data
##
## args:
##     data
##     .index: not used
## output:
##     coefficients of nls as numeric vector
##
########################################
coef_fun <- function(data, index) {
    d <- data[index, ]
    response <- get_cumsum_rate(d, max_time)
    max_rate <- max(response$rate)
    df_add <- add_data(response, max_rate)

    model <- safe_nls_try(rate ~ SSgompertz(Time, Asym, b2, b3), data = df_add)
    mycoef(model) |> as.numeric()
}


########################################
## perform bootstrap
########################################

R <- 1000
seed <- 1

set.seed(seed)
res_boot <- mclapply(dat_raw, function(x) {
    group_nest(x, Dose) |>
        mutate(boot = map(data, ~
                          boot(data = ., statistic = coef_fun, R = R))
        )
}, mc.cores = n_mc)



########################################
## save data
########################################

save(res_boot, file = "rda//model2_boot_gompertz.RData")

