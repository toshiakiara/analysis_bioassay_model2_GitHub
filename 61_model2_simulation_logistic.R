#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(parallel)
library(dqrng)


########################################
## load analyzed data
########################################

source("common_scripts.R")
load("rda/model2_fit_model.RData")



########################################
## Conditions of simulation
########################################

## Condition (sample size)
n_group <- 5  # number of dose
n <- 15       # sample size
N <- 100      # number of replication

## judgemnt time
jt <- 4:8



########################################
## Simulation (model 2)
########################################

seed <- 1234

dqset.seed(seed)
## common among TD50 and LD50
prob <- dqrunif(n_group * n * N)


res_param <- lapply(res_model, function(x) {
    x |>
        dplyr::filter(Model == "Logistic") |>
        mutate(params = map(model, ~ mycoef(.))) |>
        dplyr::select(Model, Dose, params) |>
        arrange(Model, Dose) |>
        unnest(cols = params)
})



## tibble
##   Dose
##   Asym, xmid, scal
##   N: replication
##   ID: indiveduals
##   prob, Time (by simulation)
dat_sim_raw <- mclapply(1:2, function(i) {
    res_param[[i]] |>
        expand_grid(N = seq_len(N), ID = seq_len(n)) |>
        mutate(
            prob = prob,
            Time = logistic_inv(x = prob, Asym = Asym, xmid = xmid, scal = scal),
            Time = ifelse(Time >= max_time, Inf, Time)
        )
}, mc.cores = n_mc)


## tibble
##   judgment_time
##   N: replication
##   data: Dose, Asym, b2, b3, Time, response, logDose
dat_sim_response <- mclapply(1:2, function(i) {
    expand_grid(judgment_time = jt, dat_sim_raw[[i]]) |>
        mutate(
            response = ifelse(Time <= judgment_time, 1, 0),
            logDose = log(Dose)
        ) |>
        group_nest(judgment_time, N)
}, mc.cores = n_mc)



####################
## get TD50/LD50 and 95% CI
####################

## probit analysis / plot
## calculate TD50/LD50 (95% CI) and parameters
res_d50 <- mclapply(1:2, function(i) {
    dat_sim_response[[i]] |>
        mutate(model = map(data, ~ probit_analysis(.)),
               response_tbl = map(data, ~ make_response_table(.)),
               ## result:
               ##   judgment_time
               ##   Dose: TD50/LD50
               ##   lwr, upr: 95% CI of TD50/LD50
               ##   intercept, slope: estimated parameters for linear predictor
               ##   mean, sd: estimated parameters for normal distribution
               result = map(model, ~ calc_d50_model(.))) |>
        dplyr::select(-data) |>
        unnest(col = result) |>
        rename(Time = judgment_time)
}, mc.cores = n_mc)

res <-  mclapply(1:2, function(i) {
    res_d50[[i]] |>
        dplyr::select(Time, Dose, lwr, upr,
                      intercept, slope, Mean, Sigma)
}, mc.cores = n_mc)


## output: tibble
##   reaction, Time, logDose, Dose
d50 <- get_d50(res_d50, c("TD50", "LD50"))

## output: tibble
##   reaction, Time, n
##   log_mean, log_sd, geomean
##   Min, Q1, Median, Q3, Max
d50_summary <- summarise_d50(d50)



########################################
## save analyzed data
########################################

save(
    jt,
    dat_sim_raw,
    dat_sim_response,
    res, res_d50,
    d50, d50_summary,
    file = "rda/model2_simulation_logistic.RData"
)

