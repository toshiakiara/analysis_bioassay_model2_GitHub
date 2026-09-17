#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)


########################################
# load analyzed data
########################################

source("common_scripts.R", local = TRUE)

## dat_raw
## x
load("rda/animal_data.RData")


#####################################
## data processing
#####################################

##
## list of tibble:
##   Dose
##   data
##   response: data points for scatter plot
##   max_rate
##
dat_fit <- lapply(1:2, function(i) {
    dat_raw[[i]] |>
        group_nest(Dose) |>
        mutate(
            response = map(data, ~ get_cumsum_rate(., max_time)),
            ## maximum rate in each dose
            max_rate = map_dbl(response, ~ max(.$rate)),
            ## add fictitious data for nls to avoid error
            df_add = map2(response, max_rate, add_data)
        )
})



#####################################
## fitting
#####################################

## logistic curve
res_fit_logis <- lapply(1:2, function(i) {
    dat_fit[[i]] |>
        transmute(
            Dose = Dose,
            data = df_add,
            model = map(data, ~
                        safe_nls_try(rate ~ SSlogis(Time, Asym, xmid, scal),
                                     data = .)),
        )
})

## CDF of normal distribution
res_fit_normal <- lapply(1:2, function(i) {
    dat_fit[[i]] |>
        transmute(
            Dose = Dose,
            data = df_add,
            model = map(data, ~
                        safe_nls_try(rate ~ Asym * pnorm(Time, mean, sd),
                            start = list(
                                Asym = max(.$rate),
                                mean = max_time / 2,
                                sd = max_time / 10
                            ),
                            data = .)),
        )
})


## Gompertz curve
res_fit_gompertz <- lapply(1:2, function(i) {
    dat_fit[[i]] |>
        transmute(
            Dose = Dose,
            data = df_add,
            model = map(data, ~
                        safe_nls_try(rate ~ SSgompertz(Time, Asym, b2, b3),
                                     data = .)),
        )
})

## CDF of Weibull function
res_fit_weibull <- lapply(1:2, function(i) {
    dat_fit[[i]] |>
        transmute(
            Dose = Dose,
            data = df_add,
            model = map(data, ~
                        safe_nls_try(rate ~ SSweibull(Time, Asym, Drop, lrc, pwr),
                            data = .)),
        )
})


## bind results of all models
res_model <- lapply(1:2, function(i) {
    bind_rows(
        res_fit_logis[[i]],
        res_fit_normal[[i]],
        res_fit_gompertz[[i]],
        res_fit_weibull[[i]],
        .id = "Model"
    ) |>
        mutate(Model = factor(Model,
                              labels = c("Logistic", "Normal",
                                         "Gompertz", "Weibull"))) |>
        arrange(Dose) |>
        dplyr::select(Dose, everything())
})



########################################
## save analyzed data
########################################

save(dat_fit, res_model, file = "rda/model2_fit_model.RData")

