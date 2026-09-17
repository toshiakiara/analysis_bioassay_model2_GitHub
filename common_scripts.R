library(dplyr)


n_mc <- parallel::detectCores()

max_time <- 10
x <- seq(0, max_time, by = 0.1)


########################################
## relative cumsum
##
## args:
##     dat: data.frame
##         Time, status
##     max_time: default is 10
##
## output: data.frame
##     Time, rate
########################################
get_cumsum_rate <- function(dat, max_time = 10) {
    N <- nrow(dat)
    dat$status <- ifelse(dat$Time > max_time, 0, dat$status)

    x <- subset(dat, dat$status == 1)$Time
    .tbl <- table(x)

    if (length(x) == 0) {
        return(data.frame(Time = max_time, rate = 0))
    }

    if (max(x) == max_time) {
        .df <- data.frame(
            Time = as.numeric(names(.tbl)),
            n = as.numeric(.tbl)
        )
    } else {
        .df <- data.frame(
            Time = c(as.numeric(names(.tbl)), max_time),
            n = c(as.numeric(.tbl), 0)
        )
    }

    .df <- .df |>
        mutate(cumsum = cumsum(n),
               rate = cumsum / N) |>
        dplyr::select(!!sym("Time"), !!sym("rate"))
    .df
}


##
## helper function
##

## add fictitious data for nls
## (to avoid analysis error in small sample size)
##
add_data <- function(x, max_rate) {
    x |>
        bind_rows(
            tibble(
                Time = seq(15, 25, by = 5),
                rate = max_rate * (1 +  seq_len(3) / 200)
            )) |>
        dplyr::filter(!!sym("Time") > 0)
}


## nls with try-error
safe_nls_try <- function(formula, data, start = NULL, control = nls.control()) {
    try(
        if (is.null(start)) {
            nls(formula, data = data, control = control)
        } else {
            nls(formula, data = data, start = start, control = control)
        },
        silent = TRUE
    )
}

## drm with try-error
safe_drm_try <- function(formula, data, ...) {
    try(
        drm(formula, data = data, ...),
        silent = TRUE
    )
}



#####################################
## R2, RMSE, AIC, BIC
#####################################

## RMSE, Rsquare
## func: modelr::rmse, modelr::rsquare
get_index <- function(model, data, func) {
    ifelse(class(model) == "try-error", NA_real_, func(model, data))
}

myAIC <- function(model) {
    ifelse(class(model) == "try-error", NA_real_, AIC(model))
}

myBIC <- function(model) {
    ifelse(class(model) == "try-error", NA_real_, BIC(model))
}

mycoef <- function(model) {
    if (class(model) == "try-error") {
        NA_real_
    } else {
        data.frame(t(coef(model)))
    }
}

mypredict <- function(model, x, ...) {
    if (class(model) == "try-error") {
        rep(NA_real_, length(x))
    } else {
        predict(model, newdata = data.frame(Time = x), ...)
    }
}



########################################
## set response within judgment time
##
## x: data
##     judgment time: (sec)
##     status: 0 (no response) / 1 (response)
##
## output: add these columns
##     response: 0 (no response) / 1 (response)
##
########################################
set_response <- function(x, judgment_time) {
    x$response <- as.integer(x$Time_sec <= judgment_time & x$status == 1)
    x
}



########################################
## make response table
##
## x: data
##     judgment time: (sec)
##     status: 0 (no response) / 1 (response)
##
## output: tibble
##     Dose / rate / ldose
##
########################################
make_response_table <- function(x) {
    data.frame(x) |>
        summarise(
            .by = !!sym("Dose"),
            rate = mean(!!sym("response"))
        ) |>
        mutate(logDose = log(!!sym("Dose")))
}



########################################
## probit analysis
##
## x: data
##     logDose
##     response: 0 (no response) / 1 (response)
##
## output:
##     model by glm (response ~ logDose)
##
########################################
probit_analysis <- function(x) {
    glm(response ~ logDose, family = binomial("probit"), data = x)
}



########################################
## calculate TD50/LD50 and 95% CI
##
## x: data
##     Dose
##     response: 0 (no response) / 1 (response)
##
## model:
##     model by probit analysis (glm)
##
## output:
##     judgment_time
##     Dose
##     lwr, upr: 95% CI of TD50/LD50
##     intercept, slope: estimated parameters for linear predictor
##     mean, sd: estimated parameters for normal distribution
##
########################################
calc_d50_model <- function(model) {
    res <- get_ci(model, p = 0.5)

    coef1 <- coef(model)
    intercept <- coef1[1] |> as.numeric()
    slope <- coef1[2] |> as.numeric()

    data.frame(
        res,
        intercept = intercept, slope = slope,
        Mean = -intercept / slope,
        Sigma = 1 / slope
    )
}


## Delta method
## https://rion778.hatenablog.com/entry/20100612/1276358244
get_ci <- function(model, p = 0.5, alpha = 0.05) {
    d <- MASS::dose.p(model, p = p)
    log_d <- d |> as.vector()
    dse <- attributes(d)$SE |> as.vector()
    q <- qnorm(1 - alpha / 2)

    data.frame(
        prob = p,
        logDose = as.vector(log_d),
        log_lwr = log_d - q * dse,
        log_upr = log_d + q * dse,
        Dose = exp(as.vector(log_d)),
        lwr = exp(log_d - q * dse),
        upr = exp(log_d + q * dse)
    )
}



########################################
##
## get D50 from res_d50
##
## x: res_d50 (list)
##     Time, logDose, Dose
##
## output: tibble (bind rows)
##     reaction, Time, logDose, Dose
##
########################################
get_d50 <- function(.list, labels) {
    lapply(.list, function(x) {
        dplyr::select(x, !!sym("Time"), !!sym("logDose"), !!sym("Dose"))
    }) |>
        bind_rows(.id = "reaction") |>
        mutate(reaction = factor(!!sym("reaction"), labels = labels))
}


########################################
##
## summarize D50 from res_d50
##
## x: d50 (data.frame)
##     reaction, Time, logDose, Dose
##
## output: tibble
##     reaction, Time, n
##     log_mean, log_sd, geomean
##     Min, Q1, Median, Q3, Max
##
########################################
summarise_d50 <- function(d50) {
    d50 |>
        summarize(
            .by = c(!!sym("reaction"), !!sym("Time")),
            n = n(),
            log_mean = mean(!!sym("logDose")),
            log_sd = sd(!!sym("logDose")),
            geomean = exp(!!sym("log_mean")),
            Min = min(!!sym("Dose")),
            Q1 = quantile(!!sym("Dose"), probs = 0.25),
            Median = median(!!sym("Dose")),
            Q3 = quantile(!!sym("Dose"), probs = 0.75),
            Max = max(!!sym("Dose"))
        ) |>
        ungroup()
}



########################################
## Gompertz function
########################################

gompertz <- function(x, Asym, b2, b3) {
    Asym * exp(-b2 * b3^x)
}

gompertz_inv <- function(x, Asym, b2, b3) {
    ifelse(
        x < Asym,
        log(log(Asym / x) / b2) / log(b3),
        Inf
    )
}

logistic_inv <- function(x, Asym, xmid, scal) {
    ifelse(
        x < Asym,
        qlogis(p = x / Asym, location = xmid, scale = scal),
        Inf
    )
}



########################################
## polyval function
########################################

polyval <- function(x, a, b, c, d, e) {
    (((((((e * x) + d) * x) + c) * x) + b) * x) + a
}

