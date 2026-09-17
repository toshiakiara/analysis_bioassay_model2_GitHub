#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(parallel)
library(ggplot2)
library(RColorBrewer)
library(patchwork)


########################################
## load analyzed data
########################################

load("rda/model2_cv_RMSE.RData")

source("common_scripts.R", local = TRUE)


for (i in 1:2) {
    res_fit_models[[i]] <- res_fit_models[[i]] |>
        mutate(Dose = sprintf("%.1f", Dose))
    res_RMSE[[i]] <- res_RMSE[[i]] |>
        mutate(Dose = sprintf("%.1f", Dose))
}



########################################
## prepare dataa for plot
########################################

dat_point_train <- lapply(res_fit_models, function(.x) {
    .x |>
        dplyr::select(Dose, fold, rate_train) |>
        unnest(cols = rate_train)
})

dat_point_test <- lapply(res_fit_models, function(.x) {
    .x |>
        dplyr::select(Dose, fold, rate_test) |>
        unnest(cols = rate_test)
})

dat_line <- lapply(res_fit_models, function(.x) {
    .x |>
        transmute(
            Dose = Dose,
            fold = fold,
            Time = list(x),
            ## rate for fit curve
            rate_logis = map(model_logis, ~ mypredict(., x)),
            rate_gompertz = map(model_gompertz, ~ mypredict(., x)),
        ) |>
        pivot_longer(c(rate_logis, rate_gompertz),
                     names_to = "Model", values_to = "rate") |>
        mutate(Model = factor(Model,
                              levels = c("rate_logis", "rate_gompertz"),
                              labels = c("Logistic", "Gompertz"))) |>
        arrange(Model, Dose, fold) |>
        dplyr::select(Model, Dose, fold, Time, rate) |>
        unnest(cols = c(Time, rate))
})


.theme <- list(
    labs(x = "Reaction time (min)", y = "Cumulative event (%)"),
    scale_x_continuous(breaks = scales::pretty_breaks()),
    scale_y_continuous(breaks = scales::pretty_breaks()),
    coord_cartesian(ylim = c(0, 100)),
    scale_color_brewer(palette = "Set1"),
    scale_linetype_manual(values = c("solid", "dashed")),
    theme_bw(),
    theme(
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = 0.5),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
    )
)

p <- lapply(1:2, function(i) {
    ggplot(dat_point_train[[i]], aes(Time, rate * 100)) +
        geom_point(color = "gray") +
        geom_line(data = dat_line[[i]],
                  aes(Time, rate * 100, color = Model, linetype = Model)) +
        geom_line(data = dat_point_test[[i]]) +
        geom_point(data = dat_point_test[[i]]) +
        facet_grid(Dose ~ fold) +
        .theme
})


WIDTH <- 6
HEIGHT <- 8

# plot each dose and fold
cairo_pdf(file = "Supplement/FigS1.pdf",
          width = WIDTH, height = HEIGHT, onefile = TRUE)
print(p[[1]])
dev.off()

cairo_pdf(file = "Supplement/FigS2.pdf",
          width = WIDTH, height = HEIGHT, onefile = TRUE)
print(p[[2]])
dev.off()

