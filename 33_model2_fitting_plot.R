#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(RColorBrewer)
library(patchwork)


########################################
# load analyzed data
########################################

source("common_scripts.R", local = TRUE)

## x
load("rda/animal_data.RData")
## res_model
load("rda/model2_fit_model.RData")



#####################################
## fitted curve
#####################################
mypredict <- function(model, newdata) {
    if (class(model) == "try-error") {
        NA
    } else {
        predict(model, newdata = data.frame(Time = newdata))
    }
}


dat_fitline <- lapply(res_model, function(.x) {
    .x |>
        dplyr::filter(Model %in% c("Logistic", "Gompertz")) |>
        mutate(Time = list(x),
               rate = map2(model, Time, mypredict)) |>
        dplyr::select(-data, -model) |>
        unnest(cols = c(Time, rate)) |>
        mutate(Dose = factor(Dose,
                             labels = sprintf("%.1f mg/kg", unique(Dose))))
})

dat_rate <- lapply(dat_fit, function(x) {
    x |>
        dplyr::select(Dose, response) |>
        unnest(cols = response) |>
        mutate(Dose = factor(Dose,
                             labels = sprintf("%.1f mg/kg", unique(Dose))))
})



#####################################
## plot
#####################################

.theme <- list(
    labs(x = "Reaction time (min)", y = "Cumulative event (%)", color = "Model"),
    coord_cartesian(ylim = c(0, 100)),
    scale_x_continuous(breaks = scales::pretty_breaks(), minor_breaks = NULL),
    scale_y_continuous(breaks = scales::pretty_breaks()),
    scale_color_brewer(palette = "Set1"),
    # scale_color_manual(values = c("red", "blue")),
    scale_linetype_manual(values = c("solid", "dashed")),
    # scale_linetype_manual(values = c("dashed", "solid")),
    # scale_linetype_manual(values = c("solid", "solid")),
    theme_bw(),
    theme(
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
    )
)


p <- lapply(1:2, function(i) {
    ggplot(dat_rate[[i]],
           aes(Time, rate * 100)) +
        geom_point(size = 0.7) +
        geom_line(data = dat_fitline[[i]],
                  aes(color = Model, linetype = Model),
                  linewidth = 0.5, alpha = 0.7) +
        facet_wrap(~ Dose)
})

p_comb <- wrap_plots(p, ncol = 1, guides = "collect") &
    .theme &
    plot_annotation(tag_levels = "A") &
    theme(
        plot.tag = element_text(size = 24),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)
    )


WIDTH <- 6
HEIGHT <- 9

cairo_pdf(file = "Fig/Fig2.pdf",
          width = WIDTH, height = HEIGHT)
print(p_comb)
dev.off()

