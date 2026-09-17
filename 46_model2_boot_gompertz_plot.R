#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(GGally)
library(patchwork)
library(scales)



########################################
## load analyzed data
########################################

load("rda/model2_boot_params_gompertz.RData")
source("common_scripts.R")

level_params <- c("Asym", "b2", "b3")



########################################
## plot
########################################

## ChatGPT
label_auto_log <- function(threshold = 10) {
    function(x) {
        if (max(x, na.rm = TRUE) >= threshold) {
            ## If a value exceeds the threshold,
            ##   display it in exponential notation
            parse(text = paste0("10^", log10(x)))
        } else {
            # Normal numeric display
            number_format()(x)
        }
    }
}

.theme <- list(
    theme(
        axis.text.x = element_text(size = 11, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.grid.minor = element_blank(),
    )
)

.theme_ggmatrix <- list(
    theme(
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        strip.background = element_rect(fill = "transparent"),
        panel.spacing.x = unit(12, "pt"),
    )
)


for (i in 1:2) {
    res_boot_plot[[i]] <- res_boot_plot[[i]] |>
        mutate(Dose = sprintf("%.1f", Dose))
}

p <- lapply(res_boot_plot, function(x) {
    ## data for histogram
    df_boot_plot <- x |>
        dplyr::select(Dose, t) |>
        mutate(dat = map(t, ~ {
                pivot_longer(., cols = everything(),
                             names_to = "Parameter", values_to = "Value")
        })) |>
        dplyr::select(Dose, dat) |>
        unnest(cols = dat) |>
        group_nest(Dose, Parameter) |>
        mutate(Parameter = factor(Parameter, levels = level_params)) |>
        arrange(Dose, Parameter)

    ## data for confidence interval
    ci <- x |>
        dplyr::select(Dose, param) |>
        unnest(cols = param) |>
        group_nest(Dose, Parameter) |>
        mutate(Parameter = factor(Parameter, levels = level_params)) |>
        arrange(Dose, Parameter)

    Dose <- x$Dose
    nr <- nrow(df_boot_plot)

    .plot <- lapply(seq_len(nr), function(i) {
        ggplot(df_boot_plot$data[[i]], aes(x = Value)) +
        geom_histogram(fill = "gray", color = "black") +
        geom_vline(data = ci$data[[i]], aes(xintercept = t0), linetype = "dashed") +
        geom_vline(data = ci$data[[i]], aes(xintercept = lwr)) +
        geom_vline(data = ci$data[[i]], aes(xintercept = upr)) +
        theme_bw() +
        .theme
    })

    df_plot <- expand_grid(Dose, Parameter = level_params) |>
        mutate(plot = .plot)

    ## X-axis is log scale
    ## Gompertz: b2 only
    for (i in seq_len(nr)) {
        if (df_plot$Parameter[[i]] == level_params[2]) { ## b2
               df_plot$plot[[i]] <- df_plot$plot[[i]] +
                   scale_x_log10(labels = label_auto_log(threshold = 10))
        }
    }

    ## place matrix
    pm <- ggmatrix(
        plots = df_plot$plot,
        nrow = length(Dose),
        ncol = length(level_params),
        xAxisLabels = level_params,
        yAxisLabels = as.character(Dose),
        xlab = "Parameter Value",
        ylab = "Frequency"
    ) + .theme_ggmatrix

    ## unified XY-axis scale
    nx <- length(level_params)
    ny <- length(Dose)
    xlims <- vector("list", nx)
    ymax <- vector("list", ny)

    for (i in seq_len(ny)) {
        for (j in seq_len(nx)) {
            xlims[[j]] <- df_boot_plot |>
                dplyr::filter(Parameter == level_params[j]) |>
                unnest(cols = data) |>
                pull(Value) |>
                range(na.rm = TRUE)

            max_count <- sapply(seq_along(level_params), function(j) {
                pb <- ggplot_build(pm[i, j])
                max(pb$data[[1]]$count, na.rm = TRUE)
            })
            ymax[[i]] <- max(max_count, na.rm = TRUE)
        }
    }
    ylim <- c(0, max(unlist(ymax)))

    for (i in seq_along(Dose)) {
        for (j in seq_along(level_params)) {
            pm[i, j] <- pm[i, j] +
                coord_cartesian(xlim = xlims[[j]], ylim = ylim) #+
        }
    }

    return(pm)
})


## combine ggmatrix by patchwork
##   wrap_elements(ggmatrix_gtable())
wp <- lapply(p, function(.x) {
    wrap_elements(ggmatrix_gtable(.x))
})

p_comb <- wrap_plots(wp, ncol = 1) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 28))


WIDTH <- 7
HEIGHT <- 12

cairo_pdf(file = "Fig/Fig4.pdf",
          width = WIDTH, height = HEIGHT)
print(p_comb)
dev.off()

