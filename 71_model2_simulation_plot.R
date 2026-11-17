#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggforce)
library(patchwork)


########################################
## load analyzed data
########################################

source("common_plots.R")

load("rda/model2_simulation_logistic.RData")
d50_logistic <- d50

load("rda/model2_simulation_gompertz.RData")
d50_gompertz <- d50



########################################
## Violin plot of both simulations
########################################

d50 <- bind_rows(d50_logistic, d50_gompertz, .id = "Model") |>
    mutate(Model = factor(Model, labels = c("Logistic", "Gompertz")),
           reaction = factor(reaction, labels = label_reaction),
           Time = factor(Time)) |>
    dplyr::select(Model, reaction, Time, Dose)


seed <- 123
set.seed(seed)
p_simulation <- ggplot(d50, aes(Time, Dose)) +
    geom_violin() +
    geom_sina(size = 0.8, alpha = 0.5) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.8) +
    labs(x = "Judgment Time (min)", y = "Dose (mg/kg)") +
    facet_grid(reaction ~ Model, scales = "free_y", labeller = label_parsed) +
    theme_bw() +
    theme(
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 18),
        strip.background = element_blank()
    )



####################
# save as PDF
####################

WIDTH <- 8
HEIGHT <- 6

cairo_pdf(file = "Fig/Fig5.pdf",
          width = WIDTH, height = HEIGHT)
print(p_simulation)
dev.off()



########################################
## Comparison
########################################

dat_point <- tibble(
    reaction = d50_logistic$reaction,
    Time = d50_logistic$Time,
    Dose_logistic = d50_logistic$Dose,
    Dose_gompertz = d50_gompertz$Dose
) |>
    mutate(
        Mean = (Dose_logistic + Dose_gompertz) / 2,
        Diff = Dose_logistic - Dose_gompertz,
        reaction = factor(reaction, labels = label_reaction),
    )


####################
## Scatter plot
####################

dat_scatter <- split(dat_point, dat_point$reaction)

lab_x <- c(
    expression(paste("TD"[50], " of Logistic model", sep = "")),
    expression(paste("LD"[50], " of Logistic model", sep = ""))
)
lab_y <- c(
    expression(paste("TD"[50], " of Gompertz model", sep = "")),
    expression(paste("LD"[50], " of Gompertz model", sep = ""))
)

p_scatter <- lapply(1:2, function(i) {
    ggplot(dat_scatter[[i]], aes(Dose_logistic, Dose_gompertz)) +
        geom_point(alpha = 0.5) +
        geom_abline(slope = 1, intercept = 0) +
        labs(x = lab_x[i], y = lab_y[i]) +
        facet_wrap(. ~ Time, labeller = label_parsed, nrow = 1) +
        theme_bw() +
        theme(
            axis.text = element_text(size = 12, color = "black"),
            axis.title = element_text(size = 14),
            panel.grid.major.x = element_blank(),
            strip.text = element_text(size = 16),
            strip.background = element_blank(),
            aspect.ratio = 1
        )
})



####################
## Bland-Altman plot
####################

alpha <- 0.05
q <- qnorm(1 - alpha / 2)


dat_point_BA <- dat_point |>
    group_by(reaction) |>
    nest()

dat_BA_LA <- dat_point |>
    summarise(
        .by = c(reaction, Time),
        N = n(),
        Mean = mean(Diff),
        SD = sd(Diff),
        lwr = Mean - SD * q,
        upr = Mean + SD * q
    ) |>
    drop_na() |>
    group_by(reaction) |>
    nest()


lab_x <- c(
    expression("Mean of TD"[50]),
    expression("Mean of LD"[50])
)
lab_y <- c(
    expression("Differences in TD"[50]),
    expression("Differences in LD"[50])
)

p_BA <- lapply(1:2, function(i) {
    ggplot(dat_point_BA$data[[i]], aes(Mean, Diff)) +
        geom_point(alpha = 0.5) +
        geom_hline(data = dat_BA_LA$data[[i]], aes(yintercept = Mean)) +
        geom_hline(data = dat_BA_LA$data[[i]], aes(yintercept = lwr), linetype = "dashed") +
        geom_hline(data = dat_BA_LA$data[[i]], aes(yintercept = upr), linetype = "dashed") +
        labs(x = lab_x[i], y = lab_y[i]) +
        facet_wrap(. ~ Time, labeller = label_parsed, nrow = 1) +
        theme_bw() +
        theme(
            axis.text = element_text(size = 12, color = "black"),
            axis.title = element_text(size = 14),
            panel.grid.major.x = element_blank(),
            strip.text = element_text(size = 16),
            strip.background = element_blank()
        )
})


p_comb <- wrap_plots(
        wrap_plots(p_scatter, ncol = 1),
        wrap_plots(p_BA, ncol = 1),
        ncol = 1
    ) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 28))


####################
# save as PDF
####################

WIDTH <- 8
HEIGHT <- 11

cairo_pdf(file = "Supplement/FigS3.pdf",
          width = WIDTH, height = HEIGHT)
print(p_comb)
dev.off()



########################################
## Save data
########################################

save(dat_BA_LA, file = "rda/model2_simulation_BA.RData")

