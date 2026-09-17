#!/usr/bin/env Rscript

library(survminer)
library(RColorBrewer)
library(patchwork)


########################################
## survival analysis
## load analyzed data
########################################

source("common_scripts.R", local = TRUE)

## dat_raw
load("rda/animal_data.RData")
## res_surv <= use
## tbl_surv
load("rda/model2_animal_survival_analysis.RData")


dose_str <- lapply(dat_raw, function(x) {
    sprintf("%.1f", unique(x$Dose))
})


########################################
## Kaplan-Meyer plot
########################################

.theme <- list(
    labs(x = "Reaction time (min)",
         color = "Dose (mg/kg)"),
    scale_x_continuous(breaks = seq(0, 10, by = 2), minor_breaks = NULL),
    coord_cartesian(ylim = c(0, 1)),
    scale_color_brewer(palette = "Dark2"),
    geom_hline(yintercept = 0.5, linetype = "dotted"),
    theme(
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)
    )
)


p <- lapply(1:2, function(i) {
    ggsurvplot(
        res_surv[[i]],
        dat_raw[[i]],
        fun = "event",
        size = 0.7, alpha = 0.8,
        surv.scale = "percent",
        censor.shape = "|",
        legend.lab = dose_str[[i]],
        short.panel.labs = TRUE,
        ggtheme = theme_bw())$plot
})

p_comb <- wrap_plots(p, ncol = 1) &
    .theme &
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 24))

cairo_pdf(file = "Fig/Fig1.pdf", width = 6, height = 6)
print(p_comb)
dev.off()

