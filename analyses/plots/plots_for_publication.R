# dependencies 
library(tidyverse)
library(metafor)
library(patchwork)

add_heterogeneity_metrics_to_forest <- function(fit) {
  bquote(paste("RE Model (", tau^2, " = ", .(formatC(round(fit$tau2, 1))), 
               ", ", I^2, " = ", .(formatC(round(fit$I2, 1))),
               "%, ", H^2," = ", .(formatC(round(fit$H2, 1))), ")"))
}

setwd("~/git/irap-reliability-meta-analysis/analyses/plots")


# get data
fit_internal_consistency_permuted_estimates <- read_rds("~/git/irap-reliability-meta-analysis/analyses/models/fit_internal_consistency_permuted_estimates.rds")

fit_internal_consistency_permuted_estimates_sensitivity <- read_rds("~/git/irap-reliability-meta-analysis/analyses/models/fit_internal_consistency_permuted_estimates_sensitivity.rds")
fit_test_retest_icc <- read_rds("~/git/irap-reliability-meta-analysis/analyses/models/fit_test_retest_icc.rds")

plot_gosh_ic <- read_rds("~/git/irap-reliability-meta-analysis/analyses/models/plot_gosh_ic.rds")
plot_gosh_trt <- read_rds("~/git/irap-reliability-meta-analysis/analyses/models/plot_gosh_trt.rds")
# plot_gosh <- plot_gosh_ic + plot_gosh_trt + plot_layout(ncol = 2)  # combing them throws an error for some reason


# forest plots

## IC
pdf(NULL)
dev.control(displaylist = "enable")

metafor::forest(fit_internal_consistency_permuted_estimates,
                transf = transf.iabt,
                xlab = bquote(paste("Cronbach's ", alpha)),
                addcred = TRUE,
                refline = FALSE,
                xlim = c(-1, 1.6),
                at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                mlab = add_heterogeneity_metrics_to_forest(fit_internal_consistency_permuted_estimates))
                #ilab = data_internal_consistency_permuted_estimates$n,
                #ilab.xpos = 1.1,
                #ilab.pos = 2)
text(-1, 28, "Internal consistency", pos = 4)
text(1.6, 28, bquote(paste(alpha, " [95% CI]")), pos = 2)
#text(1.1, 28, expression(italic("N")), pos = 2)


p1 <- recordPlot()
invisible(dev.off())

pdf("forest_plot_ic.pdf",
    width = 8, 
    height = 8)
p1
dev.off()


## IC sensitivity + TRT
pdf(NULL)
dev.control(displaylist = "enable")
par(mfrow=c(2,1))
layout(matrix(c(1,2), 2, 1, byrow = TRUE),
       widths=c(1), heights = c(1, 0.57))  #c(1.775,1)

metafor::forest(fit_internal_consistency_permuted_estimates_sensitivity,
                transf = transf.iabt,
                xlab = bquote(paste("Cronbach's ", alpha)),
                addcred = TRUE,
                refline = FALSE,
                xlim = c(-1, 1.6),
                at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                mlab = add_heterogeneity_metrics_to_forest(fit_internal_consistency_permuted_estimates_sensitivity))
text(-1, 27, "Internal consistency", pos = 4)
text(1.6, 27, bquote(paste(alpha, " [95% CI]")), pos = 2)

metafor::forest(fit_test_retest_icc,
                transf = transf.ztor,
                xlab = "Absolute Agreement Interclass Correlation",
                addcred = TRUE,
                refline = FALSE,
                xlim = c(-1.78, 1.82),
                at = c(-0.4, -0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                mlab = add_heterogeneity_metrics_to_forest(fit_test_retest_icc))
text(-1.78, 10, "Test-retest reliability", pos = 4)
text(1.82, 10, "ICC [95% CI]", pos = 2)

p1 <- recordPlot()
invisible(dev.off())

pdf("forest_plots_ic_sensitivity_and_trt.pdf",
    width = 8, 
    height = 12)
p1
dev.off()


# gosh plots
pdf(NULL)
dev.control(displaylist = "enable")
plot_gosh_ic
p1 <- recordPlot()
invisible(dev.off())
pdf("gosh_plot_internalconsistency.pdf",
    width = 6, 
    height = 6)
p1
dev.off()

pdf(NULL)
dev.control(displaylist = "enable")
plot_gosh_trt
p1 <- recordPlot()
invisible(dev.off())
pdf("gosh_plot_testretestreliability.pdf",
    width = 6, 
    height = 6)
p1
dev.off()



