# dependencies 
library(tidyverse)
library(metafor)
library(patchwork)
library(ggExtra)
library(scales) 
library(janitor)

add_heterogeneity_metrics_to_forest <- function(fit) {
  bquote(paste("RE Model (", tau^2, " = ", .(formatC(janitor::round_half_up(fit$tau2, 2))), 
               ", ", I^2, " = ", .(formatC(janitor::round_half_up(fit$I2, 1))),
               "%, ", H^2," = ", .(formatC(janitor::round_half_up(fit$H2, 2))), ")"))
}

setwd("~/git/irap-reliability-meta-analysis/analyses/plots")
#setwd("C:/Users/ianhu/Documents/GitHub/irap-reliability-meta-analysis/analyses/plots")


# get data
fit_internal_consistency_permuted_estimates <- read_rds("../models/fit_internal_consistency_permuted_estimates.rds")

fit_internal_consistency_permuted_estimates_sensitivity <- read_rds("../models/fit_internal_consistency_permuted_estimates_sensitivity.rds")
fit_test_retest_icc <- read_rds("../models/fit_test_retest_icc.rds")

plot_gosh_ic <- read_rds("../models/plot_gosh_ic.rds")
plot_gosh_trt <- read_rds("../models/plot_gosh_trt.rds")
# plot_gosh <- plot_gosh_ic + plot_gosh_trt + plot_layout(ncol = 2)  # combing them throws an error for some reason


# forest plots

## IC
pdf(NULL)
dev.control(displaylist = "enable")

metafor::forest(fit_internal_consistency_permuted_estimates,
                transf = transf.iabt,
                xlab = bquote(paste("Cronbach's ", alpha)),
                addcred = TRUE,
                refline = NULL,
                xlim = c(-1, 1.6),
                at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                mlab = add_heterogeneity_metrics_to_forest(fit_internal_consistency_permuted_estimates))
                #ilab = data_internal_consistency_permuted_estimates$n,
                #ilab.xpos = 1.1,
                #ilab.pos = 2)
text(-1, 37, "Internal consistency", pos = 4)
text(1.6, 37, bquote(paste(alpha, " [95% CI]")), pos = 2)
#text(1.1, 37, expression(italic("N")), pos = 2)


p1 <- recordPlot()
invisible(dev.off())

pdf("forest_plot_ic.pdf",
    width = 8, 
    height = 10)
p1
dev.off()


## IC sensitivity
pdf(NULL)
dev.control(displaylist = "enable")

metafor::forest(fit_internal_consistency_permuted_estimates_sensitivity,
                transf = transf.iabt,
                xlab = bquote(paste("Cronbach's ", alpha)),
                addcred = TRUE,
                refline = NULL,
                xlim = c(-1, 1.6),
                at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                mlab = add_heterogeneity_metrics_to_forest(fit_internal_consistency_permuted_estimates_sensitivity))
text(-1, 35, "Internal consistency", pos = 4)
text(1.6, 35, bquote(paste(alpha, " [95% CI]")), pos = 2)

p1 <- recordPlot()
invisible(dev.off())

pdf("forest_plot_ic_sensitivity.pdf",
    width = 8, 
    height = 10)
p1
dev.off()



## TRT
pdf(NULL)
dev.control(displaylist = "enable")

metafor::forest(fit_test_retest_icc,
                xlab = "Intraclass Correlation (2,1)",
                addcred = TRUE,
                refline = NULL,
                xlim = c(-2, 1.91),
                at = c(-0.5, 0.0, 0.5, 1.0),
                mlab = add_heterogeneity_metrics_to_forest(fit_test_retest_icc))
text(-2, 10, "Test-retest reliability", pos = 4)
text(1.91, 10, "ICC(2,1) [95% CI]", pos = 2)

p1 <- recordPlot()
invisible(dev.off())

pdf("forest_plot_trt.pdf",
    width = 8, 
    height = 4.22)
p1
dev.off()

# 
# ## IC sensitivity + TRT
# pdf(NULL)
# dev.control(displaylist = "enable")
# par(mfrow=c(2,1))
# layout(matrix(c(1,2), 2, 1, byrow = TRUE),
#        widths=c(1), heights = c(1, 0.47))  #c(1.775,1)
# 
# metafor::forest(fit_internal_consistency_permuted_estimates_sensitivity,
#                 transf = transf.iabt,
#                 xlab = bquote(paste("Cronbach's ", alpha)),
#                 addcred = TRUE,
#                 refline = FALSE,
#                 xlim = c(-1, 1.6),
#                 at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
#                 mlab = add_heterogeneity_metrics_to_forest(fit_internal_consistency_permuted_estimates_sensitivity))
# text(-1, 38, "Internal consistency", pos = 4)
# text(1.6, 38, bquote(paste(alpha, " [95% CI]")), pos = 2)
# 
# metafor::forest(fit_test_retest_icc,
#                 transf = transf.ztor,
#                 xlab = "Absolute Agreement Interclass Correlation",
#                 addcred = TRUE,
#                 refline = FALSE,
#                 xlim = c(-1.78, 1.82),
#                 at = c(-0.4, -0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
#                 mlab = add_heterogeneity_metrics_to_forest(fit_test_retest_icc))
# text(-1.78, 10, "Test-retest reliability", pos = 4)
# text(1.82, 10, "ICC [95% CI]", pos = 2)
# 
# p1 <- recordPlot()
# invisible(dev.off())
# 
# pdf("forest_plots_ic_sensitivity_and_trt.pdf",
#     width = 8, 
#     height = 16)
# p1
# dev.off()


# gosh plots
pdf(NULL)
dev.control(displaylist = "enable")
plot_gosh_ic
p1 <- recordPlot()
invisible(dev.off())
pdf("gosh_plot_ic.pdf",
    width = 6, 
    height = 6)
p1
dev.off()

pdf(NULL)
dev.control(displaylist = "enable")
plot_gosh_trt
p1 <- recordPlot()
invisible(dev.off())
pdf("gosh_plot_trt.pdf",
    width = 6, 
    height = 6)
p1
dev.off()



