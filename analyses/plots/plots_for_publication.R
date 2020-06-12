library(tidyverse)
library(metafor)

add_heterogeneity_metrics_to_forest <- function(fit) {
  bquote(paste("RE Model (", tau^2, " = ", .(formatC(round(fit$tau2, 1))), 
               ", ", I^2, " = ", .(formatC(round(fit$I2, 1))),
               "%, ", H^2," = ", .(formatC(round(fit$H2, 1))), ")"))
}

setwd("~/git/irap-reliability-meta-analysis/analyses/plots")

fit_internal_consistency_permuted_estimates <- read_rds("~/git/irap-reliability-meta-analysis/analyses/models/fit_internal_consistency_permuted_estimates.rds")
fit_test_retest_icc <- read_rds("~/git/irap-reliability-meta-analysis/analyses/models/fit_test_retest_icc.rds")


pdf(NULL)
dev.control(displaylist = "enable")
par(mfrow=c(2,1))
layout(matrix(c(1,2), 2, 1, byrow = TRUE),
       widths=c(1), heights=c(2.65,1))

metafor::forest(fit_internal_consistency_permuted_estimates,
                transf = transf.iabt,
                xlab = bquote(paste("Cronbach's ", alpha)),
                addcred = TRUE,
                refline = FALSE,
                xlim = c(-1, 1.6),
                at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                mlab = add_heterogeneity_metrics_to_forest(fit_internal_consistency_permuted_estimates))
text(-1, 27, "Internal Consistency", pos = 4)
text(1.6, 27, bquote(paste(alpha, " [95% CI]")), pos = 2)

metafor::forest(fit_test_retest_icc,
                transf = transf.ztor,
                xlab = "Interclass Correlation Coefficient",
                addcred = TRUE,
                refline = FALSE,
                xlim = c(-1, 1.6),
                at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                mlab = add_heterogeneity_metrics_to_forest(fit_test_retest_icc))
text(-1, 4, "Test-Retest Reliability", pos = 4)
text(1.6, 4, "ICC [95% CI]", pos = 2)

p1 <- recordPlot()
invisible(dev.off())

pdf("forest_plots.pdf",
    width = 8, 
    height = 12)
p1
dev.off()
