library(tidyverse)
library(metafor)

add_heterogeneity_metrics_to_forest <- function(fit) {
  bquote(paste("RE Model (", tau^2, " = ", .(formatC(round(fit$tau2, 1))), 
               ", ", I^2, " = ", .(formatC(round(fit$I2, 1))),
               "%, ", H^2," = ", .(formatC(round(fit$H2, 1))), ")"))
}

setwd("~/git/IRAP-critique/reliability meta analyses/analyses/plots")

fit_permutations <- read_rds("~/git/IRAP-critique/reliability meta analyses/analyses/models/fit_permutations.rds")
fit_aa <- read_rds("~/git/IRAP-critique/reliability meta analyses/analyses/models/fit_aa.rds")


pdf(NULL)
dev.control(displaylist = "enable")
par(mfrow=c(2,1))
layout(matrix(c(1,2), 2, 1, byrow = TRUE),
       widths=c(1), heights=c(2.65,1))

metafor::forest(fit_permutations,
                transf = transf.iabt,
                xlab = bquote(paste("Cronbach's ", alpha)),
                addcred = TRUE,
                refline = FALSE,
                xlim = c(-1, 1.6),
                at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                mlab = add_heterogeneity_metrics_to_forest(fit_permutations))
text(-1, 27, "Internal Consistency", pos = 4)
text(1.6, 27, bquote(paste(alpha, " [95% CI]")), pos = 2)

metafor::forest(fit_aa,
                transf = transf.ztor,
                xlab = "Interclass Correlation Coefficient",
                addcred = TRUE,
                refline = FALSE,
                xlim = c(-1, 1.6),
                at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                mlab = add_heterogeneity_metrics_to_forest(fit_aa))
text(-1, 4, "Test-Retest Reliability", pos = 4)
text(1.6, 4, "ICC [95% CI]", pos = 2)

p1 <- recordPlot()
invisible(dev.off())

pdf("forest_plots.pdf",
    width = 8, 
    height = 12)
p1
dev.off()
