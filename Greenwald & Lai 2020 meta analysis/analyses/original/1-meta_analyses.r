# Author: Calvin Lai
# Creation Date: 04/02/19
# Last updated: 04/19/19
# Function: Analyzes data using robust variance estimation meta-analysis

# INSTRUCTION: Replace C:/Users/Calvin/Documents/Dropbox/Projects/ARP/share/ with the name of your directory.

library(tidyverse)
library(robumeta)
library(metafor)
library(xlsx)
library(psych)

# Importing data ----
source("C:/Users/Calvin/Documents/Dropbox/Projects/ARP/share/0-meta_setup.r")

# robumeta analyses ----
# IC ----
# Creating overall models with transformed alphas
AAT_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "AAT" & all$reliability == "IC",])
AMP_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure =="AMP" & all$reliability == "IC",])
BIAT_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "BIAT" & all$reliability == "IC",])
EAST_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "EAST" & all$reliability == "IC",])
EP_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "Evaluative Priming" & all$reliability == "IC",])
GNAT_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "GNAT" & all$reliability == "IC",])
IAT_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "IAT" & all$reliability == "IC",])
IATRF_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "IAT-RF" & all$reliability == "IC",])
IRAP_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "IRAP" & all$reliability == "IC",])
NLE_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "Name Letter Effect" & all$reliability == "IC",])
SCIAT_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "SC-IAT" & all$reliability == "IC",])
SRCT_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "SRCT" & all$reliability == "IC",])
STIAT_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$measure == "ST-IAT" & all$reliability == "IC",])

# Tranforming alphas back
inverseBonett <- function (x) {
  return(c( 
    (1 - exp(-1*x$b.r)),
    (1 - exp(-1*x$reg_table$CI.L)),
    (1 - exp(-1*x$reg_table$CI.U))
  ))
}

# Creating table rows
IClist <- function (x, y) {
  return(c( 
    y, x$N, x$M, inverseBonett(x)[1], inverseBonett(x)[2], inverseBonett(x)[3], x$mod_info$I.2, x$mod_info$tau.sq
  ))
}
AAT_IClist <- IClist(AAT_IC, "AAT")
AMP_IClist <- IClist(AMP_IC, "AMP")
BIAT_IClist <- IClist(BIAT_IC, "BIAT")
EAST_IClist <- IClist(EAST_IC, "EAST")
EP_IClist <- IClist(EP_IC, "EP")
GNAT_IClist <- IClist(GNAT_IC, "GNAT")
IAT_IClist <- IClist(IAT_IC, "IAT")
IATRF_IClist <- IClist(IATRF_IC, "IATRF")
IRAP_IClist <- IClist(IRAP_IC, "IRAP")
NLE_IClist <- IClist(NLE_IC, "NLE")
SCIAT_IClist <- IClist(SCIAT_IC, "SCIAT")
SRCT_IClist <- IClist(SRCT_IC, "SRCT")
  SRCT_IClist[5] <- 0   # Lower limit is below 0, so return 0 (following metafor's transf.iabt function)
STIAT_IClist <- IClist(STIAT_IC, "STIAT")

# table for IC ----
ICtable <- matrix(c(
  AAT_IClist, AMP_IClist, BIAT_IClist, EAST_IClist, EP_IClist, GNAT_IClist, IAT_IClist, 
  IATRF_IClist, IRAP_IClist, NLE_IClist, SCIAT_IClist, SRCT_IClist, STIAT_IClist
  ), 
  ncol=8, byrow=TRUE)
colnames(ICtable)<-c("Measure", "IC studies", "IC outcomes", "IC estimate", "IC CI-lower", "IC CI-upper", "IC I.sq", "IC Tau.sq")

# TRR ----
# Creating overall models with z values
AAT_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "AAT" & all$reliability == "TRR",])
AMP_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "AMP" & all$reliability == "TRR",])
BIAT_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "BIAT" & all$reliability == "TRR",])
EAST_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "EAST" & all$reliability == "TRR",])
EP_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "Evaluative Priming" & all$reliability == "TRR",])
GNAT_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "GNAT" & all$reliability == "TRR",])
IAT_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "IAT" & all$reliability == "TRR",])
# IATRF_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "IAT-RF" & all$reliability == "TRR",])
IRAP_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "IRAP" & all$reliability == "TRR",])
NLE_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "Name Letter Effect" & all$reliability == "TRR",])
SCIAT_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "SC-IAT" & all$reliability == "TRR",])
# SRCT_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "SRCT" & all$reliability == "TRR",])
STIAT_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$measure == "ST-IAT" & all$reliability == "TRR",])

# Tranforming zs back to r
inverseZ <- function (x) {
  return(c( 
    fisherz2r(x$b.r),
    fisherz2r(x$reg_table$CI.L),
    fisherz2r(x$reg_table$CI.U)
  ))
}

# Creating table rows
TRRlist <- function (x, y) {
  return(c( 
    y, x$N, x$M, inverseZ(x)[1], inverseZ(x)[2], inverseZ(x)[3], x$mod_info$I.2, x$mod_info$tau.sq
  ))
}
AAT_TRRlist <- TRRlist(AAT_TRR, "AAT")
AMP_TRRlist <- TRRlist(AMP_TRR, "AMP")
BIAT_TRRlist <- TRRlist(BIAT_TRR, "BIAT")
EAST_TRRlist <- TRRlist(EAST_TRR, "EAST")
EP_TRRlist <- TRRlist(EP_TRR, "EP")
GNAT_TRRlist <- TRRlist(GNAT_TRR, "GNAT")
IAT_TRRlist <- TRRlist(IAT_TRR, "IAT")
# IATRF_TRRlist <- TRRlist(IATRF_TRR, "IATRF") # Doesn't run, no estimates
IRAP_TRRlist <- TRRlist(IRAP_TRR, "IRAP")
NLE_TRRlist <- TRRlist(NLE_TRR, "NLE")
SCIAT_TRRlist <- TRRlist(SCIAT_TRR, "SCIAT")
# SRCT_TRRlist <- TRRlist(SRCT_TRR, "SRCT") # Doesn't run, no estimates
STIAT_TRRlist <- TRRlist(STIAT_TRR, "STIAT")

# table for TRR ----
TRRtable <- matrix(c(
  AAT_TRRlist, AMP_TRRlist, BIAT_TRRlist, EAST_TRRlist, EP_TRRlist, GNAT_TRRlist, IAT_TRRlist, 
   IRAP_TRRlist, NLE_TRRlist, SCIAT_TRRlist,  STIAT_TRRlist
), 
ncol=8, byrow=TRUE)
colnames(TRRtable)<-c("Measure", "TRR studies", "TRR outcomes", "TRR estimate", "TRR CI-lower", "TRR CI-upper", "TRR I.sq", "TRR Tau.sq")

#table for IC and TRR combined ----
ALLtable <- matrix(c(
  AAT_IClist,	AAT_TRRlist,
  AMP_IClist,	AMP_TRRlist,
  BIAT_IClist,	BIAT_TRRlist,
  EAST_IClist,	EAST_TRRlist,
  EP_IClist,	EP_TRRlist,
  GNAT_IClist,	GNAT_TRRlist,
  IAT_IClist,	IAT_TRRlist,
  IATRF_IClist,	rep(NA,8),
  IRAP_IClist,	IRAP_TRRlist,
  NLE_IClist,	NLE_TRRlist,
  SCIAT_IClist,	SCIAT_TRRlist,
  SRCT_IClist,	rep(NA,8),
  STIAT_IClist,	STIAT_TRRlist), ncol = 16, byrow=T)
ALLtable <- ALLtable[,-9] # delete redundant measures column
colnames(ALLtable)<-c("Measure", "IC studies", "IC outcomes", "IC estimate", "IC CI-lower", "IC CI-upper", "IC I.sq", "IC Tau.sq", "TRR studies", "TRR outcomes", "TRR estimate", "TRR CI-lower", "TRR CI-upper", "TRR I.sq", "TRR Tau.sq")

# Computing all
(ALL_IC <- robu(ICyi ~ 1, sampleid, ICvi, data=all[all$reliability == "IC",]))
(ALL_TRR <- robu(TRRyi ~ 1, sampleid, TRRvi, data=all[all$reliability == "TRR",]))

inverseBonett(ALL_IC)
inverseZ(ALL_TRR)

# Exporting table ----
write.table(ALLtable, "C:/Users/Calvin/Documents/Dropbox/Projects/ARP/table.txt", sep="\t", row.names=F)
write.table(ICtable, "C:/Users/Calvin/Documents/Dropbox/Projects/ARP/ICtable.txt", sep="\t", row.names=F)
write.table(TRRtable, "C:/Users/Calvin/Documents/Dropbox/Projects/ARP/TRRtable.txt", sep="\t",row.names=F)

# Code for testing analyses excluding Nosek et al 2007 (large sample)
# Results didn't change much, just report with everyone
# all <- all %>% filter(articleid!=108)