##############################
# Evaluate simulated trial results quickly
#
##############################


# Quick analysis ----------------------------------------------------------

rm(list = ls())

on_cluster <- T
if (on_cluster) {
    setwd("~/git_standardGST")
} else {
    setwd("~/Dropbox/Research/git_standardGST")
}

source("src/utility.R")

token <- "20160815_MISTIE_1pop"
source(paste0("config/", token, ".R"))
checkConfigForRequiredElements(config, config$trial_type)

npara <- 200
effect_setting <- c("n", "a")
name_list <- genFilename(config, token = token, effect_setting = effect_setting, npara = npara)

evaluated <- readRDS(name_list$ltmle$evaluated)



# Compare performance -----------------------------------------------------

rm(list = ls())
library(Hmisc)

unadj               <- readRDS("~/Dropbox/Research/git_standardGST/result/20160815_MISTIE_1pop_ss=160,320,480_nstg=3_erate=100/evaluated-20160815_MISTIE_1pop_ss=_nstg=3_erate=100-unadj.rds")
ltmle               <- readRDS("~/Dropbox/Research/git_standardGST/result/20160815_MISTIE_1pop_ss=160,320,480_nstg=3_erate=100/evaluated-20160815_MISTIE_1pop_ss=_nstg=3_erate=100-ltmle.rds")
MCAR_unadj          <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMCAR_ss=160,320,480_nstg=3_erate=100/evaluated-20160817_MISTIE_1pop_dropoutMCAR_ss=160,320,480_nstg=3_erate=100-unadj.rds")
MCAR_ltmle          <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMCAR_ss=160,320,480_nstg=3_erate=100/evaluated-20160817_MISTIE_1pop_dropoutMCAR_ss=160,320,480_nstg=3_erate=100-ltmle.rds")
MAR_unadj           <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMAR_ss=160,320,480_nstg=3_erate=100/evaluated-20160817_MISTIE_1pop_dropoutMAR_ss=160,320,480_nstg=3_erate=100-unadj.rds")
MAR_ltmle           <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMAR_ss=160,320,480_nstg=3_erate=100/evaluated-20160817_MISTIE_1pop_dropoutMAR_ss=160,320,480_nstg=3_erate=100-ltmle.rds")
MCAR_unadj_largep   <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMCAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/evaluated-20160817_MISTIE_1pop_dropoutMCAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-unadj.rds")
MCAR_ltmle_largep   <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMCAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/evaluated-20160817_MISTIE_1pop_dropoutMCAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-ltmle.rds")
MAR_unadj_largep    <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/evaluated-20160817_MISTIE_1pop_dropoutMAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-unadj.rds")
MAR_ltmle_largep    <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/evaluated-20160817_MISTIE_1pop_dropoutMAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-ltmle.rds")
MNAR_unadj_largep   <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMNAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/evaluated-20160817_MISTIE_1pop_dropoutMNAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-unadj.rds")
MNAR_ltmle_largep   <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMNAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/evaluated-20160817_MISTIE_1pop_dropoutMNAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-ltmle.rds")

unadj_hetero        <- readRDS("~/Dropbox/Research/git_standardGST/result/20160818_MISTIE_1pop_TEhetero_ss=160,320,480_nstg=3_erate=100/evaluated-20160818_MISTIE_1pop_TEhetero_ss=160,320,480_nstg=3_erate=100-unadj.rds")
ltmle_hetero        <- readRDS("~/Dropbox/Research/git_standardGST/result/20160818_MISTIE_1pop_TEhetero_ss=160,320,480_nstg=3_erate=100/evaluated-20160818_MISTIE_1pop_TEhetero_ss=160,320,480_nstg=3_erate=100-ltmle.rds")
unadj_hetero_MAR    <- readRDS("~/Dropbox/Research/git_standardGST/result/20160818_MISTIE_1pop_TEhetero_MAR_ss=160,320,480_nstg=3_erate=100/evaluated-20160818_MISTIE_1pop_TEhetero_MAR_ss=160,320,480_nstg=3_erate=100-unadj.rds")
ltmle_hetero_MAR    <- readRDS("~/Dropbox/Research/git_standardGST/result/20160818_MISTIE_1pop_TEhetero_MAR_ss=160,320,480_nstg=3_erate=100/evaluated-20160818_MISTIE_1pop_TEhetero_MAR_ss=160,320,480_nstg=3_erate=100-ltmle.rds")

l <- llist(unadj, ltmle, MCAR_unadj, MCAR_ltmle, MCAR_unadj_largep, MCAR_ltmle_largep,
           MAR_unadj, MAR_ltmle, MAR_unadj_largep, MAR_ltmle_largep, MNAR_unadj_largep, MNAR_ltmle_largep,
           unadj_hetero, ltmle_hetero, unadj_hetero_MAR, ltmle_hetero_MAR)

sapply(l, function(e) round(e$performance["n_nb",], 3))
sapply(l, function(e) round(e$performance["a_b",], 3))



 # Compare consistency -----------------------------------------------------

rm(list = ls())
library(Hmisc)

unadj               <- readRDS("~/Dropbox/Research/git_standardGST/result/20160815_MISTIE_1pop_ss=160,320,480_nstg=3_erate=100/gathered-20160815_MISTIE_1pop_ss=_nstg=3_erate=100-unadj.rds")
ltmle               <- readRDS("~/Dropbox/Research/git_standardGST/result/20160815_MISTIE_1pop_ss=160,320,480_nstg=3_erate=100/gathered-20160815_MISTIE_1pop_ss=_nstg=3_erate=100-ltmle.rds")
MCAR_unadj          <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMCAR_ss=160,320,480_nstg=3_erate=100/gathered-20160817_MISTIE_1pop_dropoutMCAR_ss=160,320,480_nstg=3_erate=100-unadj.rds")
MCAR_ltmle          <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMCAR_ss=160,320,480_nstg=3_erate=100/gathered-20160817_MISTIE_1pop_dropoutMCAR_ss=160,320,480_nstg=3_erate=100-ltmle.rds")
MAR_unadj           <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMAR_ss=160,320,480_nstg=3_erate=100/gathered-20160817_MISTIE_1pop_dropoutMAR_ss=160,320,480_nstg=3_erate=100-unadj.rds")
MAR_ltmle           <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMAR_ss=160,320,480_nstg=3_erate=100/gathered-20160817_MISTIE_1pop_dropoutMAR_ss=160,320,480_nstg=3_erate=100-ltmle.rds")
MCAR_unadj_largep   <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMCAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/gathered-20160817_MISTIE_1pop_dropoutMCAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-unadj.rds")
MCAR_ltmle_largep   <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMCAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/gathered-20160817_MISTIE_1pop_dropoutMCAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-ltmle.rds")
MAR_unadj_largep    <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/gathered-20160817_MISTIE_1pop_dropoutMAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-unadj.rds")
MAR_ltmle_largep    <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/gathered-20160817_MISTIE_1pop_dropoutMAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-ltmle.rds")
MNAR_unadj_largep   <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMNAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/gathered-20160817_MISTIE_1pop_dropoutMNAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-unadj.rds")
MNAR_ltmle_largep   <- readRDS("~/Dropbox/Research/git_standardGST/result/20160817_MISTIE_1pop_dropoutMNAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100/gathered-20160817_MISTIE_1pop_dropoutMNAR_largeDropoutProb_ss=320,640,960_nstg=3_erate=100-ltmle.rds")
unadj_hetero        <- readRDS("~/Dropbox/Research/git_standardGST/result/20160818_MISTIE_1pop_TEhetero_ss=160,320,480_nstg=3_erate=100/gathered-20160818_MISTIE_1pop_TEhetero_ss=160,320,480_nstg=3_erate=100-unadj.rds")
ltmle_hetero        <- readRDS("~/Dropbox/Research/git_standardGST/result/20160818_MISTIE_1pop_TEhetero_ss=160,320,480_nstg=3_erate=100/gathered-20160818_MISTIE_1pop_TEhetero_ss=160,320,480_nstg=3_erate=100-ltmle.rds")
unadj_hetero_MAR    <- readRDS("~/Dropbox/Research/git_standardGST/result/20160818_MISTIE_1pop_TEhetero_MAR_ss=160,320,480_nstg=3_erate=100/gathered-20160818_MISTIE_1pop_TEhetero_MAR_ss=160,320,480_nstg=3_erate=100-unadj.rds")
ltmle_hetero_MAR    <- readRDS("~/Dropbox/Research/git_standardGST/result/20160818_MISTIE_1pop_TEhetero_MAR_ss=160,320,480_nstg=3_erate=100/gathered-20160818_MISTIE_1pop_TEhetero_MAR_ss=160,320,480_nstg=3_erate=100-ltmle.rds")

l <- llist(unadj, ltmle, MCAR_unadj, MCAR_ltmle, MCAR_unadj_largep, MCAR_ltmle_largep,
           MAR_unadj, MAR_ltmle, MAR_unadj_largep, MAR_ltmle_largep, MNAR_unadj_largep, MNAR_ltmle_largep,
           unadj_hetero, ltmle_hetero, unadj_hetero_MAR, ltmle_hetero_MAR)

sapply(l, function(e) mean(e$n$estimate["ATE", , 3]))
sapply(l, function(e) mean(e$n$estimate["treatment", , 3]))
sapply(l, function(e) mean(e$n$estimate["control", , 3]))

sapply(l, function(e) mean(e$a$estimate["ATE", , 3]))
sapply(l, function(e) mean(e$a$estimate["treatment", , 3]))
sapply(l, function(e) mean(e$a$estimate["control", , 3]))

sapply(l, function(e) sd(e$n$estimate["ATE", , 3]))
sapply(l, function(e) sd(e$n$estimate["treatment", , 3]))
sapply(l, function(e) sd(e$n$estimate["control", , 3]))

sapply(l, function(e) sd(e$a$estimate["ATE", , 3]))
sapply(l, function(e) sd(e$a$estimate["treatment", , 3]))
sapply(l, function(e) sd(e$a$estimate["control", , 3]))
