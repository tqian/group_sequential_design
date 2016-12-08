##################################################
# Simulate trials and compute test statistics
#     Interim analysis at fixed sample size
#
##################################################

library(ltmle)

if (0) {
    # debugging
    token <- "20160815_MISTIE_1pop"
    source(paste0("config/", token, ".R"))
    
    tmp <- simulateTrial_fixedTimeSS_standardGSTsinglePopulation(config, effect_setting = "n", nsim = 5, estimator = "ltmle", print_sample_size_table = TRUE)
}


simulateTrial_fixedTimeSS_standardGSTsinglePopulation <- function(
    config,
    effect_setting = c("n", "a"),
    nsim,
    estimator = c("unadj", "ltmle_package"), # do not use "ltmle_own"
    print_sample_size_table = FALSE
){
    effect_setting  <- match.arg(effect_setting)
    estimator       <- match.arg(estimator)
    
    cumss           <- config$cumss
    num_stage       <- config$num_stage
    enrollment_rate <- config$enrollment_rate
    delay_WL        <- config$delay_WL
    delay_LY        <- config$delay_LY
    W_to_use        <- config$W_to_use
    L_to_use        <- config$L_to_use
    dgm             <- config$dgm
    
    # estimate_covariance         <- falseIfNull(config$estimate_covariance) # whether estimate covariance structure in simulation
    # update_covariance           <- falseIfNull(config$update_covariance) # whether update earlier covariance estimate at later stages
    # estimate_covariance_method  <- falseIfNull(config$estimate_covariance_method) # c("nonboot", "boot")
    # nboot                       <- falseIfNull(config$nboot)
    
    estimates <- array(as.numeric(NA), dim = c(3, nsim, num_stage),
                       dimnames = list(c("ATE", "treatment", "control"),
                                       NULL,
                                       1:num_stage))
    # if (estimate_covariance)
    #     covariance_list <- list()
    
    # simulate trials to get test statistics
    for (isim in 1:nsim){
        if( isim %% 10 == 0)
            cat(paste0("\nTrial number ", isim))
        
        dt <- dgm(config, effect_setting)
        
        interim_time <- sort(dt$T_final)[cumss]
        
        if (print_sample_size_table) {
            ss_table <- getSampleSizeTable(config, dt = dt, interim_time = interim_time)
            cat("\n Interim_time: ", paste(round(interim_time, 2), sep = ", "), "\n")
            print_ss_table(ss_table)
        }
        estimate_and_variance <- getEstimateAndVarianceForSingleTrial(config, dt = dt, interim_time = interim_time, estimator = estimator)
        
        estimates[, isim, ] <- estimate_and_variance$est
        # if (estimate_covariance) {
        #     covariance_list <- c(covariance_list, list(estimate_and_variance$covariance_matrix))
        # }
    }
    
    # if (estimate_covariance) {
    #     return(list(estimates = estimates, covariance_list = covariance_list))
    # } else {
    #     return(list(estimates = estimates))
    # }
    return(list(estimates = estimates))
}

if (0){
    getEstimateAndVarianceForSingleTrial(    dt, num_stage, interim_time, p1, p2 = p1, W_to_use, L_to_use, estimator = "unadj",
                                             estimate_covariance = TRUE, update_covariance = FALSE, 
                                             estimate_covariance_method = "nonboot")
    
    getEstimateAndVarianceForSingleTrial(    dt, num_stage, interim_time, p1, p2 = p1, W_to_use, L_to_use, estimator = "unadj",
                                             estimate_covariance = TRUE, update_covariance = FALSE, 
                                             estimate_covariance_method = "boot")
}

getEstimateAndVarianceForSingleTrial <- function(
    config,
    dt,
    interim_time,
    estimator = c("unadj", "ltmle_package")
) {
    
    estimator <- match.arg(estimator)
    
    test_stat <- getEstimateForSingleHypo(config, dt = dt, interim_time = interim_time, estimator = estimator)
    return(test_stat)
    
    # if (estimate_covariance & estimate_covariance_method == "boot") {
    #     
    #     test_stat <- getEstimateForSingleHypo(config, dt = dt, interim_time = interim_time, estimator = estimator)
    # 
    #     output <- list(est = test_stat$estimates)
    #     
    #     vcov_array <- array(as.numeric(NA), dim = c(num_stage, num_stage, num_stage))
    #     
    #     for (stage in 1:num_stage) {
    #         boot_out <- boot(dt, getEstimateUpToCurrentStage_boot, R = nboot,
    #                          config = config, curr_stage = stage, interim_time = interim_time, estimator = estimator)
    #         vcov_array[1:stage, 1:stage, stage] <- var(boot_out$t)
    #     }
    #     output <- c(output, list(covariance_matrix = vcov_array))
    #     return(output)
    # } else {
    #     # not using bootstrap, previous code without change
    #     
    #     estimator <- match.arg(estimator)
    #     if (!estimate_covariance) {
    #         test_stat <- getEstimateForSingleHypo(config, dt = dt, interim_time = interim_time, estimator = estimator)
    #         return(test_stat)
    #     } else {
    #         
    #         ##### TO CONTINUE #####
    #         ss_tables <- getSampleSizeTable(dt, num_stage, interim_time)
    #         test_stat_1 <- getEstimateForSingleHypo(dt_subpop1, interim_time, W_to_use, L_to_use, estimator = estimator, estimate_covariance = TRUE, update_covariance = update_covariance,
    #                                                 curr_hypo = "H01", ss_Y_obs_table = ss_tables$ss_Y_obs_table, ss_L_obs_table = ss_tables$ss_L_obs_table, ss_W_obs_table = ss_tables$ss_W_obs_table)
    #         test_stat_2 <- getEstimateForSingleHypo(dt_subpop2, interim_time, W_to_use, L_to_use, estimator = estimator, estimate_covariance = TRUE, update_covariance = update_covariance,
    #                                                 curr_hypo = "H02", ss_Y_obs_table = ss_tables$ss_Y_obs_table, ss_L_obs_table = ss_tables$ss_L_obs_table, ss_W_obs_table = ss_tables$ss_W_obs_table)
    #         
    #         vcov_1 <- test_stat_1$cov
    #         vcov_2 <- test_stat_2$cov
    #         if (estimator %in% c("unadj", "ltmle_package")) {
    #             # construct vcov for subpop_1 and for subpop_2 separately
    #             ss_Y_obs <- ss_tables$ss_Y_obs_table["any", , ]
    #             vcov_1 <- expandVarToCovSingleHypo(vcov_1, ss_Y_obs["H01", ], update_covariance = update_covariance)
    #             vcov_2 <- expandVarToCovSingleHypo(vcov_2, ss_Y_obs["H02", ], update_covariance = update_covariance)
    #         }
    #         
    #         vcov_matrix <- expandFullCovMultipleHypo(vcov_1, vcov_2, p1, p2, num_stage, update_covariance = update_covariance)
    #     }
    #     
    #     output <- list(est_subpop_1 = test_stat_1$estimates,
    #                    est_subpop_2 = test_stat_2$estimates,
    #                    est_subpop_c = p1 * test_stat_1$estimates + p2 * test_stat_2$estimates)
    #     if (estimate_covariance) {
    #         output <- c(output, list(covariance_matrix = vcov_matrix))
    #     }
    #     return(output)
    # }
}


# getEstimateUpToCurrentStage_boot <- function(dt, index, ...) {
#     return(getEstimateUpToCurrentStage(dt[index, ], ...))
# }

# getEstimateUpToCurrentStage <- function(dt, num_stage, curr_stage, p1, p2 = p1, interim_time,
#                                         ... # arguments passed into getEstimateForSingleHypo()
#                                         # W_to_use, L_to_use, estimator
#                                         ){
# 
#     # copied and modified from getEstimateForSingleHypo
#     estimates <- matrix(as.numeric(NA), nrow = 3, ncol = curr_stage)
#     # row order: H0C, H01, H02
#     
#     dt <- subset(dt, T_final <= interim_time[curr_stage])
#     
#     estimates[2, ] <- getEstimateForSingleHypo(subset(dt, subpop == 1), num_stage = curr_stage, estimate_covariance = FALSE, interim_time = interim_time,
#                                                ...)$estimates["ATE", ]
#     estimates[3, ] <- getEstimateForSingleHypo(subset(dt, subpop == 2), num_stage = curr_stage, estimate_covariance = FALSE, interim_time = interim_time,
#                                                ...)$estimates["ATE", ]
#     estimates[1, ] <- p1 * estimates[2, ] + p2 * estimates[3, ]
#    
#     return(as.vector(estimates))
# }
# if (0) {
#     # getEstimateUpToCurrentStage(dt, 5, 5, p1, p2 = 1-p1, W_to_use = W_to_use, L_to_use = L_to_use, estimator = "unadj", interim_time = interim_time)
#     # getEstimateUpToCurrentStage(dt, 5, 3, p1, p2 = 1-p1, W_to_use = W_to_use, L_to_use = L_to_use, estimator = "unadj", interim_time = interim_time)
#     
# }


getEstimateForSingleHypo <- function(
    config,
    dt,
    interim_time,
    estimator = c("unadj", "ltmle_package")
){
    
    estimator <- match.arg(estimator)
    
    num_stage       <- config$num_stage
    W_to_use        <- config$W_to_use
    L_to_use        <- config$L_to_use
    
    estimates <- array(as.numeric(NA), dim = c(3, num_stage),
                       dimnames = list(c("ATE", "treatment", "control"), 1:num_stage))
    # if (estimate_covariance) {
    #     if (update_covariance) {
    #         cov_estimate <- array(as.numeric(NA), dim = c(num_stage, num_stage, num_stage))
    #         n0_each_stage <- n1_each_stage <- rep(as.numeric(NA), num_stage)
    #     } else {
    #         cov_estimate <- matrix(as.numeric(NA), nrow = num_stage, ncol = num_stage)
    #     }
    # }
    
    for (stage in 1:num_stage){
        time <- interim_time[stage]
        
        C0 <- as.numeric( dt$T_enroll    <= time)                          # W observed
        C1 <- as.numeric((dt$T_shortterm <= time) & dt$observed_shortterm) # L observed
        C2 <- as.numeric((dt$T_final     <= time) & dt$observed_final    ) # Y observed
        
        # construct the data frame to be passed into ltmle()
        dt_CWALY <- data.frame(C0, dt[, W_to_use], dt$A, C1, dt[, L_to_use], C2, dt$Y)
        colnames(dt_CWALY) <- c("C0", W_to_use, "A", "C1", L_to_use, "C2", "Y")
        
        if (all(as.logical(dt_CWALY$A[dt_CWALY$C2==1])) | !any(as.logical(dt_CWALY$A[dt_CWALY$C2==1])))
            stop("All observed Y are in one treatment arm.")
        
        if (estimator == "unadj") {
            id_yobs_a1 <- which(dt_CWALY$C2 == 1 & dt_CWALY$A == 1)
            id_yobs_a0 <- which(dt_CWALY$C2 == 1 & dt_CWALY$A == 0)
            estimates["treatment", stage] <- mean(dt_CWALY$Y[id_yobs_a1])
            estimates["control", stage] <- mean(dt_CWALY$Y[id_yobs_a0])
            estimates["ATE", stage] <- estimates["treatment", stage] - estimates["control", stage]
            # if (estimate_covariance) {
            #     n1 <- length(id_yobs_a1)
            #     n0 <- length(id_yobs_a0)
            #     var_Y1 <- var(dt_CWALY$Y[id_yobs_a1])
            #     var_Y0 <- var(dt_CWALY$Y[id_yobs_a0])
            #     if (update_covariance) {
            #         n0_each_stage[stage] <- n0
            #         n1_each_stage[stage] <- n1
            #         for (istage in 1:stage) {
            #             cov_estimate[istage, istage, stage] <- var_Y1 / n1_each_stage[istage] + var_Y0 / n0_each_stage[istage]
            #         }
            #     } else {
            #         cov_estimate[stage, stage] <- var_Y1 / n1 + var_Y0 / n0
            #     }
            # }
        # } else if (estimator == "ltmle_own") {
        #     ltmle_fit <- ltmle_own_fcn(dt_CWALY, Anodes = "A",  Cnodes = c("C0","C1","C2"), Wnodes = W_to_use, Lnodes = L_to_use, Ynodes = "Y",
        #                                ICvar = estimate_covariance, curr_stg = stage, ...)
        #     estimates["ATE", stage] <- ltmle_fit$estimate["ATE"]
        #     estimates["treatment", stage] <- ltmle_fit$estimate["treatment"]
        #     estimates["control", stage] <- ltmle_fit$estimate["control"]
        #     if (estimate_covariance) {
        #         if (update_covariance) {
        #             cov_estimate[1:stage, 1:stage, stage] <- ltmle_fit$covariance[1:stage, 1:stage]
        #         } else {
        #             cov_estimate[1:stage, stage] <- ltmle_fit$covariance[1:stage, stage]
        #             cov_estimate[stage, 1:stage] <- ltmle_fit$covariance[stage, 1:stage]
        #         }
        # 
        #     }
        } else { # ltmle_package
            suppressMessages(ltmle_fit <- ltmle(dt_CWALY, 
                                                Anodes = "A", Cnodes = c("C0","C1","C2"), Lnodes = c(W_to_use, L_to_use), Ynodes = "Y",
                                                abar = list(1, 0), estimate.time = FALSE, IC.variance.only = TRUE))
            if (summary(ltmle_fit)$transformOutcome) {
                Yrange <- attr(summary(ltmle_fit)$transformOutcome, "Yrange")
                Yleft <- Yrange[1]
                Yrange <- Yrange[2] - Yrange[1]
            } else { # if not transformed
                Yrange <- 1
                Yleft <- 0
            }
            estimates["ATE", stage] <- as.numeric(summary(ltmle_fit)$effect.measures$ATE$estimate) * Yrange
            estimates["treatment", stage] <- as.numeric(summary(ltmle_fit)$effect.measures$treatment$estimate) * Yrange + Yleft
            estimates["control", stage] <- as.numeric(summary(ltmle_fit)$effect.measures$control$estimate) * Yrange + Yleft
            # if (estimate_covariance) {
            #     if (update_covariance) {
            #         # for ltmle_package, covariance estimate at earlier stages is not actually updated
            #         for (istage in stage:num_stage){
            #             cov_estimate[stage, stage, istage] <- as.numeric(summary(ltmle_fit)$effect.measures$ATE$std.dev)^2 * Yrange^2
            #         }
            #     } else {
            #         cov_estimate[stage, stage] <- as.numeric(summary(ltmle_fit)$effect.measures$ATE$std.dev)^2 * Yrange^2
            #     }
            # 
            # }
            
            
            #       if (inter_AW_ltmle) {
            #         # Currently this only works for:
            #         # interaction terms are W3*A and W4*A, W_to_use = c("W3", "W4"), L_to_use = NULL
            #         gform <- c("C0 ~ 1", "A ~ W3 + W4", "C1 ~ W3*A + W4*A", "C2 ~ W3*A + W4*A")
            #         Qform <- c(W3="Q.kplus1 ~ 1", W4="Q.kplus1 ~ W3", 
            #                    #                  L="Q.kplus1 ~ W3*A + W4*A", 
            #                    Y="Q.kplus1 ~ W3*A + W4*A")
            #       } else {
            #         gform <- NULL
            #         Qform <- NULL
            #       }
        }
    }
    
    # if (estimate_covariance) {
    #     return(list(estimates = estimates, cov = cov_estimate))
    # } else {
    #     return(list(estimates = estimates))
    # }
    return(list(estimates = estimates))
}



getSampleSizeTable <- function(
    config,
    dt,
    interim_time
){
    
    num_stage <- config$num_stage
    
    ss_Y_obs_table <- ss_L_obs_table <- ss_W_obs_table <-
        array(dim = c(3, 3, num_stage), dimnames = list(c("treatment", "control", "any"), c("H0C", "H01", "H02"), 1:num_stage))
    for (stage in 1:num_stage) {
        C0 <- as.numeric(dt$T_enroll <= interim_time[stage])
        C1 <- as.numeric(dt$T_shortterm <= interim_time[stage])
        C2 <- as.numeric(dt$T_final <= interim_time[stage])
        for (trt in c("treatment", "control", "any")) {
            if (trt == "treatment") {
                this_treatment <- dt$A == 1
            } else if (trt == "control") {
                this_treatment <- dt$A == 0
            } else {
                this_treatment <- rep(1, nrow(dt))
            }
            for (hypo in c("H0C", "H01", "H02")) {
                if (hypo == "H01") {
                    this_hypo <- dt$subpop == 1
                } else if (hypo == "H02") {
                    this_hypo <- dt$subpop == 2
                } else {
                    this_hypo <- rep(1, nrow(dt))
                }
                this_treatment_hypo <- this_treatment & this_hypo
                ss_Y_obs_table[trt, hypo, stage] <- sum(C2 & this_treatment_hypo)
                ss_L_obs_table[trt, hypo, stage] <- sum(C1 & this_treatment_hypo)
                ss_W_obs_table[trt, hypo, stage] <- sum(C0 & this_treatment_hypo)
            }
        }
    }
    return(list(ss_Y_obs_table = ss_Y_obs_table,
                ss_L_obs_table = ss_L_obs_table,
                ss_W_obs_table = ss_W_obs_table))
}


print_ss_table <- function(ss_table) {
    cat("\n### Y_obs_table ###\n")
    cat("  Treatment + Control\n")
    print(ss_table$ss_Y_obs_table["any", , ])
    cat("\n  Treatment\n")
    print(ss_table$ss_Y_obs_table["treatment", , ])
    cat("\n  Control\n")
    print(ss_table$ss_Y_obs_table["control", , ])
    
    cat("\n### L_obs_table ###\n")
    cat("  Treatment + Control\n")
    print(ss_table$ss_L_obs_table["any", , ])
    cat("\n  Treatment\n")
    print(ss_table$ss_L_obs_table["treatment", , ])
    cat("\n  Control\n")
    print(ss_table$ss_L_obs_table["control", , ])
    
    cat("\n### W_obs_table ###\n")
    cat("  Treatment + Control\n")
    print(ss_table$ss_W_obs_table["any", , ])
    cat("\n  Treatment\n")
    print(ss_table$ss_W_obs_table["treatment", , ])
    cat("\n  Control\n")
    print(ss_table$ss_W_obs_table["control", , ])
}


# expandVarToCovSingleHypo <- function(vcov, nobs_Y, update_covariance = TRUE) {
#     if (update_covariance) {
#         num_stage <- dim(vcov)[3]
#         for (stage in 1:num_stage) {
#             vcov[1:stage, 1:stage, stage] <- expandVarToCovSingleHypo(vcov[1:stage, 1:stage, stage], nobs_Y, update_covariance = FALSE)
#         }
#     } else {
#         if (!is.matrix(vcov)) {
#             vcov <- diag(vcov, nrow = length(vcov), ncol = length(vcov))
#         }
#         variance <- diag(vcov)
#         for (i in 1:nrow(vcov)) {
#             for (j in 1:ncol(vcov)) {
#                 bigger_ij <- max(i, j)
#                 smaller_ij <- min(i, j)
#                 vcov[i, j] <- nobs_Y[smaller_ij] / nobs_Y[bigger_ij] * variance[smaller_ij]
#             }
#         }
#     }
#     return(vcov)
# }
# 
# expandFullCovMultipleHypo <- function(vcov_1, vcov_2, p1, p2, num_stage, update_covariance = TRUE) {
#     
#     if (update_covariance) {
#         vcov_array <- array(as.numeric(NA), dim = c(3 * num_stage, 3 * num_stage, num_stage))
#         for (stage in 1:num_stage) {
#             vcov_array[1:(3*stage), 1:(3*stage), stage] <- expandFullCovMultipleHypo(vcov_1[1:stage, 1:stage, stage], vcov_2[1:stage, 1:stage, stage],
#                                                                              p1, p2, num_stage = stage, update_covariance = FALSE)
#         }
#         return(vcov_array)
#     } else {
#         # see documentation/variance_and_covariance_estimation.pdf for explanation
#         if (!is.matrix(vcov_1)) {
#             vcov_1 <- diag(vcov_1, nrow = length(vcov_1), ncol = length(vcov_1))
#         }
#         if (!is.matrix(vcov_2)) {
#             vcov_2 <- diag(vcov_2, nrow = length(vcov_2), ncol = length(vcov_2))
#         }
#         vcov <- list(vcov_1, vcov_2)
#         p12 <- c(p1, p2)
#         cov_df <- expand.grid(0:2, 0:2, 1:num_stage, 1:num_stage)
#         names(cov_df) <- c("s", "s_", "k", "k_")
#         cov_df$row_in_matrix <- cov_df$s + 3 * cov_df$k - 2
#         cov_df$col_in_matrix <- cov_df$s_ + 3 * cov_df$k_ - 2
#         cov_df$cov <- 0
#         for (irow in 1:nrow(cov_df)) {
#             s <- cov_df$s[irow]; s_ <- cov_df$s_[irow]; k <- cov_df$k[irow]; k_ <- cov_df$k_[irow]
#             if (s == 0 & s_ == 0) {
#                 cov_df$cov[irow] <- p1^2 * vcov[[1]][k, k_] + p2^2 * vcov[[2]][k, k_]
#             } else if (s == s_) {
#                 cov_df$cov[irow] <- vcov[[s]][k, k_]
#             } else if (s == 0) {
#                 cov_df$cov[irow] <- p12[s_] * vcov[[s_]][k, k_]
#             } else if (s_ == 0) {
#                 cov_df$cov[irow] <- p12[s] * vcov[[s]][k, k_]
#             }
#         }
#         vcov_matrix <- matrix(as.numeric(NA), nrow = 3 * num_stage, ncol = 3 * num_stage)
#         for (irow in 1:nrow(cov_df)) {
#             vcov_matrix[cov_df$row_in_matrix[irow], cov_df$col_in_matrix[irow]] <- cov_df$cov[irow]
#         }
#         return(vcov_matrix)
#     }
# }
