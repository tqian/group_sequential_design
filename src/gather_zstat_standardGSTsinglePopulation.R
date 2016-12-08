############################
# Gather simulated trials from multiple files
#     Return a matrix of test statistics and their covariance matrix
#
############################



library(abind)

gather_zstat <- function(
    config,
    filenames
){
    
    num_stage <- config$num_stage
    
    n <- length(filenames)
    
    test <- readRDS(filenames[1])
    
    estimates <- test$estimates
    # if (estimate_covariance) {
    #     covariance_list <- test$covariance_list
    # }
    # rbind each resulting matrix to the overall matrix
    if (n >= 2) {
        for (i in 2:n){
            test <- readRDS(filenames[i])
            estimates <- abind(estimates, test$estimates, along = 2)
            # if (estimate_covariance) {
            #     covariance_list <- c(covariance_list, test$covariance_list)
            # }
        }
    }
    
    # convert zstat to a data frame
    zstat <- estimates["ATE", , ]
    
    # if (estimate_covariance) {
    #     tmp <- removeNA(num_stage, zstat, estimate_covariance, update_covariance, covariance_list = covariance_list)
    #     zstat <- tmp$zstat
    #     covariance_list <- tmp$covariance_list
    # } else {
    #     tmp <- removeNA(num_stage, zstat)
    #     zstat <- tmp$zstat
    # }
    
    
    # output standardized zstat and zstat_cov
    output <- list(estimate = estimates, cov_known = standardizeForZstat(config, zstat, cov(zstat)))
    
    # if (estimate_covariance) {
    #     for (i in 1:nrow(zstat)) {
    #         #         print(i)
    #         standardized_zstat <- standardizeForZstat(zstat[i, ], covariance_list[[i]], num_stage = num_stage, update_covariance = update_covariance)
    #         zstat[i, ] <- standardized_zstat$zstat
    #         covariance_error <- FALSE
    #         if (update_covariance) {
    #             for (stage in 1:num_stage) {
    #                 if (any(is.na(standardized_zstat$zstat_cov[1:(3 * stage), 1:(3 * stage), stage]))) {
    #                     covariance_error <- TRUE
    #                     break
    #                     # cat("Estimated covariance matrix is: \n")
    #                     # print(standardized_zstat$zstat_cov)
    #                     # stop("Missing value in estimatd covariance matrix.")
    #                 }
    #             }
    #         } else {
    #             if (any(is.na(standardized_zstat$zstat_cov))) {
    #                 covariance_error <- TRUE
    #                 # cat("Estimated covariance matrix is: \n")
    #                 # print(standardized_zstat$zstat_cov)
    #                 # stop("Missing value in estimatd covariance matrix.")
    #             }
    #         }
    #         if (!covariance_error) {
    #             covariance_list[[i]] <- makePositiveDefinite(standardized_zstat$zstat_cov, num_stage = num_stage, update_covariance = update_covariance)
    #         }
    #     }
    #     output <- c(output, list(cov_unknown = list(zstat = zstat, zstat_cov_list = covariance_list)))
    # }
    
    # if (estimate_covariance) {
    #     tmp <- removeNA(num_stage, zstat, estimate_covariance, update_covariance, covariance_list = covariance_list)
    #     zstat <- tmp$zstat
    #     covariance_list <- tmp$covariance_list
    # } else {
    #     tmp <- removeNA(num_stage, zstat)
    #     zstat <- tmp$zstat
    # }
    
    return(output)
}

# removeNA <- function(num_stage, zstat, estimate_covariance = FALSE, update_covariance = FALSE, covariance_list = NULL) {
#     # remove the trials with NA estimators (i.e. all patients are assigned to the same treatment arm)
#     
#     nrow_zstat <- nrow(zstat)
#     rowid_na <- which(rowSums(is.na(zstat)) > 0)
#     if (length(rowid_na) > 0) {
#         zstat <- zstat[-rowid_na, ]
#         cat("Removed ", length(rowid_na), " rows from ", nrow_zstat, " rows due to missingness.\n")
#         if (estimate_covariance) {
#             covariance_list <- covariance_list[-rowid_na]
#         }
#     }
#     
#     # remove the trials with variance estimates being zero for some estimators (i.e. same outcome value within arm at some stage)
#     if (estimate_covariance) {
#         if (update_covariance) {
#             id_varzero <- which(sapply(covariance_list, function(vcov) {
#                 num_stage <- dim(vcov)[3]
#                 
#                 for (stage in 1:num_stage) {
#                     if (any(is.na(vcov[1:stage, 1:stage, stage]))) {
#                         return(FALSE)
#                     } 
#                     if (stage == 1 &  vcov[1, 1, 1] < .Machine$double.eps^0.75) {
#                         return(TRUE)
#                     } else if (stage >= 2 & any(diag(vcov[1:stage, 1:stage, stage]) < .Machine$double.eps^0.75)) {
#                         return(TRUE)
#                     }
#                 }
#                 return(FALSE)
#             }))
#         } else {
#             id_varzero <- which(sapply(covariance_list, function(vcov) { return(any(diag(vcov) < .Machine$double.eps^0.75)) }))
#         }
#         n_trials <- length(covariance_list)
#         if (length(id_varzero) > 0) {
#             zstat <- zstat[-id_varzero, ]
#             covariance_list <- covariance_list[-id_varzero]
#             cat("Removed ", length(id_varzero), " rows from ", n_trials, " rows due to variance being zero.\n")
#         }
#     }
#     
#     # further remove trials with covariance involving NA (not sure what's the reason yet)
#     if (estimate_covariance) {
#         if (update_covariance) {
#             id_covna <- which(sapply(covariance_list, function(vcov) {
#                 num_stage <- dim(vcov)[3]
#                 for (stage in 1:num_stage) {
#                     if (stage == 1 & is.na(vcov[1, 1, 1]) ) {
#                         return(TRUE)
#                     } else if (stage >= 2 & any(is.na(vcov[1:stage, 1:stage, stage]))) {
#                         return(TRUE)
#                     }
#                 }
#                 return(FALSE) 
#             }))
#         } else {
#             id_covna <- which(sapply(covariance_list, function(vcov) { return(any(is.na(vcov))) }))
#         }
#         
#         n_trials <- length(covariance_list)
#         if (length(id_covna) > 0) {
#             zstat <- zstat[-id_covna, ]
#             covariance_list <- covariance_list[-id_covna]
#             cat("Removed ", length(id_covna), " rows from ", n_trials, " rows due to covariance NA (weird).\n")
#         }
#     }
#     
#     return(list(zstat = zstat, covariance_list = covariance_list))
# }

standardizeForZstat <- function(
    config,
    estimates, 
    covariance
) {
    
    num_stage <- config$num_stage
    
    # standardize estimates by its covariance matrix
    # if (update_covariance) {
    #     zstat <- rep(as.numeric(NA), 3 * num_stage)
    #     for (stage in 1:num_stage) {
    #         standardize_up_to_current_stage <- standardizeForZstat(estimates[1:(3 * stage)], covariance[1:(3 * stage), 1:(3 * stage), stage], num_stage, update_covariance = FALSE)
    #         zstat[(3 * stage - 2) : (3 * stage)] <- standardize_up_to_current_stage$zstat[(3 * stage - 2) : (3 * stage)]
    #         covariance[1:(3 * stage), 1:(3 * stage), stage] <- standardize_up_to_current_stage$zstat_cov[1:(3 * stage), 1:(3 * stage)]
    #     }
    #     return(list(zstat = zstat, zstat_cov = covariance))
    # } else {
        if (!is.matrix(estimates)) {
            estimates <- matrix(estimates, nrow = 1)
        }
        inv_std_dev_mat <- diag(diag(covariance) ^ (-1/2))
        zstat <- estimates %*% inv_std_dev_mat
        covariance <- inv_std_dev_mat %*% covariance %*% inv_std_dev_mat
        return(list(zstat = zstat, zstat_cov = covariance))
    # }
}

# makePositiveDefinite <- function(mat, num_stage, update_covariance = FALSE){
#     if (update_covariance) {
#         for (stage in 1:num_stage) {
#             mat[1:(3 * stage), 1:(3 * stage), stage] <- makePositiveDefinite(mat[1:(3 * stage), 1:(3 * stage), stage], stage, update_covariance = FALSE)
#         }
#         return(mat)
#     } else {
#         eigen_decomp <- eigen(mat)
#         if (all(eigen_decomp$value >= 0)) {
#             return(mat)
#         } else {
#             # modify mat so that it is positive semi definite
#             mat_approx <- eigen_decomp$vector %*% diag(eigen_decomp$value) %*% solve(eigen_decomp$vector)
#             
#             value_modified <- pmax(eigen_decomp$value, 0)
#             
#             mat_modified <- eigen_decomp$vector %*% diag(value_modified) %*% solve(eigen_decomp$vector)
#             mat_modified <- (mat_modified + t(mat_modified)) / 2
#             
#             return(mat_modified)
#             
#             # sd_ests <- diag(mat_modified)^(1/2)
#             # mat_std_modified <- mat_modified / outer(sd_ests, sd_ests)
#         }
#     }
# }


# gather_eval_parallel <- function(filenames){
#     
#     n <- length(filenames)
#     
#     
#     
#     load(filenames[1])
#     
#     gathered_result <- result
#     methods <- names(gathered_result)
#     ntrials_n1n2 <- length(result[[1]]$n1n2_b$SS_dist)
#     
#     if (n >= 2) {
#         for (i in 2:n) {
#             cat(i, " ")
#             load(filenames[i])
#             for (method in methods) {
#                 gathered_result[[method]]$performance_futBinding <- NULL
#                 gathered_result[[method]]$performance_futNotBinding <- NULL
#                 for (effectSetting in c("n1n2_b", "a1n2_b", "n1a2_b", "a1a2_b", "n1n2_nb", "a1n2_nb", "n1a2_nb", "a1a2_nb")){
#                     gathered_result[[method]][[effectSetting]]$SS_dist <- 
#                         c(gathered_result[[method]][[effectSetting]]$SS_dist, result[[method]][[effectSetting]]$SS_dist)
#                     gathered_result[[method]][[effectSetting]]$dur_dist <- 
#                         c(gathered_result[[method]][[effectSetting]]$dur_dist, result[[method]][[effectSetting]]$dur_dist)
#                     gathered_result[[method]][[effectSetting]]$trial_result <- 
#                         rbind(gathered_result[[method]][[effectSetting]]$trial_result, result[[method]][[effectSetting]]$trial_result)
#                     gathered_result[[method]][[effectSetting]]$effBounds <- 
#                         c(gathered_result[[method]][[effectSetting]]$effBounds, result[[method]][[effectSetting]]$effBounds)
#                 }
#             }
#             ntrials_n1n2 <- ntrials_n1n2 + length(result[[1]]$n1n2_b$SS_dist)
#         }
#     }
#     output <- list()
#     effBounds <- list()
#     for (method in methods) {
#         cat('\n', method, '\n')
#         tmp <- lapply(gathered_result[[method]],
#                       function(result_single_effect_setting) {
#                           print(paste0(length(complete.cases(result_single_effect_setting$trial_result)), "  ", nrow(result_single_effect_setting$trial_result)))
#                           result_vector <- c('E_SS' = mean(result_single_effect_setting$SS_dist, na.rm = T),
#                                              'E_dur' = mean(result_single_effect_setting$dur_dist, na.rm = T), # expected duration
#                                              'Pow_H0C' = mean(result_single_effect_setting$trial_result$reject_H0C, na.rm = T), # power to reject H0C
#                                              'Pow_H01' = mean(result_single_effect_setting$trial_result$reject_H01, na.rm = T), # power to reject H01
#                                              'Pow_all' = mean(result_single_effect_setting$trial_result$reject_H01 & result_single_effect_setting$trial_result$reject_H0C, na.rm = T),
#                                              'Pow_any' = mean(result_single_effect_setting$trial_result$reject_H01 | result_single_effect_setting$trial_result$reject_H0C, na.rm = T))
#                           num_stage <- NUM_STG_TMP
#                           reject_each_stage <- rep(as.numeric(NA), num_stage)
#                           for (stage in 1:num_stage) {
#                               reject_each_stage[stage] <- mean((result_single_effect_setting$trial_result$reject_H01 & result_single_effect_setting$trial_result$final_stage_subpop_1_enrolled_up_through == stage) |
#                                                                    (result_single_effect_setting$trial_result$reject_H0C & result_single_effect_setting$trial_result$final_stage_subpop_2_enrolled_up_through == stage),
#                                                                na.rm = T)
#                           }
#                           names(reject_each_stage) <- paste0("Pow_any_stg", 1:num_stage)
#                           return(c(result_vector, reject_each_stage))
#                       })
#         performance_futBinding <- rbind(tmp$n1n2_b, tmp$a1n2_b, tmp$n1a2_b, tmp$a1a2_b)
#         performance_futNotBinding <- rbind(tmp$n1n2_nb, tmp$a1n2_nb, tmp$n1a2_nb, tmp$a1a2_nb)
#         rownames(performance_futBinding) <- rownames(performance_futNotBinding) <- c("n1n2", "a1n2", "n1a2", "a1a2")
#         output <- c(output, list(list(performance_futBinding = performance_futBinding, performance_futNotBinding = performance_futNotBinding,
#                                       effBounds_examples = gathered_result[[method]]$n1n2_b$effBounds,
#                                       futBounds = gathered_result[[method]]$n1n2_b$futBounds[[1]])))
#         effBounds <- c(effBounds, list(lapply(gathered_result[[method]], function(result_single_effect_setting) {
#             return(result_single_effect_setting$effBounds)
#         })))
#     }
#     names(output) <- methods
#     names(effBounds) <- methods
#     
#     other_info <- list(ntrials_n1n2 = ntrials_n1n2,
#                        effBounds = effBounds)
#     
#     return(list(output = output, other_info = other_info))
# }