###################################
# Evaluate Trials
#    Outputs Type I error, power, etc for simulated test statistics and covariance matrix
#
###################################


# see the end of the code for example 

library(mvtnorm)


trialPerformance_fixedTimeSS_singlepop <- function(
    config,
    zstat_and_cov_multi_effect_setting
) {
    
    n_nb <- trialPerformance_fixedTimeSS_singlepop_singleEffectSetting(config, zstat_and_cov_multi_effect_setting[["n"]], futBinding = FALSE)
    n_b  <- trialPerformance_fixedTimeSS_singlepop_singleEffectSetting(config, zstat_and_cov_multi_effect_setting[["n"]], futBinding = TRUE)
    a_nb <- trialPerformance_fixedTimeSS_singlepop_singleEffectSetting(config, zstat_and_cov_multi_effect_setting[["a"]], futBinding = FALSE)
    a_b  <- trialPerformance_fixedTimeSS_singlepop_singleEffectSetting(config, zstat_and_cov_multi_effect_setting[["a"]], futBinding = TRUE)
    
    performance <- rbind(n_b$performance, a_b$performance, n_nb$performance, a_nb$performance)
    rownames(performance) <- c("n_b", "a_b", "n_nb", "a_nb")
    return(list(performance = performance,
                n_nb = n_nb, n_b = n_b, a_nb = a_nb, a_b = a_b))
}



trialPerformance_fixedTimeSS_singlepop_singleEffectSetting <- function(
    config,
    zstat_and_cov,
    futBinding
){
    cumss           <- config$cumss
    num_stage       <- config$num_stage
    enrollment_rate <- config$enrollment_rate
    delay_WL        <- config$delay_WL
    delay_LY        <- config$delay_LY
    
    delay_WY <- delay_WL + delay_LY
    
    ### DETERMINE TRIAL OUTCOME ###
    
    # true covariance known
    # using true covariance, evaluate all trials at once (boundaries will be the same for all trials)
    trial_result <- evaluateTrial(config, zstat_and_cov, futBinding)
    
    effBounds <- trial_result$effBounds
    futBounds <- trial_result$futBounds
    trial_result <- trial_result$result
 
    
    ### COMPUTE TRIAL PERFORMANCE ###
    
    pipeline_participant <- delay_WY * enrollment_rate
    maxss <- cumss[num_stage]
    maxdur <- cumss[num_stage]
    
    #Distributions for sample size & duration
    SS_dist <- pmin(cumss[trial_result$final_stage_enrolled_up_through] + pipeline_participant, maxss)
    
    dur_dist <- cumss[trial_result$final_stage_enrolled_up_through] / enrollment_rate +
        ifelse(trial_result$final_stage_enrolled_up_through == num_stage, delay_WY, 0)
    
    print(paste0(length(complete.cases(trial_result)), "  ", nrow(trial_result)))
    
    reject_each_stage <- rep(as.numeric(NA), num_stage)
    for (stage in 1:num_stage) {
        reject_each_stage[stage] <- mean((trial_result$reject) & (trial_result$final_stage_enrolled_up_through == stage))
    }
    names(reject_each_stage) <- paste0("Pow_any_stg", 1:num_stage)
    
    return(list(performance=c(
        'E_SS' = mean(SS_dist),
        'E_dur' = mean(dur_dist), # expected duration
        'Pow' = mean(trial_result$reject)),
        'reject_each_stage' = reject_each_stage,
        'effBounds' = effBounds,
        'futBounds' = futBounds,
        SS_dist = SS_dist, #Full distribution of SS
        dur_dist = dur_dist, #Full distribution of trial duration
        trial_result = trial_result
    ))
}



### Determine the outcome of simulated trials ###
evaluateTrial <- function(
    config,
    zstat_and_cov, # covariance matrix for zstat
    futBinding
){
    
    num_stage <- config$num_stage
    futBounds <- config$futBounds
    FWER      <- config$FWER
    power_ESF <- config$power_ESF
    
    estimate  <- zstat_and_cov$estimate
    zstat     <- zstat_and_cov$cov_known$zstat
    cov_known <- zstat_and_cov$cov_known$zstat_cov
    
    info_level <- diag(cov(estimate["ATE", , ]))^-1
    eff_allocated <- power_ESF(info_level, FWER)
    
    ### GET EFFICACY AND FUTILITY BOUNDARIES ###
    
    # get effBounds
    effBounds <- getEffBoundsSequentially(eff_allocated, rep(0, num_stage), cov_known,
                                         abseps = 0.000001, maxpts = 500000, eff_cut = 1e-10)
    # get futBounds
    if (!futBinding) {
        futBounds <- rep(-Inf, length(futBounds))
    }
    
    ### MULTIPLE TESTING PROCEDURE - PREPARATION ###
    
    nsim <- nrow(zstat)

    enroll_stopped <- rep(0, nsim)
    reject <- rep(0, nsim)
    final_stage_enrolled_up_through <- rep(num_stage, nsim)
    
    ### MULTIPLE TESTING PROCEDURE ###
    
    for(stage in 1:num_stage) {
        reject <- ifelse((!enroll_stopped) & (zstat[, stage] > effBounds[stage]), 1, reject)
        enroll_stopped <- ifelse(reject | (zstat[, stage] < futBounds[stage]) | (stage == num_stage), 1, enroll_stopped)
        final_stage_enrolled_up_through <- ifelse((final_stage_enrolled_up_through == num_stage) & enroll_stopped, stage, final_stage_enrolled_up_through)
    }
    result <- data.frame(reject = reject, enroll_stopped = enroll_stopped, final_stage_enrolled_up_through = final_stage_enrolled_up_through)
    
    return(list('result' = result,
                'effBounds' = effBounds,
                'futBounds' = futBounds))
}


### A generic function to compute boundaries using covariance ###
getEffBoundsSequentially <- function(eff_allocated, mvmean0, mvcov_to_use,
                                  abseps = 0.000001, maxpts = 500000, eff_cut = 1e-10) {
    cum_eff_allocated <- cumsum(eff_allocated)
    u <- rep(as.numeric(NA), length(eff_allocated))
    for(k in 1:length(u)){
        if (k == 1){
            u[k] <- qnorm(eff_allocated[k], mvmean0[k], mvcov_to_use[k, k], lower.tail = FALSE)      
        } else {
            if (eff_allocated[k] > eff_cut) {
                u[k] <- uniroot_binarySearch(
                    function(x){
                        1 - pmvnorm(lower = rep(-Inf, k), upper = c(u[1:(k-1)], x),
                                    mean = mvmean0[1:k], sigma = mvcov_to_use[1:k, 1:k],
                                    algorithm = GenzBretz(abseps = abseps, maxpts = maxpts)) - cum_eff_allocated[k] })
            } else {
                u[k] <- Inf
            }
        }
    }
    return(u)
}


# test for getBoundsSequentially
if (0) {
    eff_allocated <- c(0.005, 0.01, 0.015)
    getBoundsSequentially(c(0.005, 0.01, 0.015), rep(0, 3),
                          rbind(c(1.0000000, 0.6933689, 0.5600766), c(0.6933689, 1.0000000, 0.8081340), c(0.5600766, 0.8081340, 1.0000000)))
    # shoud output: 2.575829 2.263050 2.014397
}
