# file: MISTIE_dgm_nondeterministic.R
# author: Tianchen Qian
# date: 1/5/2016

#####
# Note: require EVEN number of sample size at each stage for each subpopulation
#####

#####
# Generates data from MISTIE data set:
# 1. sample W with replacement from the original data set
# 2. Bernoulli draw of L1 and Y based on the model fits from the original data set
# 3. randomly assign subpop = 1,2 and treatment A = 0,1
# 4. tweak Y with reset_prob so that treatment effect is 0.122
# 5. assign enrollment time and make sure treatment assignment is balanced at each stage
#####


# Load data ---------------------------------------------------------------

# open original data file (robust to file location)
data_locations <- c("MISTIE_100pts.csv", "../MISTIE_100pts.csv", "data/MISTIE_100pts.csv", "../data/MISTIE_100pts.csv")
data_which <- file.exists(data_locations)
if (sum(data_which) == 0) {
    stop("Cannot find MISTIE_100pts.csv.")
} else {
    data_which <- which(data_which)[1]
    data_MISTIE100 <- read.csv(data_locations[data_which])
}

# fit models
fitL <- glm(L1 ~ S + W1 + W2 + W3,      data = data_MISTIE100, family = binomial)
fitY <- glm(Y  ~ S + W1 + W2 + W3 + L1, data = data_MISTIE100, family = binomial)

# function for resetting Y (to 1) with some probability (for matching the treatment effect)
reset_w_prob <- function(vec, prob) {
    return(ifelse(rbinom(length(vec), 1, prob), 1, vec))
}

# duplicate data to add in treatment heterogeneity

fitL_withA <- glm(L1 ~ S + W1 + W2 + W3      + A + S*A + A*W3,          data = data_MISTIE100, family = binomial)
fitY_withA <- glm(Y  ~ S + W1 + W2 + W3 + L1 + A + S*A + A*W3,          data = data_MISTIE100, family = binomial)

# Basic data generating mechanism for MISTIE ( w/wo treatment heterogeneity) ------------------------------------

########################################################
# Routine: resampleMistie
#
#     Generates data (subpop, W1, W2, W3, A, L1, Y)
#     with treatment effect 0.122.
########################################################
resampleMistie <- function(ss, effect_setting = c("n", "a"), reset_prob = 0.175, data_original = data_MISTIE100) {
    
    sample_idx <- sample.int(nrow(data_original), ss, replace = TRUE)
    dt <- data_original[sample_idx, c("S", "W1", "W2", "W3")]
    dt$A <- rep(c(0, 1), each = ss / 2)
    dt$L1 <- rbinom(ss, 1, predict(fitL, newdata = dt, type = "response"))
    dt$Y <- rbinom(ss, 1, predict(fitY, newdata = dt, type = "response"))
    
    if (effect_setting == "a") {
        # add treatment effect by randomly setting more Y[A==1] to be 1
        # So nothing is predictive of the treatment effect
        dt$Y[dt$A == 1] <- reset_w_prob(dt$Y[dt$A == 1], reset_prob)
    }
    return(dt)
}

if (0) {
    # find reset probability
    dt <- resampleMistie(10000000, "a", 0.175)
    mean(dt$Y[dt$A==1]) - mean(dt$Y[dt$A==0])
}

########################################################
# Routine: resampleMistieTrtEffHeterogeneity
#
#     Generates data (subpop, W1, W2, W3, A, L1, Y)
#     with treatment effect 0.122 and treatment effect heterogeneity
########################################################
resampleMistieTrtEffHeterogeneity <- function(ss, effect_setting = c("n", "a"), shift_n = -0.10094, desired_TE = 0.122, data_original = data_MISTIE100) {
    
    sample_idx <- sample.int(nrow(data_original), ss, replace = TRUE)
    dt    <- data_original[sample_idx, c("S", "W1", "W2", "W3")]
    dt$A  <- rep(c(0, 1), each = ss / 2)
    dt$L1 <- rbinom(ss, 1, predict(fitL_withA, newdata = dt, type = "response"))
    dt$Y  <- rbinom(ss, 1, predict(fitY_withA, newdata = dt, type = "response"))
    
    if (effect_setting == "n") {
        dt$Y[dt$A == 1] <- dt$Y[dt$A == 1] + shift_n
    } else if (effect_setting == "a") {
        dt$Y[dt$A == 1] <- dt$Y[dt$A == 1] + shift_n + desired_TE
    }
    return(dt)
}

if (0) {
    # find reset probability
    dt <- resampleMistieTrtEffHeterogeneity(1000000, "n", shift_n = -0.10094)
    mean(dt$Y[dt$A==1]) - mean(dt$Y[dt$A==0])
}



# dgm wrapper -------------------------------------------------------------

########################################################
# Routine: genDgmMistieBalanceArm
#
#     Generates dgm function that balances number of patients in each arm.
#     dgm_type can be "homogeneous" or "heterogeneous".
########################################################
genDgmMistieBalanceArm <- function(dgm_type) {
    
    if (dgm_type == "homogeneous") {
        basic_dgm_fcn <- resampleMistie
    } else if (dgm_type == "heterogeneous") {
        basic_dgm_fcn <- resampleMistieTrtEffHeterogeneity
    }
    
    dgm_wrapper_to_return <- function(config, effect_setting = c("n", "a"), dgm_fcn = basic_dgm_fcn) {
        effect_setting <- match.arg(effect_setting)
        
        cumss    <- config$cumss
        erate    <- config$enrollment_rate
        delay_WL <- config$delay_WL
        delay_LY <- config$delay_LY
        
        stopifnot(all(cumss %% 2 == 0))
        
        dt <- dgm_fcn(cumss[length(cumss)], effect_setting)
        
        # shuffle once
        dt <- dt[sample.int(nrow(dt)), ]
        
        # for each subpop, construct dt with one A=0 and one A=1
        dt_a0 <- subset(dt, A == 0)
        dt_a1 <- subset(dt, A == 1)
        
        flag_a0 <- TRUE
        count_a0 <- 1
        count_a1 <- 1
        for (i in 1:nrow(dt)) {
            if (flag_a0) {
                dt[i, ] <- dt_a0[count_a0, ]
                count_a0 <- count_a0 + 1
            } else {
                dt[i, ] <- dt_a1[count_a1, ]
                count_a1 <- count_a1 + 1
            }
            flag_a0 <- !flag_a0
        }
        
        dt$T_enroll <- (1:nrow(dt)) / erate
        dt$T_shortterm <- dt$T_enroll + delay_WL
        dt$T_final <- dt$T_shortterm + delay_LY
        
        ### dropout ###
        
        dropout <- config$dropout
        if (!is.null(dropout)) {
            dt <- dropout(dt)
        } else {
            dt$observed_final <- 1
            dt$observed_shortterm <- 1
        }
        
        return(dt)
    }
    
    return(dgm_wrapper_to_return)
}

########################################################
# dgm functions to use
########################################################

dgmMistieBalanceArm <- genDgmMistieBalanceArm(dgm_type = "homogeneous")
dgmMistieBalanceArmTrtEffHeterogeneity <- genDgmMistieBalanceArm(dgm_type = "heterogeneous")


# Missing data mechanism for MISTIE ---------------------------------------


########################################################
# Routine: genDropoutMCAR
#
#     Generates dropout function for MCAR,
#     with dropout probability for Y and L.
########################################################
genDropoutMCAR <- function(prob_dropout_Y, prob_dropout_L) {
    f <- function(dt) {
        prob_dropout_L_given_Y <- prob_dropout_L / prob_dropout_Y
        
        dt$observed_final <- rbinom(nrow(dt), 1, 1 - prob_dropout_Y)
        
        dt$observed_shortterm <- 1
        dt$observed_shortterm[dt$observed_final == 0] <- rbinom(sum(dt$observed_final == 0), 1, 1 - prob_dropout_L_given_Y)
        return(dt)
    }
    return(f)
}

########################################################
# Routine: genDropoutMAR
#
#     Generates dropout function for MAR.
#     Dropout probability is based on dichotomized W_to_use.
########################################################
genDropoutMAR <- function(prob_dropout_Y, prob_dropout_L, W_cutoff_quantile, W_to_use) {
    
    f <- function(dt) {
        
        W_cutoff <- quantile(dt[, W_to_use], probs = W_cutoff_quantile)
        prob_dropout_L_given_Y <- prob_dropout_L / prob_dropout_Y
        
        dt$observed_final <- 1
        dt$observed_shortterm <- 1
        
        index <- list(dt[, W_to_use] <= W_cutoff, dt[, W_to_use] > W_cutoff)
        for (i in 1:2) {
            dt$observed_final[index[[i]]] <- rbinom(sum(index[[i]]), 1, 1 - prob_dropout_Y[i])
            dt$observed_shortterm[dt$observed_final == 0 & index[[i]]] <- rbinom(sum(dt$observed_final == 0 & index[[i]]), 1, 1 - prob_dropout_L_given_Y[i])
        }
        
        return(dt)
    }
    return(f)
}

########################################################
# Routine: genDropoutMAR2
#
#     Generates dropout function for MAR.
#     Dropout probability is based on "S" and "W3".
########################################################
dropoutMAR_SW3 <- function(dt) {
    
    dt$observed_final <- 1
    dt$observed_shortterm <- 1
    
    prob_dropout <- dt$S * 0.02 + dt$W3 * 0.01
    
    dt$observed_final <- rbinom(nrow(dt), 1, 1 - prob_dropout)
    # dt$observed_shortterm[dt$observed_final == 0 & index[[i]]] <- rbinom(sum(dt$observed_final == 0 & index[[i]]), 1, 1 - prob_dropout_L_given_Y[i])
    
    return(dt)
}


########################################################
# Routine: genDropoutMAR
#
#     Generates dropout function for MNAR.
#     Dropout probability is based on dichotomized Y_to_use.
########################################################
genDropoutMNAR <- function(prob_dropout_Y, prob_dropout_L, Y_cutoff_quantile, Y_to_use = "Y") {
    
    f <- function(dt) {
        
        Y_cutoff <- quantile(dt[, Y_to_use], probs = Y_cutoff_quantile)
        prob_dropout_L_given_Y <- prob_dropout_L / prob_dropout_Y
        
        dt$observed_final <- 1
        dt$observed_shortterm <- 1
        
        index <- list(dt[, Y_to_use] <= Y_cutoff, dt[, Y_to_use] > Y_cutoff)
        for (i in 1:2) {
            dt$observed_final[index[[i]]] <- rbinom(sum(index[[i]]), 1, 1 - prob_dropout_Y[i])
            dt$observed_shortterm[dt$observed_final == 0 & index[[i]]] <- rbinom(sum(dt$observed_final == 0 & index[[i]]), 1, 1 - prob_dropout_L_given_Y[i])
        }
        
        return(dt)
    }
    return(f)
}
