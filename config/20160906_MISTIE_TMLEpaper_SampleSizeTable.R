source("dgm/MISTIE_dgm_singlepop.R")

config <- list(
    trial_type = "standard-1pop",
    cumss = cumsum(rep(300/5,5)),
    num_stage = 5,
    enrollment_rate = 140,
    delay_WL = 30/365,
    delay_LY = 150/365,
    futBounds = c(0,0,0),
    FWER = 0.025,
    power_ESF = function(x, total_err, rho = 2){
        x <- x / x[length(x)]
        alpha_cumulative <- total_err * x ^ rho
        alpha <- diff(c(0, alpha_cumulative))
        return(alpha)
    },
    W_to_use = c("S", "W3"),
    L_to_use = "L1",
    dgm = dgmMistieBalanceArmTrtEffHeterogeneity
)