############################
# Small utility functions
#
############################




genFilename <- function(config, token, effect_setting, npara, estimators = c("ltmle", "unadj")) {
    
    cumss           <- config$cumss
    num_stage       <- config$num_stage
    enrollment_rate <- config$enrollment_rate
    
    cumss <- paste0("ss=", paste0(cumss, collapse = ","))
    num_stage <- paste0("nstg=", num_stage)
    enrollment_rate <- paste0("erate=", enrollment_rate)
    text <- paste0(token, '_', cumss, '_', num_stage, '_', enrollment_rate)
    folder <- paste0("result/", text)
    name_list <- list(folder = folder,
                      folder_parallel = paste0(folder, "/parallel"),
                      parallel = paste0(folder, "/parallel/", "parallel-", text),
                      gathered = paste0(folder, "/gathered-", text),
                      evaluated = paste0(folder, "/evaluated-", text))
    dir.create("result", showWarnings = FALSE)
    dir.create(name_list$folder, showWarnings = FALSE)
    dir.create(name_list$folder_parallel, showWarnings = FALSE)
    
    name_list_full <- list()
    for (est in estimators) {
        name_parallel <- paste0(name_list$parallel, '-', est)
        name_gathered <- paste0(name_list$gathered, '-', est, ".rds")
        name_evaluated <- paste0(name_list$evaluated, '-', est, ".rds")
        
        name_parallel_list <- list()
        for (effset in effect_setting)
            name_parallel_list <- c(name_parallel_list, list(paste0(name_parallel, "-", effset, "-", 1:npara, ".rds")))
        names(name_parallel_list) <- effect_setting
        
        name_list_full <- c(name_list_full, list(list(parallel = name_parallel_list,
                                                      gathered = name_gathered,
                                                      evaluated = name_evaluated)))
    }
    names(name_list_full) <- estimators
    
    return(name_list_full)
}


falseIfNull <- function(x) {
    if (is.null(x)) {
        return(FALSE)
    } else {
        return(x)
    }
}


checkConfigForRequiredElements <- function(config, trial_type = "standard") {
    
    if (trial_type == "standard-1pop") {
        items_required <- c(
            "cumss",
            "delay_WL",
            "delay_LY",
            "num_stage",
            "enrollment_rate",
            "W_to_use",
            "L_to_use",
            "dgm"
        )
    } else if (trial_type == "standard-1pop-dropout") {
        items_required <- c(
            "cumss",
            "delay_WL",
            "delay_LY",
            "num_stage",
            "enrollment_rate",
            "W_to_use",
            "L_to_use",
            "dgm",
            "dropout"
        )
    }
    
    stopifnot(all(items_required %in% names(config)))
}


uniroot_binarySearch <- function(f, interval = c(-20, 20), errtol = 1e-6) {
    
    # finding root for monotonic function f
    
    if (length(interval) != 2) {
        stop("Interval in binarySearch should be vector of length 2 (lower, upper).")
    }
    
    lower <- interval[1]
    upper <- interval[2]
    
    if (f(lower) < f(upper)) {
        # increasing f
        if ((f(lower) > 0) | (f(upper) < 0)) {
            stop("Increasing f, but f(lower) > 0 or f(upper) < 0.")
        }
        while(upper - lower > errtol) {
            mid <- (upper + lower) / 2
            if (f(mid) < 0) {
                lower <- mid
            } else {
                upper <- mid
            }
        }
    } else {
        # decreasing f
        if ((f(lower) < 0) | (f(upper) > 0)) {
            stop("Decreasing f, but f(lower) < 0 or f(upper) > 0.")
        }
        while(upper - lower > errtol) {
            mid <- (upper + lower) / 2
            if (f(mid) < 0) {
                upper <- mid
            } else {
                lower <- mid
            }
        }
    }
    
    return(mid)
}