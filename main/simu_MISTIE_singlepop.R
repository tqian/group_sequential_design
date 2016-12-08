rm(list = ls())

num_simu <- 1000
npara <- 1
nsim <- num_simu / npara

version_fut <- as.integer(Sys.getenv("SGE_TASK_ID"))
on_cluster <- !is.na(version_fut)
if (on_cluster) {
    setwd("~/git_standardGST")
} else {
    version_fut <- npara
    setwd("~/Dropbox/Research/git_standardGST")
}

# parallel job id and random seeds
parallel_seeds <- data.matrix(read.csv("misc/parallel_seeds.csv"))
taskID <- (version_fut %% npara) + 1
case_id <- (version_fut - 1) %/% npara + 1

source("src/utility.R")

source("src/simulateTrial_fixedTimeSS_standardGSTsinglePopulation.R")
source("src/gather_zstat_standardGSTsinglePopulation.R")
source("src/evaluateTrial_fixedTimeSS_standardGSTsinglePopulation.R")


token <- "20160815_MISTIE_1pop"
token <- "20160817_MISTIE_1pop_MCAR"
token <- "20160817_MISTIE_1pop_MAR"
token <- "20160817_MISTIE_1pop_MNAR_largeDropoutProb"
token <- "20160818_MISTIE_1pop_TEhetero"
token <- "20160818_MISTIE_1pop_TEhetero_MAR"

# tokens <- c("20160817_MISTIE_1pop_MCAR_largeDropoutProb", "20160817_MISTIE_1pop_MAR_largeDropoutProb")
# token <- tokens[case_id]

source(paste0("config/", token, ".R"))
checkConfigForRequiredElements(config, config$trial_type)

effect_setting <- c("n", "a")
name_list <- genFilename(config, token = token, effect_setting = effect_setting, npara = npara)

for (est in c("unadj", "ltmle")){
    # for (est in c("ltmle_own", "ltmle_package")){
    # for (est in c("unadj")){
    # est <- "unadj"
    # est <- "ltmle_package"
    print(paste0("## Estimator: ", est))
    .Random.seed <- parallel_seeds[taskID, ]
    
    for (effset in effect_setting){
        test <- simulateTrial_fixedTimeSS_standardGSTsinglePopulation(config, effect_setting = effset, nsim = nsim, estimator = est)
        saveRDS(test, file = name_list[[est]][["parallel"]][[effset]][taskID])
    }
    
    if (taskID == 1) { # the 1st parallel job will wait for all to complete, then do the gathering
        
        completed <- rep(FALSE, length(effect_setting))
        num_completed <- rep(NA, length(effect_setting))
        while (!all(completed)) {
            cat(paste0("\nWaiting for parallel jobs to be done:\n"))
            for (i in 1:length(effect_setting)) {
                effset <- effect_setting[i]
                num_completed[i] <- sum(file.exists(name_list[[est]][["parallel"]][[effset]]))
                completed[i] <- all(file.exists(name_list[[est]][["parallel"]][[effset]]))
                cat("    ", effset, ": ", num_completed[i], "/", npara, "\n")
            }
            Sys.sleep(10)
        }
        
        gathered_zstat <- list()
        for (effset in effect_setting)
            gathered_zstat <- c(gathered_zstat, list(gather_zstat(config, name_list[[est]][["parallel"]][[effset]])))
        names(gathered_zstat) <- effect_setting
        
        saveRDS(gathered_zstat, file = name_list[[est]][["gathered"]])
        
        
        result <- trialPerformance_fixedTimeSS_singlepop(config, zstat_and_cov_multi_effect_setting = gathered_zstat)
        
        saveRDS(result, file = name_list[[est]][["evaluated"]])
        
        
        # remove parallel results
        for (effset in effect_setting)
            file.remove(name_list[[est]][["parallel"]][[effset]])
    }
}

