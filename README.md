# Simulating Standard Group Sequential Designs

## The code does the following:

### 1. For a specified config file, simulate standard group sequential trials to get test statistics at each stage.

### 2. Use error spending boundaries to test for the trial, and outputs: Type I error, power, expected sample size, expected duration of the simulated trials.

## Code structure:

analysis/: code to analysis the simulated test-statistics.

config/: configuration of a specific trial, including number of stages, sample size at each stage, enrollment rate, data generating mechanism (dgm) to use, and other design characteristics.

data/: data set. (Omitted in the display github version due to data security.)

dgm/: functions of data generating mechanisms.

main/: code to simulate trials. Use "qsub -t" for submitting to the cluster for parallelization.

misc/: random seeds for parallel jobs.

src/: source code of functions.

## How to run:

The data set MISTIE_100pts.csv is stored on jhpce cluster. On the cluster, cd to main/, and submmit jobs as (if want to run 200 parallle jobs):

qsub -t 1:200 simu_MISTIE_singlepop.sh