################################################################################
####   Per protocol analysis for benchmarking the effect of trastuzumab    #####
####                  in early curative breast cancer                      #####
################################################################################

# Script for manuscript:
# "Benchmarking an observational analysis against a randomized trial:
#  an application to trastuzumab in early curative breast cancer"

# Treatments:
# - Trastuzumab + chemotherapy
# - Chemotherapy


############################################
##   LOAD REQUIRED PACKAGES               ##
############################################

  library(tidyverse)
  library(boot)
  library(data.table)
  library(survival)
  library(openxlsx)
  library(tidyr)


############################################
##   LOAD FUNCTIONS                       ##
############################################

  # Define directory where functions are stored
  input.dir.func <- "insert_path_to_where_functions_are_stored"

  # Function to extract event numbers
  source(paste0(input.dir.func, "func_extract_nevents_VVoelskow.R"))

  # Function to estimate stabilized inverse probability of treatment weights truncated at 99th percentile
  source(paste0(input.dir.func, "func_estimate_IPTW_VVoelskow.R"))
  
  # Function to estimate stabilized IPTW truncated at 99th percentile while bootstrapping
  source(paste0(input.dir.func, "func_estimate_IPTW_boot_VVoelskow.R"))

  # Function to extract the distribution of the final IP weights
  source(paste0(input.dir.func, "func_extract_distribution_final_weights_VVoelskow.R"))

  # Function to fit the final model with truncated weights and store results in a table
  source(paste0(input.dir.func, "func_fit_ipmodel_VVoelskow.R"))
  
  # Function to fit the final model while bootstrapping
  source(paste0(input.dir.func, "func_fit_ipmodel_boot_VVoelskow.R"))


#####################################################
##   DEFINE PARAMETERS FOR MODELLING               ##
#####################################################

  ## Define outcome for modelling
  event <- "death"
  
  ## Define analysis
  analysis <- "main"

  ## Define number of bootstrap iterations
  num.boot <- 500

  ## Denominator
  p.denom <- treat_trast ~ time + timesq + age + I(age^2) + mari.stat.cat + income.fam.cat + 
    menopausal.status + diag.dt.year + diag.tclass + diag.nclass + diag.er + diag.pr +
    op.grade + op.numtumours + I(op.numtumours^2) + op.sizetumour.cat +
    conf.cardio.drug + conf.cardio.drug.l1 + conf.cardio.mild + conf.cardio.mild.l1 +
    conf.cardio.severe + conf.cardio.severe.l1 + conf.liver.fu + conf.liver.fu.l1 +
    conf.renal.fu + conf.renal.fu.l1 + conf.dyspnea.fu + conf.dyspnea.fu.l1 +
    conf.hemato.fu + conf.hemato.fu.l1 + conf.infect.fu + conf.infect.fu.l1 +
    conf.neuro.fu + conf.neuro.fu.l1 + conf.gastro.fu + conf.gastro.fu.l1 +
    conf.visits.5yr.bl + I(conf.visits.5yr.bl^2)

  # Numerator
  p.num <- treat_trast ~ time + timesq + age + I(age^2) + mari.stat.cat +
    income.fam.cat + menopausal.status + diag.dt.year + diag.tclass + diag.nclass +
    diag.er + diag.pr + op.grade + op.numtumours + I(op.numtumours^2) +
    op.sizetumour.cat + conf.visits.5yr.bl + I(conf.visits.5yr.bl^2)

  # Final outcome model
  p.final <- as.formula(paste(event,
  "~ arm + time + timesq + I(arm*time) + I(arm*timesq)"))

  ## Set time point for estimation of risks (260 weeks = 5 years) 
  V <- 260
  K <- 'insert_earliest_possible_week_for_trastuzumab_initiation'
  Z <- 'insert_latest_possible_week_for_trastuzumab_initiation'
  
  ## Define input paths
  input.filepath1 <- 'insert_path_to_where_data_is_stored_on_your_computer_including_filename'
  input.filepath2 <- 'insert_path_to_where_data_is_stored_on_your_computer_including_filename'
  
  ## Define directory to save results (output)
  save.to.dir <- "insert_path_to_where_results_shall_be_stored"

  ## Write function to create folders if nonexistent
  create.folders <- function(output.path) {
    extracted.dir.path <- dirname(output.path)
    if (!dir.exists(extracted.dir.path)) {
      dir.create(extracted.dir.path, recursive = TRUE)
    }
    return(output.path)
  }
  
  ## Define output paths
  output.path.events <- create.folders(
    paste0(
      save.to.dir,
      "/events/", analysis, "/", "num_events_bygroup_", event, "_", analysis, ".xlsx"))
  
  output.path.nonunique <- create.folders(
    paste0(
      save.to.dir,
      "/nonunique/", analysis, "/", "nonunique_events_", event, "_", analysis, ".xlsx"))
  
  output.path.distr.ipw <- create.folders(
    paste0(
      save.to.dir,
      "/distr_ipw/", analysis, "/", "distribution_final_weights_", event, "_", analysis, ".xlsx"))
  
  output.path.tabs <- create.folders(
    paste0(
      save.to.dir,
      "/tabs/", analysis, "/", "tab_", event, "_", analysis, ".csv"))
  
  output.path.boot <- create.folders(
    paste0(
      save.to.dir,
      "/boot/", analysis, "/", "boot_results_", event, "_", analysis, ".rds"))


###########################################################################
##   LOAD PREPARED DATA SETS                                             ##
###########################################################################

  # Load data in long person-time format
  df.surv <- read.csv(file = input.filepath1)

  # Load data with clones in long person-time format
  df <- read.csv(file = input.filepath2)

# Variables

## Structural
# - lopnr: unique identifier for each individual
# - treat_trast: indicator for whether an individual ever received trastuzumab
# - cum_treat_trast_lag1: cumulative values of treat_trast lagged by one week
# - event: binary indicator for event of interest
# - arm: indicator of treatment arm to which an individual was assigned through cloning
# - time: continuous variable for time
# - timesq: continuous variable for time squared

## Baseline variables (further details can be found in the Supplementary Material of the manuscript)
# - age: continuous variable for age
# - mari.stat.cat: categorical variable for marital status
# - income.fam.cat: categorical variable for family income
# - menopausal.status: categorical variable for menopausal status
# - diag.dt.year: categorical variable for year of diagnosis
# - diag.tclass: categorical variable for size of tumor at diagnosis
# - diag.nclass: categorical variable for lymph node involvement
# - diag.er: categorical variable for ER status
# - diag.pr: categorical variable for PR status
# - op.sizetumour.cat: : categorical variable for size of the largest tumor at surgery
# - op.grade: categorical variable for histological grade at surgery
# - op.numtumours: continuous variable for number of tumors in breast at surgery
# - conf.visits.5yr.bl: continuous variable for number of visits to healthcare professional within 5 years prior to baseline

## Time-varying variables (further details can be found in the Supplementary Material of the manuscript)
### Notes: - All variables are binary indicators of a condition in a specific week during follow-up.
###        - The suffix "l1" indicates a lag of one week.
# - conf.cardio.drug: drug intake against cardiac disease during study period
# - conf.cardio.mild: mild to moderate cardiac disease during study period
# - conf.cardio.severe: severe cardiac disease during study period
# - conf.liver.fu: liver function disorder during study period
# - conf.renal.fu: renal function disorder during study period
# - conf.dyspnea.fu: dyspnea during study period
# - conf.hemato.fu: hematological events during study period
# - conf.infect.fu: severe infections during study period
# - conf.neuro.fu: neurologic disorders during study period
# - conf.gastro.fu: gastrointestinal disorders during study period


################################################################################
##    EXTRACT NUMBER OF EVENTS BY ARM AND NON-UNIQUE EVENTS                 ####
################################################################################

  extract.event.statistics(
    data = df,
    event.col = event,
    arm.col = "arm",
    id.col = "lopnr",
    output.path.events = output.path.events,
    output.path.nonunique = output.path.nonunique
  )


################################################################################
##   ESTIMATE STABILIZED IPT WEIGHTS TRUNCATED AT 99TH PERCENTILE             ##
################################################################################

  estimate.stabilized.weights.99(
    df.surv = df.surv,
    df = df,
    C = 0,
    K = K,
    Z = Z,
    p.denom = p.denom,
    p.num = p.num,
    treat.col = "treat_trast",
    cum.treat.lag.col = "cum_treat_trast_lag1"
  )


################################################################################
###     EXTRACT DISTRIBUTION OF FINAL WEIGHTS                                ###
################################################################################

  extract.distribution.of.weights(
    df.sw = df.sw,
    event = event,
    analysis = analysis,
    output.path.distr.ipw = output.path.distr.ipw
  )


################################################################################
###   FIT POOLED LOGISTIC REGRESSION WITH TRUNCATED WEIGHTS AND STORE RESULTS ##
################################################################################

  fit.ipmodel(
    df.sw = df.sw,
    sw.99 = sw.99,
    event = event,
    p.final = p.final,
    V = V,
    output.path.tabs = output.path.tabs
  )


################################################################################
###     BOOTSTRAPPING WITH 500 ITERATIONS                                   ###
################################################################################

  ###     EXTRACT LOPNRs AND CREATE COPY OF DF
  
  df.surv.lopnrs <- df.surv[df.surv$time==0,"lopnr"]

  ###     DEFINE BOOTSTRAP FUNCTION
  
  clone.boot <- function(data, indices, event) {
    
    # Select individuals in each bootstrap iteration
    lopnrs <- data$lopnr
    selected.lopnrs <- data.frame(lopnr = lopnrs[indices])
    selected.lopnrs$blopnr <- 1:nrow(selected.lopnrs)
    
    # Keep data for individuals in the bootstrap sample
    df.surv.boot <- left_join(selected.lopnrs, df.surv, by = "lopnr", relationship = "many-to-many")
    df.boot <- left_join(selected.lopnrs, df, by = "lopnr", relationship = "many-to-many")
    
    # Estimate stabilized weights and truncate them
    weights.results <- estimate.stabilized.weights.99.boot(
      df.surv = df.surv.boot,
      df = df.boot,
      C = 0,
      K = K,
      Z = Z,
      p.denom = p.denom,
      p.num = p.num,
      treat.col = "treat_trast",
      cum.treat.lag.col = "cum_treat_trast_lag1"
    )

    # Fit pooled logistic regression and calculate survival and risks
    tab <- fit.ipmodel.boot(
      df.sw.d = df.sw.d,
      sw.99.boot = sw.99.boot,
      event = event,
      p.final = p.final,
      V = V)
    
    # Add risk differences and risk ratios
    tab$rd <- tab$risk0 - tab$risk1
    tab$rr <- tab$risk0 / tab$risk1
    
    # Return results for the last time point
    return(c(
      tab$risk0[which(tab$time == V - 1)],
      tab$risk1[which(tab$time == V - 1)],
      tab$rd[which(tab$time == V - 1)],
      tab$rr[which(tab$time == V - 1)]
    ))
  }
  
  ###    RUN BOOTSTRAPS
  
  set.seed(4237)
  
  # Write function to run bootstraps and store results
  run.boot.store.results <- function(event, analysis) {
    
    # Print number of bootstrap iteration to console
    num.iteration <- 0 # start at 1
    
    boot.results <- boot(
      data = df.surv.lopnrs,
      statistic = function(data, indices) {
        
        num.iteration <<- num.iteration + 1
        cat("Running bootstrap iteration:", num.iteration, "\n")
        clone.boot(data, indices, event = event)
        
      },
      R = num.boot
    ) 
    
    # Save to global environment
    assign(paste0("boot.results.", event, ".", analysis), boot.results, envir = .GlobalEnv)
    
    # Save results as RDS file
    saveRDS(boot.results, file = output.path.boot)
    
  }
  
  # Run above function
  run.boot.store.results(event = event,
                         analysis = analysis)
  
  
  
  
  
  
  
  
  
