################################################################################
###    MASTER SCRIPT TO EXTRACT BOOTSTRAP RESULTS AND CREATE PLOTS         #####
################################################################################

# SET WORKING DIRECTORY
setwd("C:/Users/voelskov/OneDrive - Charité - Universitätsmedizin Berlin/Dokumente/Karolinska/Publikationen/Project1")

############################################
##   LOAD REQUIRED PACKAGES               ##
############################################

  # Load packages needed
  library(tidyverse)
  library(openxlsx)
  library(readxl)
  library(broom)
  library(boot)
  library(data.table)
  library(ggplot2)
  library(ggbreak)
  library(dplyr)
  library(ggimage)


############################################
##   LOAD REQUIRED FUNCTIONS              ##
############################################

  # Function to create a table from bootstrap results and event numbers
  source("8_extractCI_code/func_create_table_boot_results_and_events_VVoelskow.R")

  # Function to load and save above table for each outcome
  source("8_extractCI_code/func_load_table_for_each_outcome_VVoelskow.R")


###########################################
##   Define objects for use in functions
###########################################

  # Create a list of outcomes
  event.list <- c(
    "dfs",
    "os",
    "recloc",
    "recmet",
    "contralat",
    "secprim",
    "bc.mortality"
  )


  # Define analysis
  analysis <- "main"


  # Create a descriptive vector for terms in the final output table
  term <- c("Disease-free survival", "Overall survival", "Local recurrence",
             "Distant recurrence", "Contralateral breast cancer",
             "Other second primary cancer", "Survival from breast cancer")

  
  # Define beginning of path for function which processes results (input)
  base.path <- "10_results/"

  
  # Define directory to save results (output)
  save.to.dir.charite.tabs <- paste0("4_tables/output_r")
  
  
  # Write function to create folders if nonexistent
  create.folders <- function(output.path) {
    extracted.dir.path <- dirname(output.path)
    if (!dir.exists(extracted.dir.path)) {
      dir.create(extracted.dir.path, recursive = TRUE)
    }
    return(output.path)
  }
  
  ## Define output paths
  output.path.results.all.events.tab <- create.folders(
    paste0(
      save.to.dir.charite.tabs,
      "/", analysis, "/", "table_results_", analysis, ".xlsx"))
  
  output.path.results.bc.mortality.tab <- create.folders(
    paste0(
      save.to.dir.charite.tabs,
      "/", analysis, "/", "table_results_bc.mortality_", analysis, ".xlsx"))


#########################################################
##   EXTRACT RESULTS AND STORE THEM IN GLOBAL ENVIRONMENT
#########################################################

  # Extract results for all outcomes
  results.list <- lapply(event.list, process.results, base.path = base.path)

  # Create final table
  table.results.final <- do.call(rbind, results.list[c(1:6)])

  # Add term column
  table.results.final <- cbind(term[1:6], table.results.final)

  # Use dynamic name for final table depending on the analysis type
  assign(paste0("table.results.", analysis), table.results.final, envir = .GlobalEnv)


#################################################
##   SAVE TABLES TO LOCAL COMPUTER FOR MANUSCRIPT
#################################################

  # Save final table for manuscript (dfs and single components)
  write.xlsx(get(paste0("table.results.", analysis)), file = output.path.results.all.events.tab)

  # Save final table for manuscript (death from breast cancer)
  write.xlsx(get(paste0("results.bc.mortality.", analysis)), file = output.path.results.bc.mortality.tab)
           
           

# Remove objects that are not needed for plotting
rm(table.results.final, results.bc.mortality.main, term)






