################################################################################
###   MASTER SCRIPT TO CREATE PLOTS                                          ###
################################################################################

# Note: run this script after the "master2_extract_results_boot" script,
#       as it requires the results to be pre-loaded in the global environment.


# SET WORKING DIRECTORY
setwd("C:/Users/voelskov/OneDrive - Charité - Universitätsmedizin Berlin/Dokumente/Karolinska/Publikationen/Project1")


############################
##   LOAD REQUIRED PACKAGES 
############################

  library(ggplot2)
  library(ggbreak)
  library(dplyr)
  library(ggimage)
  library(common)


########################
##   LOAD MY FUNCTIONS
########################

  # Refer to script with functions
  source("7_plot_modi_code/func_plots_modify_VVoelskow.R")


#############################################
##   Define objects for use in ggplot script
#############################################

  # Define outcome
  event <- "dfs"
  
  # Define desciption for y-axsis
  ylab.description <- "Disease-free survival (%)"
  
  
  # Define analysis
  analysis <- "main"

  
  # Define beginning of path for loading results tables (input)
  base.path <- "10_results/"
  
  
  # Define full paths for inputs
  input.path.tab <- paste0(base.path, "tabs/", analysis, "/tab_", event, "_", analysis, ".csv")

  
  # Define directory to save results (output)
  save.to.dir.charite.figs <- paste0("3_figures/output_r")
  
  
  # Define output paths - individual plots
  output.path.fig.i.pdf <- create.folders(
    paste0(
      save.to.dir.charite.figs,
      "/", analysis, "/", "gg_", event, "_", analysis, ".pdf"))
  
  output.path.fig.i.png <- create.folders(
    paste0(
      save.to.dir.charite.figs,
      "/", analysis, "/", "gg_", event, "_", analysis, ".png"))
  
  output.path.fig.i.eds <- create.folders(
    paste0(
      save.to.dir.charite.figs,
      "/", analysis, "/", "gg_", event, "_", analysis, ".eds"))
  
  
  ## Define output paths - combined plots
  output.path.fig.c.pdf <- create.folders(
    paste0(
      save.to.dir.charite.figs,
      "/", analysis, "/", "gg_dfs_components_", analysis, ".pdf"))
  
  output.path.fig.c.png <- create.folders(
    paste0(
      save.to.dir.charite.figs,
      "/", analysis, "/", "gg_dfs_components_", analysis, ".png"))
  
  output.path.fig.c.eds <- create.folders(
    paste0(
      save.to.dir.charite.figs,
      "/", analysis, "/", "gg_dfs_components_", analysis, ".eds"))
  
  
  # Define resolution - individual plots
  width.i = 7.8
  height.i = 9
  units.i = "cm"
  dpi.i = 320
  
  # Define resolution - combined plots
  width.c = 19.05
  height.c = 13
  units.c = "cm"
  dpi.c = 600
  


################################################################################
##   RUN GGPLOT SCRIPTS
################################################################################

  # Individual plots
  source("7_plot_modi_code/ggplots_individual_VVoelskow.R")

  # Combined plots
  source("7_plot_modi_code/ggplots_combined_VVoelskow.R")


rm(fun.tidy.boot.event.results, theme_PLOS_dfs_os, theme_PLOS_other)