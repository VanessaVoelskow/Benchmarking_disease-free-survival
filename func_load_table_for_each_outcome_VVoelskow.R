################################################################################
###    FUNCTION TO STREEMLINE EXTRACTION FOR DIFFERENT OUTCOMES AND ANALYSES  ##
################################################################################

# Define the function to streamline the process
process.results <- function(event, base.path) {
  # Define file paths for each outcome
  filepath.boot.i <- paste0(base.path, "boot/", analysis, "/boot_results_", event, "_", analysis, ".rds")
  filepath.events.i <- paste0(base.path, "events/", analysis, "/num_events_bygroup_", event, "_", analysis, ".xlsx")
  filepath.nonunique.i <- paste0(base.path, "nonunique/", analysis, "/nonunique_events_", event, "_", analysis, ".xlsx")
  
  # Print file paths for potential debugging
  print(filepath.boot.i)
  print(filepath.events.i)
  print(filepath.nonunique.i)
  
  # Call the fun.tidy.boot.event.results function
  result <- fun.tidy.boot.event.results(filepath.boot = filepath.boot.i,
                                        filepath.events = filepath.events.i,
                                        filepath.nonunique = filepath.nonunique.i)
  
  # Assign the result to a variable in the global environment with respective name
  assign(paste0("results.", event, ".", analysis), result, envir = .GlobalEnv)
  
}