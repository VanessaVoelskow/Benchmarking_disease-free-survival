#################################################################################
###     FUNCTION FOR EXTRACTING NUMBER OF INDIVIDUALS WITH AN EVENT           ###
#################################################################################

##    EXTRACT NUMBER OF EVENTS BY ARM AND NON-UNIQUE EVENTS                 

extract.event.statistics <- function(data, event.col, arm.col, id.col, 
                                     output.path.events, output.path.nonunique) {
  # Validate input
  if (!all(c(event.col, arm.col, id.col) %in% names(data))) {
    stop("Some of the defined columns do not exist in the dataset.")
  }
  
  # Calculate the number of events by arm
  num.events.bygroup <- data %>%
    count(!!sym(event.col), !!sym(arm.col)) %>%
    pivot_wider(
      names_from = !!sym(arm.col),
      values_from = n
    )
  
  # Save the number of events by group to Excel
  write.xlsx(num.events.bygroup, file = output.path.events)
  
  # Calculate the number of non-unique events
  nonunique.events <- data %>%
    filter(!!sym(event.col) == 1) %>%
    count(!!sym(id.col)) %>%
    mutate(count.nonunique = ifelse(n > 1, 1, 0)) %>%
    mutate(n.nonunique = sum(count.nonunique)) %>%
    select(n.nonunique) %>%
    mutate(helper = 1)
  
  # Extract the last row only
  nonunique.events <- nonunique.events[nrow(nonunique.events), ]
  
  # Save the non-unique events to Excel
  write.xlsx(nonunique.events, file = output.path.nonunique)
}


