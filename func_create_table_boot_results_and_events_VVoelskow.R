################################################################################
###    FUNCTION TO EXTRACT BOOTSTRAP RESULTS AND EVENT NUMBERS             #####
################################################################################

fun.tidy.boot.event.results <- function(filepath.boot, filepath.events, filepath.nonunique) {
  # Load the bootstrapped results from the .rds file
  boot.results <- readRDS(file = filepath.boot)
  events.results <- read_excel(path = filepath.events)
  nonunique.results <- read_excel(path = filepath.nonunique)
  
  
  
  ## TIDY BOOTSTRAP RESULTS FIRST
  # Tidy the results and extract the statistic and confidence intervals
  boot.results.tidy <- boot.results %>%
    tidy(conf.int = TRUE) %>%
    select(statistic, starts_with("conf"))
  
  # Compute percentages for risks and risk differences (first three rows)
  boot.results.tidy[1:3, ] <- boot.results.tidy[1:3, ] * 100
  
  # Compute survival rates from risks (first two rows)
  boot.results.tidy[1:2, ] <- 100 - boot.results.tidy[1:2, ]
  
  # Create a descriptive vector for terms in the table
  term <- c("surv0", "surv1", "rd", "rr")
  
  # Bind the descriptive terms with the tidy dataframe
  boot.results.tidy.bind <- cbind(term, boot.results.tidy)
  
  # Separate results for risk differences and risk ratios
  results.rr.rd <- boot.results.tidy.bind %>%
    filter(term %in% c("rd", "rr"))
  
  # Separate and tidy results for survival rates
  results.surv <- boot.results.tidy.bind %>%
    filter(term %in% c("surv0", "surv1")) %>%
    mutate(conf.high1 = conf.low,
           conf.low1 = conf.high) %>%
    select(-conf.low, -conf.high) %>%
    rename(conf.low = conf.low1,
           conf.high = conf.high1) %>%
    select(term, statistic, conf.low, conf.high)
  
  # Combine the two sets of results
  results.boot.tidy <- rbind(results.surv, results.rr.rd)
  
  # Reshape the data to a manuscript table format
  results.boot.tidy.wide <- results.boot.tidy %>%
    pivot_wider(
      names_from = term, 
      values_from = c(statistic, conf.low, conf.high)
    ) %>%
    select(ends_with("surv0"),
           ends_with("surv1"),
           ends_with("rd"),
           ends_with("rr")) %>%
    mutate(
      across(
        c(ends_with("surv0"), ends_with("surv1"), ends_with("rd")),
        \(.x) format(round(.x, digits = 1), nsmall = 1)
      )
    ) %>%
    mutate(
      across(
        c(ends_with("rr")),
        \(.x) format(round(.x, digits = 2), nsmall = 2)
      )
    ) %>%
    mutate(surv0.ci = paste0(statistic_surv0, " (", conf.low_surv0, ", ", conf.high_surv0, ")")) %>%
    mutate(surv1.ci = paste0(statistic_surv1, " (", conf.low_surv1, ", ", conf.high_surv1, ")")) %>%
    mutate(rd.ci = paste0(statistic_rd, " (", conf.low_rd, ", ", conf.high_rd, ")")) %>%
    mutate(km.rd.ci = paste0(statistic_rd,"%", " (", conf.low_rd,"%", ", ", conf.high_rd,"%", ")")) %>%
    mutate(rr.ci = paste0(statistic_rr, " (", conf.low_rr, ", ", conf.high_rr, ")")) %>%
    mutate(text.surv0.ci = paste0(statistic_surv0,"%", " (95% CI [", conf.low_surv0,"%", ", ", conf.high_surv0,"%", "])")) %>%
    mutate(text.surv1.ci = paste0(statistic_surv1,"%", " (95% CI [", conf.low_surv1,"%", ", ", conf.high_surv1,"%", "])")) %>%
    mutate(text.rd.ci = paste0(statistic_rd,"%", " (95% CI [", conf.low_rd,"%", ", ", conf.high_rd,"%", "])")) %>%
    mutate(text.rr.ci = paste0(statistic_rr, " (95% CI [", conf.low_rr, ", ", conf.high_rr, "])")) %>%
    select(-ends_with("surv0"),
           -ends_with("surv1"),
           -ends_with("rd"),
           -ends_with("rr"))
  
  
  ## Tidy number of events tables (number of events in censored dataset first)
  # Keep only number of events (not of non-events)
  events.results <- events.results[2, 2:3]
  
  results.events <- cbind(events.results, nonunique.results)
  
  
  results.events.tidy <- results.events %>%
    rename(n.surv0 = '0',
           n.surv1 = '1') %>%
    mutate(unique.surv0 = n.surv0-n.nonunique,
           unique.surv1 = n.surv1-n.nonunique) %>%
    select(-starts_with("event"),
           -n.nonunique,
           -helper
    ) %>%
    mutate(surv0.events.unique = paste0(n.surv0, " (", unique.surv0, ")")) %>%
    mutate(surv1.events.unique = paste0(n.surv1, " (", unique.surv1, ")")) %>%
    select(-ends_with("surv0"),
           -ends_with("surv1"))
  
  
  ## Combine both tables (bootstrap results and event numbers)
  results.tidy.final <- cbind(results.boot.tidy.wide, results.events.tidy) %>%
    relocate(surv0.events.unique, .before = surv0.ci) %>%
    relocate(surv1.events.unique, .before = surv1.ci) %>%
    relocate(km.rd.ci, .after = rr.ci)
  
  
  # Return the final tidy results dataframe
  return(results.tidy.final)
}

