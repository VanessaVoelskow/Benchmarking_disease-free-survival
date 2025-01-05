#################################################################################
###     FUNCTION TO EXTRACT DISTRIBUTION OF FINAL IP WEIGHTS                  ###
#################################################################################

extract.distribution.of.weights <- function(df.sw, analysis, event, output.path.distr.ipw) {
  
  ###     EXTRACT DISTRIBUTION OF WEIGHTS
  
  dist.sw <- df.sw %>%
    summarise(
      Minimum = min(sw.99, na.rm = TRUE),
      `1st Quartile` = quantile(sw.99, 0.25, na.rm = TRUE),
      Median = median(sw.99, na.rm = TRUE),
      Mean = mean(sw.99, na.rm = TRUE),
      `3rd Quartile` = quantile(sw.99, 0.75, na.rm = TRUE),
      Maximum = max(sw.99, na.rm = TRUE)
    )
  
  ###     EXTRACT STANDARD DEVIATION
  
  d.temp.sd.sw.99 <- tibble(
    analysis = analysis,
    outcome = event,
    ipweight = "sw.99",
    sd = sd(df.sw$sw.99, na.rm = TRUE)
  )
  
  ###     COMBINE DATAFRAMES AND SAVE TABLE
  
  dist.sw.sd <- bind_cols(d.temp.sd.sw.99, dist.sw)
  
  write.xlsx(dist.sw.sd, file = output.path.distr.ipw)
}
