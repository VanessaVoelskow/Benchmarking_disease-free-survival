#################################################################################
###     FUNCTION TO FIT FINAL POOLED LOGISTIC REGRESSION MODEL                ###
#################################################################################

fit.ipmodel.boot <- function(df.sw.d, sw.99.boot, event, p.final, V) {

  ###     FIT POOLED LOGISTIC REGRESSION WITH TRUNCATED WEIGHTS

  m.final <- glm(p.final,
                 family = binomial(link = 'logit'),
                 data = df.sw.d,
                 weights = sw.99.boot)
  
  ###     STORE RESULTS IN TABLE                                

  ### Create arm-specific dataframes
  arm.trast <- data.frame(time = seq(0, V - 1), arm = 0, timesq = seq(0, V - 1)^2)
  arm.chemo <- data.frame(time = seq(0, V - 1), arm = 1, timesq = seq(0, V - 1)^2)
  
  ### Add predicted values
  arm.trast$p.event0 <- predict(m.final, arm.trast, type = "response")
  arm.chemo$p.event1 <- predict(m.final, arm.chemo, type = "response")
  
  ### Estimate survival probabilities from hazards
  arm.trast$surv0 <- cumprod(1 - arm.trast$p.event0)
  arm.chemo$surv1 <- cumprod(1 - arm.chemo$p.event1)
  
  ### Estimate risks
  arm.trast$risk0 <- 1 - arm.trast$surv0
  arm.chemo$risk1 <- 1 - arm.chemo$surv1
  
  ### Create a table with results per time point
  tab.prep <- merge(arm.trast, arm.chemo, by = c("time", "timesq"))
  tab.prep$t0 <- tab.prep$time + 1
  
  ### Add baseline row
  bl <- data.frame(time = 0, timesq = 0, arm.x = 0, arm.y = 1,
                   p.event0 = 0, p.event1 = 0, surv0 = 1, surv1 = 1,
                   risk0 = 0, risk1 = 0, t0 = 0)
  tab <- rbind(bl, tab.prep)
  
  return(tab)
}
