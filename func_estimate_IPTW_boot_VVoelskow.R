#################################################################################
###     FUNCTION TO ESTIMATE STABILIZED IPT WEIGHTS                           ###
#################################################################################

estimate.stabilized.weights.99.boot <- function(df.surv, df, C, K, Z, p.denom, p.num,
                                           treat.col, cum.treat.lag.col) {

  ###     KEEP TIMES WHEN TRASTUZUMAB INITIATION WAS POSSIBLE
  
  df.weight.estimation <- df.surv[get(cum.treat.lag.col) == C & time >= K & time <= Z]
  
  ###     WEIGHTING - DENOMINATOR
  
  m.denom <- glm(p.denom, family = binomial(link = "logit"), data = df.weight.estimation)
  df.surv$m.denom <- predict(m.denom, df.surv, type = "response")
  df.surv[, m.denom.t := fifelse(get(treat.col) == 1, m.denom, 1-m.denom)]
  df.surv[, m.denom.t := fifelse(get(cum.treat.lag.col) == C & time >= K & time <= Z, m.denom.t, 1)]
  
  ###     WEIGHTING - NUMERATOR
  
  m.num <- glm(p.num, family = binomial(link = "logit"), data = df.weight.estimation)
  df.surv$m.num <- predict(m.num, df.surv, type = "response")
  df.surv[, m.num.t := fifelse(get(treat.col) == 1, m.num, 1-m.num)]
  df.surv[, m.num.t := fifelse(get(cum.treat.lag.col) == C & time >= K & time <= Z, m.num.t, 1)]
  
  ###     ESTIMATE FINAL WEIGHTS
  
  df.surv <- df.surv %>%
    group_by(blopnr) %>%
    mutate(sw = cumprod(m.num.t) / cumprod(m.denom.t)) %>%
    ungroup()
  
  d.sw <- df.surv[, c("blopnr", "time", "sw")]
  df.sw.d <- merge(d.sw, df, by = c("blopnr", "time"))
  
  ###     TRUNCATION OF WEIGHTS

  t.99 <- quantile(df.sw.d$sw, 0.99, na.rm = TRUE)
  df.sw.d$sw.99.boot <- df.sw.d$sw
  df.sw.d$sw.99.boot[df.sw.d$sw.99.boot > t.99] <- t.99
  
  assign("df.sw.d", df.sw.d, envir = .GlobalEnv)
  assign("sw.99.boot", df.sw.d$sw.99.boot, envir = .GlobalEnv)
  return(list(df.sw.d = df.sw.d, sw.99.boot = df.sw.d$sw.99.boot))
}

