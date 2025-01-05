#################################################################################
###     FUNCTION TO ESTIMATE STABILIZED IPT WEIGHTS                           ###
#################################################################################

estimate.stabilized.weights.99 <- function(df.surv, df, C, K, Z, p.denom, p.num,
                                           treat.col, cum.treat.lag.col) {

  ###     KEEP TIMES WHEN TRASTUZUMAB INITIATION WAS POSSIBLE
  
  df.weight.estimation <- df.surv %>% 
    filter(.data[[cum.treat.lag.col]] == C, time >= K, time <= Z)
  
  ###     WEIGHTING - DENOMINATOR
  
  m.denom <- glm(p.denom, family = binomial(link = "logit"), data = df.weight.estimation)
  df.surv <- df.surv %>% 
    mutate(m.denom = predict(m.denom, ., type = "response"),
           m.denom.t = ifelse(.data[[treat.col]] == 1, m.denom, 1 - m.denom),
           m.denom.t = ifelse(.data[[cum.treat.lag.col]] == C & time >= K & time <= Z, m.denom.t, 1))
  
  ###     WEIGHTING - NUMERATOR
  
  m.num <- glm(p.num, family = binomial(link = "logit"), data = df.weight.estimation)
  df.surv <- df.surv %>% 
    mutate(m.num = predict(m.num, ., type = "response"),
           m.num.t = ifelse(.data[[treat.col]] == 1, m.num, 1 - m.num),
           m.num.t = ifelse(.data[[cum.treat.lag.col]] == C & time >= K & time <= Z, m.num.t, 1))
  
  ###     ESTIMATE FINAL WEIGHTS
  
  df.surv <- df.surv %>%
    group_by(lopnr) %>%
    mutate(sw = cumprod(m.num.t) / cumprod(m.denom.t)) %>%
    ungroup()
  
  df.sw <- df.surv %>% 
    select(lopnr, time, sw) %>%
    inner_join(df, by = c("lopnr", "time"))
  
  ###     TRUNCATION OF WEIGHTS

  t.99 <- quantile(df.sw$sw, 0.99, na.rm = TRUE)
  df.sw <- df.sw %>%
    mutate(sw.99 = pmin(sw, t.99))
  
  assign("df.sw", df.sw, envir = .GlobalEnv)
  assign("sw.99", df.sw$sw.99, envir = .GlobalEnv)
  return(list(df.sw = df.sw, sw.99 = df.sw$sw.99))
}

