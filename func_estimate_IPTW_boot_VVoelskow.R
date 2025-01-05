#################################################################################
###     FUNCTION TO ESTIMATE STABILIZED IPT WEIGHTS                           ###
#################################################################################

estimate.stabilized.weights.99.boot <- function(df.surv, df, C, K, Z, p.denom, p.num,
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
    group_by(blopnr) %>%
    mutate(sw = cumprod(m.num.t) / cumprod(m.denom.t)) %>%
    ungroup()
  
  df.sw.d <- df.surv %>% 
    select(blopnr, time, sw) %>%
    inner_join(df, by = c("blopnr", "time"))
  
  ###     TRUNCATION OF WEIGHTS

  t.99 <- quantile(df.sw.d$sw, 0.99, na.rm = TRUE)
  df.sw.d <- df.sw.d %>%
    mutate(sw.99.boot = pmin(sw, t.99))
  
  assign("df.sw.d", df.sw.d, envir = .GlobalEnv)
  assign("sw.99.boot", df.sw.d$sw.99.boot, envir = .GlobalEnv)
  return(list(df.sw.d = df.sw.d, sw.99.boot = df.sw.d$sw.99.boot))
}

