function(dv, fixed, random){
  #' Generates (or fails to generate) every possible lme4 mixed linear model.
  #' 
  #' @param dv a one-column dataframe object.
  #' @param fixed a dataframe with the same number and order of cases as in dv.
  #' @param random a dataframe with the same number and order of cases as in dv and fixed.

  if (!require("foreach")) install.packages("foreach"); require(foreach)
  if (!require("lmerTest")) install.packages("lmerTest"); require(lmerTest)
  
  # Fixed and Random are DFs. Generate one DF from the two
  stopifnot(class(fixed) == class(data.frame())) # random and fixed should be dataframes.
  stopifnot(class(random) == class(data.frame())) # random and fixed should be dataframes.
  stopifnot(nrow(fixed) == length(dv)) #cases clearly don't line up! yikes!
  stopifnot(nrow(fixed) == nrow(random)) #cases clearly don't line up! yikes!
  unique_colnames = length(unique(c(colnames(fixed), colnames(random))))
  all_colnames = length(c(colnames(fixed), colnames(random)))
  stopifnot(unique_colnames == all_colnames) #duplicate column names! yikes!
  if (class(dv) != class(data.frame())) cat("DV is not a dataframe and may be misnamed later.\n")
  df = cbind(dv, fixed, random)
  
  # 1.) generate + record call without any random effects or interactions
  
  
  # 2.) generate + record calls without any random effects, with interactions
  # 3.) find every random slope / intercept combo (slope or intercept)
  # 4.) find every random slope / intercept combo (slope and intercept)
  # 5.) run each potential model in parallel ("%dopar%") and record results
  # retain the following information for each model:
  #   - summed variance explained by random effects (prediction quality)
  #   - summed variance explained total (prediction quality)
  #   - measure of heteroscedicity (systemic prediction errors)
  #   - if available, AIC / BIC (model fit stats)
  # 6.) return a dataframe of results. 
}
