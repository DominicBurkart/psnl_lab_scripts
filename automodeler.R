function (dv, fixed, random, fixed_interactions = FALSE, random_combs = FALSE,
          slope_and_intercept = FALSE, subset_random = FALSE, subset_fixed = FALSE,
          verbose = TRUE)
{
  #' Generates (or fails to generate) every possible lme4 mixed linear model.
  #' 
  #' Model statistics for models that converge are saved to disk. Calculations
  #' are parallelized, but results are not returned until the last model fails or is
  #' returned. 
  #' @param dv a one-column dataframe object.
  #' @param fixed a dataframe with the same number and order of cases as in dv.
  #' @param random a dataframe with the same number and order of cases as in dv and fixed.
  #' @param fixed_interactions boolean. When true, compute all fixed interaction models.
  #' @param random_combs boolean. When true, attempt all models with combinations.
  #' @param slope_and_intercept boolean. When true, include models with random intercept.
  #' @param subset_random boolean. When true, include models with only some of the input random effects.
  #' @param subset_fixed boolean. When true, include all models with only some of the input fixed effects.
  #' and slope for the same variables.
  #' @return a dataframe with information about each model ran.
  
  if (!require("gtools")) install.packages("gtools"); require("gtools")
  if (!require("foreach")) install.packages("foreach"); require("foreach")
  if (!require("lmerTest")) install.packages("lmerTest"); require("lmerTest")
  if (verbose) cat("Automodeler started. Packages loaded.\n")
  
  #overview:
  # fixed, random and dv are DFs. Generate one DF from the three.
  # 1.) generate + record call without any random effects or interactions
  # 2.) generate + record calls without any random effects, with interactions
  # 3.) find every random slope / intercept combo (slope or intercept), no combinations
  # 4.) find every random slope / intercept combo (slope and intercept), no combinations
  # 5.) run each potential model in parallel ("%dopar%") and record results
  # retain the following information for each model:
  #   - summed variance explained by random effects (prediction quality)
  #   - summed variance explained total (prediction quality)
  #   - measure of heteroscedicity (systemic prediction errors)
  #   - if available, AIC / BIC (model fit stats)
  # 6.) return a dataframe of results. 
  
  # fixed, random and dv are DFs. Generate one DF from the three.
  stopifnot(class(fixed) == class(data.frame())) # random and fixed should be dataframes.
  stopifnot(class(random) == class(data.frame())) # random and fixed should be dataframes.
  stopifnot(class(dv) != class(data.frame())) # dv should also be a dataframe.
  stopifnot(length(colnames(dv)) == 1) # multiple regression not supported at this time.
  stopifnot(nrow(fixed) == nrow(dv)) #cases clearly don't line up! yikes!
  stopifnot(nrow(fixed) == nrow(random)) #cases clearly don't line up! yikes!
  unique_colnames = length(unique(c(colnames(fixed), colnames(random))))
  all_colnames = length(c(colnames(fixed), colnames(random)))
  stopifnot(unique_colnames == all_colnames) #duplicate column names! yikes!
  df = cbind(dv, fixed, random)
  dv_name = colnames(dv)[1]
  fixed_names = colnames(fixed)
  random_names = colnames(random)
  
  # 1.) generate + record call without any random effects or interactions
  min_call = dv_name + " ~ " + paste(fixed_names, collapse=' + ' )
  if (verbose) cat("Minimal case found: ", min_call, "\n")
  if (verbose) cat("Random effects to be added: ", paste(random_names, collapse=', '))

  # 2.) generate + record calls without any random effects, with fixed interactions
  interaction_calls = NULL
  if ( length(fixed_names) > 1 && fixed_interactions == TRUE ){
    interaction_calls = c()
    stop("Interactions are unimplemented.")
    if (verbose) cat("Number of potential models with fixed interaction: ", length(interaction_calls), "\n")
  } 
  all_calls = c(min_call, interaction_calls)
  
  # 3.) find every random slope / intercept combo (slope or intercept), no combinations
  rlen = length(random_names)
  rand_col_perms = permutations(rlen, rlen, random_names)
  for (i in 1:rlen/2){
    for (perm in rand_col_perms){
      slopes = random_names[1:i]
      intercepts = intercepts[i:rlen]
    }
  }
  
  # 4.) find every random slope / intercept combo (slope and intercept), no combinations
  # note that most of these models won't converge.
  if (slope_and_intercept && length(random_names) > 1 ){
    
  }
  
  # 5.) run each potential model in parallel ("%dopar%") and record results
  # retain the following information for each model:
  #   - summed variance explained by random effects (prediction quality)
  #   - summed variance explained total (prediction quality)
  #   - measure of heteroscedicity (systemic prediction errors)
  #   - if available, AIC / BIC (model fit stats)
  # 6.) return a dataframe of results. 
  results = data.frame()
  return(results)
} 
