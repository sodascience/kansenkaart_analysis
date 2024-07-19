#' Function to create expectations for the kansenkaart
#' 
#' This function computes marginal and conditional expectations for the kansenkaart. 
#' 
#' @param cohort_dat data frame containing all data for the entire cohort
#' @param outcome_name the name of the outcome column, for example income_perc
#' @param region_id the region in which an expectation is desired (for postcode3 e.g. 102)
#' @param region_type the name of the region column from which region_id should be taken, e.g., "postcode3"
#' @param income_grp parental income percentile group: "low", "mid", "high" or "all"
#' @param migration_grp migration background group: e.g., "Nederland", "Marokko", "Suriname" or "all"
#' @param gender_grp gender group: "Mannen", "Vrouwen", or "all"
#' @param conf_level the confidence level of the confidence interval around the estimate
#' 
#' @returns a column matrix with the following elements in order: 
#' - expectation (estimate)
#' - lower bound of the confidence interval
#' - upper bound of the confidence interval
#' - number of samples the expectation was computed from

kansenkaart_expect <- function(cohort_dat, outcome_name, region_id, region_type, 
                               income_grp = "all", migration_grp = "all", gender_grp = "all", 
                               conf_level = 0.95) {
  
  # Step 1: data filtering ----
  ## income group ----
  if (income_grp != "all") {
    cohort_dat <- cohort_dat %>% filter(income_group == income_grp)
  }
  
  ## migration group ----
  if (migration_grp != "all") {
    cohort_dat <- cohort_dat %>% filter(migration == migration_grp)
  }
  
  ## gender ----
  if (gender_grp != "all") {
    cohort_dat <- cohort_dat %>% filter(geslacht == gender_grp)
  }
  
  ## region ----
  cohort_dat <- cohort_dat %>% filter(.data[[region_type]] == region_id)
  
  
  # Step 2: model fitting ----
  ## Formula creation ----
  if (income_grp != "all") {
    frm_txt <- paste0(outcome_name, " ~ income_parents_perc")
  } else {
    frm_txt <- paste0(outcome_name, " ~ 1")
  }
  
  ## Model fitting ----
  fit <- try(lm(as.formula(frm_txt), data = cohort_dat), silent = TRUE)
  
  
  # Step 3: expectation ----
  ## At what parental income do we predict?
  new_data <- switch(
    EXPR = income_grp, 
    all  = tibble(1),
    low  = tibble(income_parents_perc = 0.25),
    mid  = tibble(income_parents_perc = 0.50),
    high = tibble(income_parents_perc = 0.75)
  )
  
  ## Create prediction matrix
  if (inherits(fit, "try-error")) {
    pred <- matrix(nrow = 1, ncol = 3)
  } else {
    pred <- suppressWarnings(
      predict(fit, interval = "confidence",  newdata = new_data)
    )
  }
  
  colnames(pred) <- c("est", "lwr", "upr")
  
  # Step 4: add number of samples and return ----
  result <- list(
    est = pred[1],
    lwr = pred[2],
    upr = pred[3],
    n   =  nrow(cohort_dat)
  )
  return(result)
}