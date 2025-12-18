#' Tidy epidemiologic regression models
#'
#' @param model A glm (binomial) or lm model
#' @param conf_level Confidence level for intervals
#' @return A tibble of model results
#' @export
# Prevent R CMD check NOTES
utils::globalVariables(c(
  "%>%", "term", "estimate", "conf.low", "conf.high", "p.value",
  "estimate_ci", "sig"
))

tidy_epi_model <- function(model, conf_level = 0.95) {

  if (inherits(model, "glm") && model$family$family == "binomial") {

    broom::tidy(
      model,
      conf.int = TRUE,
      conf.level = conf_level,
      exponentiate = TRUE
    ) %>%
      dplyr::mutate(measure = "Odds Ratio")

  } else if (inherits(model, "lm")) {

    broom::tidy(
      model,
      conf.int = TRUE,
      conf.level = conf_level
    ) %>%
      dplyr::mutate(measure = "Beta")

  } else {
    stop("Model must be logistic (glm binomial) or linear (lm)")
  }
}
