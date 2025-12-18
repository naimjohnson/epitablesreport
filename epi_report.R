#' Generate brief epidemiologic interpretation
#'
#' @param model A fitted regression model
#' @return Character string summary
#' @export
# Prevent R CMD check NOTES
utils::globalVariables(c(
  "%>%", "term", "estimate", "conf.low", "conf.high", "p.value",
  "estimate_ci", "sig"
))

epi_brief_report <- function(model) {

  results <- tidy_epi_model(model) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      direction = dplyr::case_when(
        estimate > 1 ~ "increased",
        estimate < 1 ~ "decreased",
        TRUE ~ "no change"
      ),
      sig_text = dplyr::case_when(
        p.value < 0.05 ~ "was statistically significant",
        TRUE ~ "was not statistically significant"
      )
    )

  glue::glue_collapse(
    glue::glue_data(
      results,
      "{term} was associated with a {direction} odds of the outcome
      (OR = {round(estimate, 2)}, 95% CI: {round(conf.low, 2)}–{round(conf.high, 2)}),
      and this association {sig_text}."
    ),
    sep = "\n"
  )
}
