#' Generate brief epidemiologic interpretation
#'
#' @param model A fitted regression model
#' @return Character string summary
#' @export

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
        p.value < 0.05 ~ "statistically significant",
        TRUE ~ "not statistically significant"
      )
    )

  glue::glue_data(
    results,
    "• {term}: associated with a {direction} odds of the outcome
    (OR = {round(estimate, 2)}, 95% CI: {round(conf.low, 2)}–{round(conf.high, 2)}),
    and this association was {sig_text}."
  )
}
