#' Create epidemiologic regression results table
#'
#' @param model A fitted regression model
#' @param digits Number of decimal places
#' @param include_intercept Logical; include intercept row
#' @return A formatted tibble
#' @export
# Prevent R CMD check NOTES
utils::globalVariables(c(
  "%>%", "term", "estimate", "conf.low", "conf.high", "p.value",
  "estimate_ci", "sig"
))

epi_rep_table <- function(model, digits = 2, include_intercept = FALSE) {

  results <- tidy_epi_model(model)

  if (!include_intercept) {
    results <- results %>% dplyr::filter(term != "(Intercept)")
  }

  results %>%
    dplyr::mutate(
      estimate_ci = glue::glue(
        "{round(estimate, digits)} ({round(conf.low, digits)}–{round(conf.high, digits)})"
      ),
      sig = dplyr::case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ ""
      ),
      p.value = round(p.value, 3)
    ) %>%
    dplyr::select(term, estimate_ci, p.value, sig) %>%
    dplyr::rename(
      Variable = term,
      `Estimate (95% CI)` = estimate_ci,
      `p-value` = p.value,
      Significance = sig
    )
}
