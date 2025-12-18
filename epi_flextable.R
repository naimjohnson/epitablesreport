#' Create publication-ready flextable
#'
#' @param model A fitted regression model
#' @return A flextable object
#' @export
# Prevent R CMD check NOTES
utils::globalVariables(c(
  "%>%", "term", "estimate", "conf.low", "conf.high", "p.value",
  "estimate_ci", "sig"
))

epi_flextable <- function(model) {

  tab <- epi_rep_table(model)

  flextable::flextable(tab) %>%
    flextable::autofit() %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::set_caption(
      caption = "Regression Results with 95% Confidence Interals"
    )
}
