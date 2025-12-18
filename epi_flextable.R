#' Create publication-ready flextable
#'
#' @param model A fitted regression model
#' @return A flextable object
#' @export

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
