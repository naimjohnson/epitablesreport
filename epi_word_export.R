#' Export regression table and interpretation to Word
#'
#' @param model A fitted regression model
#' @param file Path to Word file
#' @return A Word document saved to disk
#' @export
# Prevent R CMD check NOTES
utils::globalVariables(c(
  "%>%", "term", "estimate", "conf.low", "conf.high", "p.value",
  "estimate_ci", "sig"
))

epi_export_word <- function(model, file = "epi_regression_report.docx") {

  # Create table
  ft <- epi_flextable(model)

  # Create interpretation text
  interpretation <- epi_brief_report(model)

  # Create Word document
  doc <- officer::read_docx()

  # Add title
  doc <- officer::body_add_par(
    x = doc,
    value = "Epidemiologic Regression Results",
    style = "heading 1"
  )

  # Add flextable (NOTE: from flextable package)
  doc <- flextable::body_add_flextable(
    x = doc,
    value = ft
  )

  # Blank line
  doc <- officer::body_add_par(doc, value = "")

  # Interpretation heading
  doc <- officer::body_add_par(
    x = doc,
    value = "Interpretation",
    style = "heading 2"
  )

  # Interpretation text
  doc <- officer::body_add_par(
    x = doc,
    value = interpretation,
    style = "Normal"
  )

  # Save document
  print(doc, target = file)

  message("Word document saved to: ", normalizePath(file))
}

