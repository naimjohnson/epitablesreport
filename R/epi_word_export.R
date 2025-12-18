#' Export regression table and interpretation to Word
#'
#' @param model A fitted regression model
#' @param file Path to Word file
#' @return A Word document saved to disk
#' @export
epi_export_word <- function(model, file = "epi_report.docx") {

  doc <- officer::read_docx()

  # Title
  doc <- officer::body_add_par(
    doc,
    "Epidemiologic Regression Results",
    style = "heading 1"
  )

  # Add regression results table
  doc <- flextable::body_add_flextable(
    doc,
    epi_flextable(model)
  )

  # Interpretation section
  doc <- officer::body_add_par(
    doc,
    "Interpretation of Results",
    style = "heading 2"
  )

  # Add single block of text (full report)
  interpretation_text <- paste(epi_brief_report(model), collapse = " ")
  doc <- officer::body_add_par(
    doc,
    interpretation_text,
    style = "Normal"
  )

  # Save Word document
  print(doc, target = file)
}
