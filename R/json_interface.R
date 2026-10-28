#' Open JSON file.
#'
#' @param json JSON in its string form or path to JSON file
#' @param schema JSON schema
#'
#' @return parsed JSON object
#' @importFrom jsonlite parse_json
#' @importFrom jsonvalidate json_schema
#' @keywords internal
#'
open_json <- function(json, schema = NULL) {
  if (is.list(json)) {
    return(campsismod::JSONElement(json)) # Don't go further if data is already parsed
  }
  assertthat::assert_that(length(json) == 1, msg = "Argument json must be a path or the JSON string")

  if (grepl(pattern = "\\s*[\\{\\[]", x = json)) {
    raw_json <- json
  } else {
    raw_json <- suppressWarnings(paste0(readLines(json), collapse = "\n"))
  }

  # Validate content against schema
  if (!is.null(schema)) {
    obj <- jsonvalidate::json_schema$new(schema)
    obj$validate(raw_json, error = TRUE)
  }

  json_ <- jsonlite::parse_json(raw_json, simplifyVector = FALSE)

  return(campsismod::JSONElement(json_))
}
