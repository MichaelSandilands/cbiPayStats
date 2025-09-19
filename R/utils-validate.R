#' Validate CBI data download
#'
#' Ensures that the downloaded data has the expected structure
#' before being passed to [cbi_read_data()].
#'
#' @param data A data downloaded from [cbi_download()].
#'
#' @return Invisibly returns `data` if valid, otherwise throws an error.
#' @keywords internal
validate_cbi_download <- function(data) {
  expected_names <- c(
    "Reporting Period", "Table", "Category", "DSI", "Channel Type",
    "Geographical Description", "Series Description", "Sector", "Sub-sector",
    "Observation Type", "Observation Value"
  )

  expected_classes <- c(
    "POSIXct",            # Reporting Period
    "POSIXt",             # Reporting Period
    rep("character", 9),  # first 9 are character
    "numeric"             # Observation Value
  )

  if (ncol(data) != length(expected_names)) {
    stop("`data` must have exactly ", length(expected_names),
         " columns.", call. = FALSE)
  }

  if (!identical(colnames(data), expected_names)) {
    stop("`data` must have the expected column names:\n",
         paste(expected_names, collapse = ", "),
         call. = FALSE)
  }

  actual_classes <- lapply(data, class) |> unlist() |> unname()

  if (!identical(actual_classes, expected_classes)) {
    stop("`data` must have the expected column classes:\n",
         paste(expected_names, collapse = ", "),
         call. = FALSE)
  }

  invisible(data)
}

#' Validate cleaned CBI data
#'
#' Ensures that the input tibble has the expected structure
#' before being passed to plotting or analysis functions.
#'
#' @param data A tibble produced by [cbi_read_data()].
#'
#' @return Invisibly returns `data` if valid, otherwise throws an error.
#' @keywords internal
validate_cbi_clean <- function(data) {
  expected_names <- c(
    "Reporting Period", "Table", "Category", "DSI", "Channel Type",
    "Geographical Description", "Series Description", "Sector", "Sub-sector",
    "Observation Type", "Observation Value"
  )

  expected_classes <- c(
    "Date",               # Reporting Period
    rep("character", 9),  # first 9 are character
    "numeric"             # Observation Value
  )

  if (ncol(data) != length(expected_names)) {
    stop("`data` must have exactly ", length(expected_names),
         " columns.", call. = FALSE)
  }

  if (!identical(colnames(data), expected_names)) {
    stop("`data` must have the expected column names:\n",
         paste(expected_names, collapse = ", "),
         call. = FALSE)
  }

  actual_classes <- lapply(data, class) |> unlist() |> unname()

  if (!identical(actual_classes, expected_classes)) {
    stop("`data` must have the expected column classes:\n",
         paste(expected_names, collapse = ", "),
         call. = FALSE)
  }

  if (anyNA(data)) {
    stop("Error: The data frame contains NA values.")
  }

  invisible(data)
}

#' Check if multiple arguments are character strings
#'
#' @param ... A series of arguments to be checked.
#'
#' @return The function returns `invisible(TRUE)` if all arguments are
#'   character strings, otherwise it throws an error.
#' @export
validate_characters <- function(...) {
  args <- list(...)
  stopifnot(all(vapply(args, is.character, logical(1))))
  invisible(TRUE)
}

#' Check if an argument is a single character string
#'
#' @param ... A series of arguments to be checked.
#'
#' @return The function returns `invisible(TRUE)` if the argument is a single
#'   character string, otherwise it throws an error.
#' @export
validate_single_string <- function(...) {
  args <- list(...)
  stopifnot(all(vapply(args, function(x) length(x) == 1, logical(1))))
  invisible(TRUE)
}
