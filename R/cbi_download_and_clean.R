#' Scrape the Download URL for the CBI Monthly Card Payment Statistics
#'
#' This function web scrapes the latest download URL for the monthly card
#' payment statistics .xlsx file from the Central Bank of Ireland website.
#'
#' @return The latest URL for the CBI Monthly Card Payment Statistics
#' @importFrom rvest read_html html_elements html_attr
#' @export
cbi_get_dl_link <- function() {

  # The specific URL that contains the download links
  data_page_url <- paste0("https://www.centralbank.ie/statistics/",
                          "data-and-analysis/monthly-card-payment-statistics")
  base_url <- "https://www.centralbank.ie"

  # Find the link to the latest Excel file on the specific data page
  links <- rvest::read_html(data_page_url) |>
    rvest::html_elements(".xls") |>
    rvest::html_attr("href")

  monthly_card_pay_stats_url <- paste0(
    base_url,
    links[!grepl("discontinued", links)]
  )

  # Check to make sure we found exactly one link
  if (length(monthly_card_pay_stats_url) != 1) {
    stop("Could not find a unique monthly card payments URL.")
  }

  monthly_card_pay_stats_url

}

#' Download Monthly Card Payment Statistics
#'
#' This function downloads the latest monthly card payment statistics
#' from the Central Bank of Ireland website. It will only download a new file
#' if the content has changed since the last download, this is convenient for a
#' `targets` workflow as an example.
#'
#' @param dest_path The path where the Excel file should be saved, including the
#'   file name (e.g., "./data/my_payments.xlsx")
#' @return The function downloads the file and returns the file path invisibly.
#' @importFrom utils download.file
#' @importFrom readxl read_excel
#' @export
cbi_download <- function(dest_path) {

  url <- cbi_get_dl_link()

  validate_characters(dest_path)
  validate_single_string(dest_path)

  # Download the file to a temporary location
  temp_path <- tempfile(fileext = ".xlsx")
  utils::download.file(url,
                       destfile = temp_path,
                       quiet = TRUE,
                       mode = "wb")

  # Create the destination directory if it doesn't exist
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)

  # Check if the new file is identical to the existing one
  if (file.exists(dest_path) &&
        identical(readxl::read_excel(dest_path, sheet = "Master Data"),
                  readxl::read_excel(temp_path, sheet = "Master Data"))) {
    file.remove(temp_path)
    message("No new data found. Using existing file.")
  } else {
    # If a new file is found, save it to the specified destination
    file.copy(temp_path, dest_path, overwrite = TRUE)
    file.remove(temp_path) # Now, remove the original temp file
    message("New data downloaded and saved to: ", dest_path)
  }

  validate_cbi_download(readxl::read_excel(dest_path, sheet = "Master Data"))

  invisible(dest_path)

}

#' Read and Clean Card Payment Statistics
#'
#' This function reads the "Master Data" sheet from the monthly card payments
#' Excel file, formats the `Reporting Period` and removes the `NA` values from
#' `Sector` and `Sub-sector`.
#'
#' @param file_path The path to the Excel file containing the data.
#' @return A clean `tibble` or `data.frame` with the payment statistics.
#' @importFrom readxl read_excel
#' @export
cbi_read_data <- function(file_path) {

  validate_characters(file_path)
  validate_single_string(file_path)

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  df <- readxl::read_excel(file_path, sheet = "Master Data")

  validate_cbi_download(df)

  df$"Reporting Period" <- as.Date(df$"Reporting Period")
  df[is.na(df$Sector), ]$Sector <- ""
  df[is.na(df$"Sub-sector"), ]$"Sub-sector" <- ""

  validate_cbi_clean(df)

  df
}

cbi_filter_data <- function(
    df, table, category, dsi, channel_type, geographical_description,
    series_description, sector, sub_sector, observation_type
) {

  validate_characters(
    table, category, dsi, channel_type, geographical_description,
    series_description, sector, sub_sector, observation_type
  )
  validate_single_string(
    table, category, dsi, channel_type, geographical_description,
    series_description, sector, sub_sector, observation_type
  )

  validate_cbi_clean(df)

  df <- df[df$"Table" == table, ]
  df <- df[df$"Category" == category, ]
  df <- df[df$"DSI" == dsi, ]
  df <- df[df$"Channel Type" == channel_type, ]
  df <- df[df$"Geographical Description" == geographical_description, ]
  df <- df[df$"Series Description" == series_description, ]
  df <- df[df$"Sector" == sector, ]
  df <- df[df$"Sub-sector" == sub_sector, ]
  df <- df[df$"Observation Type" == observation_type, ]

  df

}
