dest_path <- file.path(tempdir(), "test_payments.xlsx")

testthat::expect_false(file.exists(dest_path))

ok <- tryCatch({
  cbi_download(dest_path = dest_path)
  TRUE
}, error = function(e) {
  message("cbi_download failed: ", e$message)
  FALSE
})

if (ok) {
  testthat::expect_true(file.exists(dest_path))
  cbi_tbl <- read_excel(dest_path, sheet = "Master Data")

  cbi_clean_tbl <- cbi_read_data(dest_path)
} else {
  testthat::skip("Download failed, skipping integration tests")
}

# Clean up automatically after tests finish
withr::defer(
  file.remove(dest_path),
  testthat::teardown_env()
)
