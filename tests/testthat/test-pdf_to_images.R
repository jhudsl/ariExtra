testthat::context("PDF conversion")

testthat::test_that("PDF converted to Ari", {

  skip_on_cran()

  ex_file = system.file("extdata", "example.pdf", package = "didactr")
  if (file.exists(ex_file)) {
    res = pdf_to_ari(ex_file, script = c("asfd", "asdf"))

    testthat::expect_length(res, 1)
    testthat::expect_true(file.exists(res))
    testthat::expect_type(res, "character")
  }
})



