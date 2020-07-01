testthat::context("Trying a xaringan")
testthat::test_that("xaringan example", {
  if (requireNamespace("xaringan", quietly = TRUE)) {
    path  = system.file("examples", "lucy-demo.Rmd", package = "xaringan")
    x = readLines(path)
    x = gsub("library\\(ggplot2\\)", "", x)
    x = gsub("^\\s*ggplot.*", "", x)
    x = gsub("^\\s*geom_bar.*", "barplot(table(mtcars$am))", x)
    path = tempfile(fileext = ".Rmd")
    writeLines(x, path)

    script = c("this", "is", "one", "word", "per slide")
    rendered_file = tempfile(fileext = ".html")

    rmarkdown::render(path, output_format = xaringan::moon_reader(),
                      output_file = rendered_file)
    testthat::expect_error({
      res = rmd_to_ari(path, open = FALSE,
                       rendered_file = rendered_file,
                       capturer = "webshot",
                       capture_method = "vectorized")
    }, regexp = "Failed to generate")

    testthat::expect_error({
      res = rmd_to_ari(path, open = FALSE,
                       rendered_file = rendered_file,
                       capturer = "chrome_print",
                       capture_method = "vectorized")
    },
    regexp = "Failed to")


    have_decktape = nzchar(Sys.which("decktape"))
    pdf_file = tempfile(fileext = ".pdf")
    if (have_decktape) {
      xaringan::decktape(rendered_file, pdf_file, docker = FALSE)

      res = pdf_to_ari(pdf_file, script = script, open = FALSE)
    }
  }
})
