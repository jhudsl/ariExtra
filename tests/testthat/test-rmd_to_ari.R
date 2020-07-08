testthat::context("Trying a xaringan")
testthat::test_that("xaringan example", {
  if (requireNamespace("xaringan", quietly = TRUE)) {
    # use xaringan's Rmd but remove ggplot2 requirement
    path  = system.file("examples", "lucy-demo.Rmd", package = "xaringan")
    x = readLines(path)
    x = gsub("library\\(ggplot2\\)", "", x)
    x = gsub("^\\s*ggplot.*", "", x)
    x = gsub("^\\s*geom_bar.*", "barplot(table(mtcars$am))", x)
    path = tempfile(fileext = ".Rmd")
    writeLines(x, path)

    fail_msg = paste0("Failed to generate|",
                      "Cannot find.* Google Chrome|",
                      "pagedown package needed|",
                      "not executable")


    run_rmd = function(capturer) {
      res = rmd_to_ari(path, open = FALSE,
                       script = script,
                       rendered_file = rendered_file,
                       capturer = capturer,
                       capture_method = "vectorized")
    }

    script = c("this", "is", "one", "word", "per slide")

    required_pandoc <- "1.12.3"
    if (rmarkdown::pandoc_available(required_pandoc)) {
      rendered_file = tempfile(fileext = ".html")

      # needs pagedown still
      rmarkdown::render(path, output_format = xaringan::moon_reader(),
                        output_file = rendered_file)
      if (requireNamespace("pagedown", quietly = TRUE)) {
        testthat::expect_error({
          run_rmd("webshot")
        }, regexp = fail_msg)
      } else {
        run_rmd("webshot")
      }

      testthat::expect_error({
        run_rmd("chrome_print")
      }, regexp = fail_msg)

    }
  }
})

testthat::test_that("xaringan example with pre-rendered", {
  if (requireNamespace("xaringan", quietly = TRUE)) {
    # use xaringan's Rmd but remove ggplot2 requirement
    path  = system.file("examples", "lucy-demo.Rmd", package = "xaringan")
    x = readLines(path)
    x = gsub("library\\(ggplot2\\)", "", x)
    x = gsub("^\\s*ggplot.*", "", x)
    x = gsub("^\\s*geom_bar.*", "barplot(table(mtcars$am))", x)
    path = tempfile(fileext = ".Rmd")
    writeLines(x, path)

    fail_msg = paste0("Failed to generate|",
                      "Cannot find.* Google Chrome|",
                      "pagedown package needed|",
                      "not executable")
    run_rmd = function(capturer) {
      res = rmd_to_ari(path, open = FALSE,
                       script = script,
                       rendered_file = rendered_file,
                       capturer = capturer,
                       capture_method = "vectorized")
    }

    script = c("this", "is", "one", "word", "per slide")

    # already rendered
    rendered_file = system.file("extdata",
                                "lucy-demo-noggplot2.html",
                                package = "ariExtra")
    tfile = tempfile(fileext = ".pdf")
    out = try({
      pagedown::chrome_print(rendered_file,
                             output = tfile)
    }, silent = TRUE)
    chrome_print_failure = inherits(out, "try-error")

    if (!requireNamespace("pagedown", quietly = TRUE) ||
        chrome_print_failure) {
      testthat::expect_error({
        run_rmd("webshot")
      }, regexp = fail_msg)
    } else {
      run_rmd("webshot")
    }

    if (!requireNamespace("pagedown", quietly = TRUE) ||
        chrome_print_failure) {
      testthat::expect_error({
        run_rmd("chrome_print")
      }, regexp = fail_msg)
    } else {
      run_rmd("chrome_print")
    }


    have_decktape = nzchar(Sys.which("decktape"))
    pdf_file = tempfile(fileext = ".pdf")
    if (have_decktape) {
      xaringan::decktape(rendered_file, pdf_file, docker = FALSE)

      res = pdf_to_ari(pdf_file, script = script, open = FALSE)
      res = rmd_to_ari(path,
                       open = FALSE,
                       script = script,
                       rendered_file = rendered_file,
                       capturer = "decktape")
      testthat::expect_length(res$images, 5)
    }
  }
})
