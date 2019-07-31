testthat::context("PPTX notes")

testthat::test_that("The notes are ordered", {

  ex_file = system.file("extdata", "example.pptx", package = "didactr")
  res = pptx_notes(ex_file)
  ans = c("notesSlide1.xml", "notesSlide2.xml")

  testthat::expect_identical(names(res), ans)

})

testthat::test_that("PPTX to Ari works", {

  ex_file = system.file("extdata", "example.pptx", package = "didactr")
  res = pptx_to_ari(ex_file)
  testthat::expect_error(
    {res = pptx_to_ari(ex_file, script = "heyasdf")},
    regexp = "identical\\(length\\(images"
    )
})


