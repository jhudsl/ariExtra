testthat::context("BAD GS notes")

testthat::test_that("The bad ID should fail", {

  bad_id = "1AJxokPkGVzNZMXjyF59mNe3EiQRGxylbBfYGGoQMQls"
  testthat::expect_error(gs_to_ari(bad_id))

  testthat::expect_error({
    testthat::expect_warning(gs_to_ari(bad_id))
  })

})

