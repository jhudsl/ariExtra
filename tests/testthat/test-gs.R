testthat::context("gs_to_ari Runs")

testthat::test_that("multiplication works", {
  id = "1Opt6lv7rRi7Kzb9bI0u3SWX1pSz1k7botaphTuFYgNs"
  res = gs_to_ari(id, verbose = 2, open = FALSE)
  res2 = to_ari(id, open = FALSE)
  testthat::expect_equal(res$script, res2$script)
  bad_id = "1AJxokPkGVzNZMXjyF59mNe3EiQRGxylbBfYGGoQMQls"
  testthat::expect_error(res = gs_to_ari(bad_id))
})
