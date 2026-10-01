test_that("gateway returns full-precision JSON", {
  parsed <- jsonlite::fromJSON(gateway(func = "model_run",
                                       model_input = get_default_input()))
  expect_equal(parsed$risk, model_run(get_default_input())$risk, tolerance = 1e-12)
})

test_that("gateway strips platform fields and defaults to model_run", {
  js <- gateway(model_input = get_default_input(), execution_id = "z",
                callback_url = "https://example.invalid/cb")
  expect_equal(nrow(jsonlite::fromJSON(js)), 1L)
  expect_equal(nrow(jsonlite::fromJSON(gateway())), 1L)
})
