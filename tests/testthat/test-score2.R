# Expected values are MDCalc's SCORE2 and SCORE2-OP calculators (1 dp). An
# independent implementation (RiskScorescvd) agrees on all of them.
oracle <- data.frame(
  sex = c("male","female","male","female","male","female","male","female","male","female",
          "male","female","male","female","male","female"),
  age = c(50,50,65,45,40,68,58,62,69,55, 73,73,80,75,70,85),
  smoking = c(0,0,1,1,0,0,1,1,1,0, 0,0,1,0,1,1),
  sbp = c(140,140,160,120,110,170,135,150,180,125, 150,150,170,140,130,160),
  total_chol = c(6.3,6.3,7.0,5.0,4.0,6.5,5.5,7.5,8.0,4.5, 6.0,6.0,5.0,6.5,7.0,5.5),
  hdl = c(1.4,1.4,1.0,1.6,1.5,1.1,1.2,0.9,0.8,2.0, 1.4,1.4,1.0,1.6,1.2,1.3),
  diabetes = c(0,0,0,0,0,0,0,0,0,0, 0,0,1,1,0,1),
  region = c("moderate","moderate","very_high","low","low","high","high","very_high",
             "moderate","high", "moderate","moderate","very_high","low","high","high"),
  pct = c(4.5,2.5,39.0,2.0,0.9,18.9,12.1,36.2,32.0,1.9, 16.6,12.0,56.1,16.9,23.5,71.9),
  stringsAsFactors = FALSE
)

test_that("SCORE2 and SCORE2-OP match MDCalc on 16 patients", {
  for (i in seq_len(nrow(oracle))) {
    args <- as.list(oracle[i, setdiff(names(oracle), "pct")])
    r <- do.call(score2, args)
    expect_lt(abs(100 * r$risk - oracle$pct[i]), 0.051, label = paste("row", i))
    expect_equal(r$model, if (oracle$age[i] < 70) "SCORE2" else "SCORE2-OP")
  }
})

test_that("risk rises with region, from low to very high", {
  r <- vapply(c("low", "moderate", "high", "very_high"), function(g)
    score2(55, "male", "yes", 140, 6, 1.3, region = g)$risk, numeric(1))
  expect_true(all(diff(r) > 0))
})

test_that("mg/dL cholesterol gives the same answer as mmol/L", {
  a <- score2(50, "male", "no", 140, 6.3, 1.4, region = "moderate")
  b <- score2(50, "male", "no", 140, 6.3 * 38.67, 1.4 * 38.67,
              region = "moderate", chol_units = "mg/dL")
  expect_equal(a$risk, b$risk, tolerance = 1e-12)
  expect_error(score2(50, "male", "no", 140, 240, 50, region = "moderate"), "mg/dL")
})

test_that("out-of-range and unknown inputs are refused", {
  expect_error(score2(35, "male", "no", 140, 6, 1.3, region = "low"), "40-89")
  expect_error(score2(92, "male", "no", 140, 6, 1.3, region = "low"), "40-89")
  expect_error(score2(50, "male", "no", 140, 6, 1.3, region = "mars"), "region")
  expect_error(model_run(list(age = 50, sex = "male")), "Missing required")
})

test_that("JSON nulls and empty strings mean not supplied", {
  r <- model_run(list(age = 50, sex = "male", smoking = "no", sbp = 140,
                      total_chol = 6.3, hdl = 1.4, region = "moderate",
                      diabetes = NULL, chol_units = ""))
  expect_lt(abs(100 * r$risk - 4.5), 0.051)
})
