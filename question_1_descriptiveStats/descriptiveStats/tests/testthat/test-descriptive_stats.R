test_that("calc_mean handles basic cases", {
  expect_equal(calc_mean(c(1, 2, 3)), 2)
  expect_true(is.na(calc_mean(numeric(0))))
  expect_true(is.na(calc_mean(c(NA, NA), na_rm = TRUE)))
  expect_error(calc_mean("a"), "`x` must be a numeric vector.")
})

test_that("calc_median handles basic cases", {
  expect_equal(calc_median(c(1, 2, 10, 20)), 6)
  expect_true(is.na(calc_median(numeric(0))))
  expect_equal(calc_median(c(1, NA, 3), na_rm = TRUE), 2)
})

test_that("calc_mode handles ties and no mode", {
  expect_equal(calc_mode(c(1, 2, 2, 3)), 2)
  expect_true(is.na(calc_mode(c(1, 2, 3))))
  expect_equal(calc_mode(c(1, 1, 2, 2)), c(1, 2))
  expect_equal(calc_mode(c("a", "b", "b")), "b")
})

test_that("quartiles and IQR work", {
  x <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
  expect_equal(calc_q1(x), as.numeric(stats::quantile(x, 0.25, type = 7, names = FALSE)))
  expect_equal(calc_q3(x), as.numeric(stats::quantile(x, 0.75, type = 7, names = FALSE)))
  expect_equal(calc_iqr(x), calc_q3(x) - calc_q1(x))
})