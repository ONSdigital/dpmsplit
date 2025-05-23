## 'array3d_to_listdf' --------------------------------------------------------

test_that("'array3d_to_listdf' gives expected answer with valid input", {
  x <- array(1:8,
    dim = c(2, 2, 2),
    dimnames = list(
      region_orig = c("a", "b"),
      region_dest = c("a", "b")
    )
  )
  ans_obtained <- array3d_to_listdf(x)
  ans_expected <- data.frame(
    region_orig = c("a", "b", "a", "b"),
    region_dest = c("a", "a", "b", "b"),
    stringsAsFactors = TRUE
  )
  ans_expected$count <- list(c(1L, 5L), c(2L, 6L), c(3L, 7L), c(4L, 8L))
  expect_identical(ans_obtained, ans_expected)
})



## 'ipf_iter' ----------------------------------------------------------------------

test_that("'ipf_iter' gives valid answer with valid input", {
  for (seed in seq_len(5)) {
    set.seed(seed)
    tmp <- array(rpois(n = 72, lambda = 10),
      dim = c(6, 6, 2),
      dimnames = list(region_orig = 1:6, region_dest = 1:6, iter = NULL)
    )
    rowsums <- apply(tmp, c(1, 3), sum)
    colsums <- apply(tmp, c(2, 3), sum)
    m <- matrix(rpois(n = 36, lambda = 4),
      nrow = 6, ncol = 6,
      dimnames = list(region_orig = 1:6, region_dest = 1:6)
    )
    ans <- ipf_iter(m = m, rowsums = rowsums, colsums = colsums, max_iter = 1000, tolerance = 1e-6)
    expect_equal(apply(ans, c(1, 3), sum), rowsums, tolerance = 1e-6)
    expect_equal(apply(ans, c(2, 3), sum), colsums, tolerance = 1e-6)
    for (i in 1:2) {
      expect_identical(m == 0, ans[, , i] == 0)
    }
  }
})


## 'nms_counts_reported' ------------------------------------------------------

test_that("'nms_counts_reported' gives correct answer with valid inputs", {
  totals <- data.frame(
    age = 1,
    sex = "F",
    time = 2001,
    cohort = 2000,
    count = 100
  )
  reported <- data.frame(
    age = 0,
    time = 2001,
    internal = 10,
    external = 5
  )
  ans_obtained <- nms_counts_reported(totals = totals, reported = reported)
  ans_expected <- c("internal", "external")
  expect_identical(ans_obtained, ans_expected)
})


## 'simple_left_join' ---------------------------------------------------------

test_that("'simple_left_join' gives correct answer with valid inputs", {
  x <- data.frame(a = 1:5, b = 3:7, x = 1:5)
  y <- data.frame(b = 1:10, y = 11:20)
  ans_obtained <- simple_left_join(x = x, y = y)
  ans_expected <- data.frame(x, y = 13:17)
  expect_identical(ans_obtained, ans_expected)
})


## 'to_list_col' --------------------------------------------------------------

test_that("'to_list_col' gives correct answer with valid inputs", {
  x <- 1:20
  n_iter <- 5L
  ans_obtained <- to_list_col(x = x, n_iter = n_iter)
  ans_expected <- list(1:5, 6:10, 11:15, 16:20)
  expect_identical(ans_obtained, ans_expected)
})

## 'sort_df' -----------------------------------------------------------------
test_that("sort_df sorts by all columns when ignore is NULL", {
  df <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 4:1
  )
  sorted <- sort_df(df)
  expect_equal(sorted$age, c(0, 0, 1, 1))
  expect_equal(sorted$region, c("a", "b", "a", "b"))
  expect_equal(sorted$count, c(4, 2, 3, 1))
})

test_that("sort_df ignores specified columns", {
  df <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 4:1
  )
  sorted <- sort_df(df, ignore = "age")
  expect_equal(sorted$age, c(1, 0, 1, 0))
  expect_equal(sorted$region, c("a", "a", "b", "b"))
  expect_equal(sorted$count, c(3, 4, 1, 2))
})

test_that("sort_df works with multiple ignore columns", {
  df <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 4:1
  )
  sorted <- sort_df(df, ignore = c("region", "count"))
  expect_equal(sorted$age, c(0, 0, 1, 1))
  expect_equal(sorted$region, c("a", "b", "a", "b"))
  expect_equal(sorted$count, c(4, 2, 3, 1))
})

test_that("sort_df throws error for invalid ignore column", {
  df <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 4:1
  )
  expect_error(sort_df(df, ignore = "sex"), "\"sex\" is not a valid column name")
})

test_that("sort_df resets row names", {
  df <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 4:1,
    row.names = c("p", "q", "r", "s")
  )
  sorted <- sort_df(df)
  expect_equal(rownames(sorted), c("1", "2", "3", "4"))
})
