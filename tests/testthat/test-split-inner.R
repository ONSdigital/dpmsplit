## 'split_single_inner' -------------------------------------------------------

test_that("'split_single_inner' gives expected answer with typical inputs", {
  split1 <- function(y, u, a) {
    w <- u * a
    u + (w / sum(w)) * (y - sum(u))
  }
  val_total <- 110
  vals_reported <- matrix(c(16, 48, 36), nrow = 1)
  vals_add <- matrix(rep(0, 3), nrow = 1)
  ## proportional adjustment
  vals_alter <- matrix(rep(1, 3), nrow = 1)
  ans_obtained <- split_single_inner(
    val_total = val_total,
    vals_reported = vals_reported,
    vals_alter = vals_alter,
    vals_add = vals_add
  )
  ans_expected <- split1(
    y = val_total,
    u = vals_reported,
    a = vals_alter
  )
  expect_equal(ans_obtained, ans_expected)
  ## additive adjustment
  vals_alter <- 1 / vals_reported
  ans_obtained <- split_single_inner(
    val_total = val_total,
    vals_reported = vals_reported,
    vals_alter = vals_alter,
    vals_add = vals_add
  )
  ans_expected <- split1(
    y = val_total,
    u = vals_reported,
    a = vals_alter
  )
  expect_equal(ans_obtained, ans_expected)
  ## varying weights
  vals_alter <- c(0.02, 0.2, 0.02)
  ans_obtained <- split_single_inner(
    val_total = val_total,
    vals_reported = vals_reported,
    vals_alter = vals_alter,
    vals_add = vals_add
  )
  ans_expected <- split1(
    y = val_total,
    u = vals_reported,
    a = vals_alter
  )
  expect_equal(ans_obtained, ans_expected)
})

test_that("'split_single_inner' leaves values unchanged when val_total and vals_reported consistent", {
  val_total <- c(10, 20, 5)
  vals_reported <- matrix(
    c(
      3, 5, 2,
      0, 15, 5,
      2, 2, 1
    ),
    nrow = 3,
    byrow = TRUE
  )
  vals_alter <- matrix(
    c(
      1, 1, 1,
      2, 1, 5,
      3, 2, 1
    ),
    nrow = 3,
    byrow = TRUE
  )
  vals_add <- matrix(0, nrow = 3, ncol = 3)
  expect_equal(
    split_single_inner(
      val_total = val_total,
      vals_reported = vals_reported,
      vals_alter = vals_alter,
      vals_add = vals_add
    ),
    vals_reported
  )
})

test_that("'split_single_inner' leaves values unchanged when val_alter is 0", {
  val_total <- c(8, 25, 4)
  vals_reported <- matrix(
    c(
      3, 5, 2,
      0, 15, 5,
      2, 2, 1
    ),
    nrow = 3,
    byrow = TRUE
  )
  vals_alter <- matrix(
    c(
      0, 1, 1,
      2, 0, 5,
      3, 2, 0
    ),
    nrow = 3,
    byrow = TRUE
  )
  vals_add <- matrix(0, nrow = 3, ncol = 3)
  ans_obtained <- split_single_inner(
    val_total = val_total,
    vals_reported = vals_reported,
    vals_alter = vals_alter,
    vals_add = vals_add
  )
  expect_equal(diag(ans_obtained), diag(vals_reported))
})

test_that("'split_single_inner' avoids negative values - simple case", {
  val_total <- 4
  vals_reported <- matrix(c(1, 5, 2), nrow = 1)
  vals_alter <- matrix(c(10, 1, 1), nrow = 1)
  vals_add <- matrix(rep(0, 3), nrow = 1)
  ans_obtained <- split_single_inner(
    val_total = val_total,
    vals_reported = vals_reported,
    vals_alter = vals_alter,
    vals_add = vals_add
  )
  val_total_new <- val_total - vals_reported[1]
  vals_reported_new <- vals_reported
  vals_reported_new[1] <- 0
  alpha <- vals_reported_new * vals_alter / sum(vals_reported_new * vals_alter)
  ans_expected <- vals_reported_new + alpha * (val_total_new - rowSums(vals_reported_new))
  ans_expected <- ans_expected + c(vals_reported[1], 0, 0)
  expect_equal(ans_obtained, ans_expected)
  expect_true(all(ans_expected >= 0))
  expect_equal(rowSums(ans_obtained), val_total)
})

test_that("'split_single_inner' avoids negative values - two rounds required", {
  val_total <- 4
  vals_reported <- matrix(c(1, 2, 5), nrow = 1)
  vals_alter <- matrix(c(100, 10, 1), nrow = 1)
  vals_add <- matrix(rep(0, 3), nrow = 1)
  ans_obtained <- split_single_inner(
    val_total = val_total,
    vals_reported = vals_reported,
    vals_alter = vals_alter,
    vals_add = vals_add
  )
  val_total_new <- val_total - vals_reported[1]
  vals_reported_new <- vals_reported
  vals_reported_new[1] <- 0
  val_total_new <- val_total_new - vals_reported[2]
  vals_reported_new[2] <- 0
  alpha <- vals_reported_new * vals_alter / sum(vals_reported_new * vals_alter)
  ans_expected <- vals_reported_new + alpha * (val_total_new - rowSums(vals_reported_new))
  ans_expected <- ans_expected + c(vals_reported[1:2], 0)
  expect_equal(ans_obtained, ans_expected)
  expect_true(all(ans_expected >= 0))
  expect_equal(rowSums(ans_obtained), val_total)
})


## 'split_multi_inner' -------------------------------------------------------

test_that("'split_multi_inner' gives expected answer - totals_in$count is numeric", {
  classif_vars <- data.frame(age = 0:1)
  m <- list(
    matrix(c(0L, 1L, 1L, 3L, 0L, 3L, 4L, 2L, 0L),
      nr = 3,
      dimnames = list(
        region_orig = c("a", "b", "A"),
        region_dest = c("a", "b", "A")
      )
    ),
    matrix(c(0L, 2L, 2L, 4L, 0L, 4L, 3L, 1L, 0L),
      nr = 3,
      dimnames = list(
        region_orig = c("a", "b", "A"),
        region_dest = c("a", "b", "A")
      )
    )
  )
  colsums <- list(
    c(a = 1L, b = 3L, A = NA),
    c(a = 2L, b = 4L, A = NA)
  )
  rowsums <- list(
    c(a = 11L, b = 13L, A = NA),
    c(a = 12L, b = 14L, A = NA)
  )
  ans_obtained <- split_multi_inner(
    m = m,
    colsums = colsums,
    rowsums = rowsums,
    max_iter = 100,
    tolerance = 0.000001
  )
  ans_expected <- list(
    ipf(
      m = m[[1]],
      rowsums = rowsums[[1]],
      colsums = colsums[[1]],
      max_iter = 100,
      tolerance = 0.000001
    ),
    ipf(
      m = m[[2]],
      rowsums = rowsums[[2]],
      colsums = colsums[[2]],
      max_iter = 100,
      tolerance = 0.000001
    )
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'split_multi_inner' gives expected answer - totals_in$count is list", {
  classif_vars <- data.frame(age = 0:1)
  m <- list(
    matrix(c(0L, 1L, 1L, 3L, 0L, 3L, 4L, 2L, 0L),
      nr = 3,
      dimnames = list(
        region_orig = c("a", "b", "A"),
        region_dest = c("a", "b", "A")
      )
    ),
    matrix(c(0L, 2L, 2L, 4L, 0L, 4L, 3L, 1L, 0L),
      nr = 3,
      dimnames = list(
        region_orig = c("a", "b", "A"),
        region_dest = c("a", "b", "A")
      )
    )
  )
  colsums <- list(
    matrix(c(1L, 5L, NA, 2L, 6L, NA),
      nr = 3,
      dimnames = list(
        region = c("a", "b", "A"),
        iter = NULL
      )
    ),
    matrix(c(3L, 7L, NA, 4L, 8L, NA),
      nr = 3,
      dimnames = list(
        region = c("a", "b", "A"),
        iter = NULL
      )
    )
  )
  rowsums <- list(
    matrix(c(11L, 15L, NA, 12L, 16L, NA),
      nr = 3,
      dimnames = list(
        region = c("a", "b", "A"),
        iter = NULL
      )
    ),
    matrix(c(13L, 17L, NA, 14L, 18L, NA),
      nr = 3,
      dimnames = list(
        region = c("a", "b", "A"),
        iter = NULL
      )
    )
  )
  ans_obtained <- split_multi_inner(
    m = m,
    colsums = colsums,
    rowsums = rowsums,
    max_iter = 100,
    tolerance = 0.000001
  )
  ans_expected <- list(
    ipf_iter(
      m = m[[1]],
      rowsums = rowsums[[1]],
      colsums = colsums[[1]],
      max_iter = 100,
      tolerance = 0.000001
    ),
    ipf_iter(
      m = m[[2]],
      rowsums = rowsums[[2]],
      colsums = colsums[[2]],
      max_iter = 100,
      tolerance = 0.000001
    )
  )
  expect_identical(ans_obtained, ans_expected)
})
