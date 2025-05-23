## 'ipf' ----------------------------------------------------------------------

test_that("'ipf' gives valid answer when 'rowsums' and 'columns' have no NAs", {
  for (seed in seq_len(5)) {
    set.seed(seed)
    tmp <- matrix(rpois(n = 36, lambda = 10), nrow = 6, ncol = 6)
    rowsums <- rowSums(tmp)
    colsums <- colSums(tmp)
    m <- matrix(rpois(n = 36, lambda = 4), nrow = 6, ncol = 6)
    ans <- ipf(m = m, rowsums = rowsums, colsums = colsums, max_iter = 1000, tolerance = 1e-6)
    expect_equal(rowSums(ans), rowsums, tolerance = 1e-6)
    expect_equal(colSums(ans), colsums, tolerance = 1e-6)
    expect_identical(m == 0, ans == 0)
  }
})

test_that("'ipf' gives valid answer when 'm' is non-square - no NAs", {
  for (seed in seq_len(5)) {
    set.seed(seed)
    tmp <- matrix(rpois(n = 48, lambda = 10), nrow = 6, ncol = 8)
    rowsums <- rowSums(tmp)
    colsums <- colSums(tmp)
    m <- matrix(rpois(n = 48, lambda = 4), nrow = 6, ncol = 8)
    ans <- ipf(m = m, rowsums = rowsums, colsums = colsums, max_iter = 1000, tolerance = 1e-6)
    expect_equal(rowSums(ans), rowsums, tolerance = 1e-6)
    expect_equal(colSums(ans), colsums, tolerance = 1e-6)
    expect_identical(m == 0, ans == 0)
  }
})

test_that("'ipf' gives valid answer when 'm' is non-square - with NAs", {
  for (seed in seq_len(5)) {
    set.seed(seed)
    tmp <- matrix(rpois(n = 48, lambda = 10), nrow = 6, ncol = 8)
    rowsums <- rowSums(tmp)
    rowsums[2] <- NA
    colsums <- colSums(tmp)
    colsums[5] <- NA
    m <- matrix(rpois(n = 48, lambda = 4), nrow = 6, ncol = 8)
    ans <- ipf(m = m, rowsums = rowsums, colsums = colsums, max_iter = 1000, tolerance = 1e-6)
    expect_equal(rowSums(ans)[-2], rowsums[-2], tolerance = 1e-6)
    expect_equal(colSums(ans)[-5], colsums[-5], tolerance = 1e-6)
    expect_identical(m == 0, ans == 0)
  }
})

test_that("'ipf' gives valid answer when 'rowsums' and 'columns' have NAs", {
  for (seed in seq_len(5)) {
    set.seed(seed)
    tmp <- matrix(rpois(n = 36, lambda = 10), nrow = 6, ncol = 6)
    rowsums <- rowSums(tmp)
    colsums <- colSums(tmp)
    rowsums[1] <- NA
    colsums[6] <- NA
    m <- matrix(rpois(n = 36, lambda = 4), nrow = 6, ncol = 6)
    ans <- ipf(m = m, rowsums = rowsums, colsums = colsums, max_iter = 1000, tolerance = 1e-6)
    expect_equal(rowSums(ans)[-1], rowsums[-1], tolerance = 1e-6)
    expect_equal(colSums(ans)[-6], colsums[-6], tolerance = 1e-6)
    expect_identical(m == 0, ans == 0)
    expect_true(!any(is.na(ans)))
  }
})

test_that("'ipf' gives valid answer when 'rowsums' have 0s", {
  for (seed in seq_len(5)) {
    set.seed(seed)
    tmp <- matrix(rpois(n = 36, lambda = 2), nrow = 6, ncol = 6)
    tmp[1, ] <- 0
    rowsums <- rowSums(tmp)
    colsums <- colSums(tmp)
    m <- matrix(rpois(n = 36, lambda = 4), nrow = 6, ncol = 6)
    ans <- ipf(m = m, rowsums = rowsums, colsums = colsums, max_iter = 1000, tolerance = 1e-6)
    expect_equal(rowSums(ans), rowsums, tolerance = 1e-6)
    expect_equal(colSums(ans), colsums, tolerance = 1e-6)
    expect_identical(m[-1, ] == 0, ans[-1, ] == 0)
    expect_true(!any(is.na(ans)))
  }
})

test_that("'ipf' gives valid answer when 'colsums' have 0s", {
  for (seed in seq_len(5)) {
    set.seed(seed)
    tmp <- matrix(rpois(n = 36, lambda = 10), nrow = 6, ncol = 6)
    tmp[, 3] <- 0
    rowsums <- rowSums(tmp)
    colsums <- colSums(tmp)
    m <- matrix(rpois(n = 36, lambda = 2), nrow = 6, ncol = 6)
    ans <- ipf(m = m, rowsums = rowsums, colsums = colsums, max_iter = 1000, tolerance = 1e-6)
    expect_equal(rowSums(ans), rowsums, tolerance = 1e-6)
    expect_equal(colSums(ans), colsums, tolerance = 1e-6)
    expect_identical(m[, -3] == 0, ans[, -3] == 0)
    expect_true(!any(is.na(ans)))
  }
})

test_that("C and R versions of 'ipf' gives same answers", {
  ipf_R <- function(m, rowsums, colsums, max_iter, tolerance) {
    nrow <- nrow(m)
    is_na_rows <- is.na(rowsums)
    is_na_cols <- is.na(colsums)
    is_zero_rows <- ifelse(is_na_rows, FALSE, rowsums == 0)
    is_zero_cols <- ifelse(is_na_cols, FALSE, colsums == 0)
    is_rescale_rows <- !is_na_rows & !is_zero_rows
    is_rescale_cols <- !is_na_cols & !is_zero_cols
    m[is_zero_rows, ] <- 0
    m[, is_zero_cols] <- 0
    found_ans <- FALSE
    rowsums_m <- rowSums(m)
    for (i in seq_len(max_iter)) {
      mult_rows <- ifelse(is_rescale_rows, rowsums / rowsums_m, 1)
      m <- mult_rows * m
      colsums_m <- colSums(m)
      mult_cols <- ifelse(is_rescale_cols, colsums / colsums_m, 1)
      mult_cols <- rep(mult_cols, each = nrow)
      m <- mult_cols * m
      rowsums_m <- rowSums(m)
      if (all(abs(rowsums_m - rowsums) < tolerance, na.rm = TRUE)) {
        found_ans <- TRUE
        return(m)
      }
    }
    if (!found_ans) {
      stop("did not converge")
    }
  }
  for (seed in seq_len(5)) {
    set.seed(seed)
    tmp <- matrix(rpois(n = 48, lambda = 10), nrow = 6, ncol = 8)
    rowsums <- rowSums(tmp)
    rowsums[2] <- NA
    colsums <- colSums(tmp)
    colsums[5] <- NA
    colsums[6] <- 0
    m <- matrix(rpois(n = 48, lambda = 4), nrow = 6, ncol = 8)
    ans_C <- ipf(m = m, rowsums = rowsums, colsums = colsums, max_iter = 1000, tolerance = 1e-6)
    ans_R <- ipf_R(m = m, rowsums = rowsums, colsums = colsums, max_iter = 1000, tolerance = 1e-6)
    expect_true(max(abs(ans_C - ans_R)) < 2e-6)
  }
})

test_that("'ipf' gives expected error message when can't converge", {
  m <- matrix(c(0, 1, 0, 0), nr = 2)
  rowsums <- c(0, 1)
  colsums <- c(0, 1)
  sink(tempfile())
  expect_error(
    ipf(m = m, rowsums = rowsums, colsums = colsums, max_iter = 10, tolerance = 1e-6),
    "IPF failed to converge after 10 iterations"
  )
  sink()
})

test_that("'ipf' leaves 'm' unchanged", {
  m <- matrix(1:4, nr = 2)
  rowsums <- 1:2
  colsums <- 1:2
  ans <- ipf(m = m, rowsums = rowsums, colsums = colsums, max_iter = 10, tolerance = 1e-6)
  expect_identical(m, matrix(1:4, nr = 2))
})
