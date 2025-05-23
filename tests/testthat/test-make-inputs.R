## 'make_inputs_multi' --------------------------------------------------------

test_that("'make_inputs_multi' works with valid inputs - counts columns for 'totals' are numeric, reported includes all combin of classif vars", {
  totals_in <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 1:4
  )
  totals_out <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 11:14
  )
  reported_int <- data.frame(
    age = 0:1,
    region_orig = rep(c("b", "a"), each = 2),
    region_dest = rep(c("a", "b"), each = 2),
    count = 1:4
  )
  reported_im <- data.frame(
    age = 0:1,
    region_orig = "A",
    region_dest = rep(c("a", "b"), each = 2),
    count = 1:4
  )
  reported_em <- data.frame(
    age = 0:1,
    region_orig = rep(c("a", "b"), each = 2),
    region_dest = "A",
    count = 4:1
  )
  ans_obtained <- make_inputs_multi(
    totals_in = totals_in,
    totals_out = totals_out,
    reported_int = reported_int,
    reported_im = reported_im,
    reported_em = reported_em,
    epsilon = 0.001
  )
  ans_expected <- list(
    m = list(
      matrix(
        c(
          0L, 1L + 0.001, 1L + 0.001,
          3L + 0.001, 0L, 3L + 0.001,
          4L + 0.001, 2L + 0.001, 0L
        ),
        nr = 3,
        dimnames = list(
          region_orig = c("a", "b", "A"),
          region_dest = c("a", "b", "A")
        )
      ),
      matrix(
        c(
          0L, 2L + 0.001, 2L + 0.001,
          4L + 0.001, 0L, 4L + 0.001,
          3L + 0.001, 1L + 0.001, 0L
        ),
        nr = 3,
        dimnames = list(
          region_orig = c("a", "b", "A"),
          region_dest = c("a", "b", "A")
        )
      )
    ),
    colsums = list(
      c(a = 1L, b = 3L, A = NA),
      c(a = 2L, b = 4L, A = NA)
    ),
    rowsums = list(
      c(a = 11L, b = 13L, A = NA),
      c(a = 12L, b = 14L, A = NA)
    ),
    classif_vars = data.frame(age = 0:1),
    n_external = 1L
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_inputs_multi' works with valid inputs - counts columns for 'totals' are lists, reported includes all combin of classif vars", {
  totals_in <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2)
  )
  totals_in$count <- list(1:2, 3:4, 5:6, 7:8)
  totals_out <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2)
  )
  totals_out$count <- list(11:12, 13:14, 15:16, 17:18)
  reported_int <- data.frame(
    age = 0:1,
    region_orig = rep(c("b", "a"), each = 2),
    region_dest = rep(c("a", "b"), each = 2),
    count = 1:4
  )
  reported_im <- data.frame(
    age = 0:1,
    region_orig = "A",
    region_dest = rep(c("a", "b"), each = 2),
    count = 1:4
  )
  reported_em <- data.frame(
    age = 0:1,
    region_orig = rep(c("a", "b"), each = 2),
    region_dest = "A",
    count = 4:1
  )
  ans_obtained <- make_inputs_multi(
    totals_in = totals_in,
    totals_out = totals_out,
    reported_int = reported_int,
    reported_im = reported_im,
    reported_em = reported_em,
    epsilon = 0.001
  )
  ans_expected <- list(
    m = list(
      matrix(
        c(
          0L, 1L + 0.001, 1L + 0.001,
          3L + 0.001, 0L, 3L + 0.001,
          4L + 0.001, 2L + 0.001, 0L
        ),
        nr = 3,
        dimnames = list(
          region_orig = c("a", "b", "A"),
          region_dest = c("a", "b", "A")
        )
      ),
      matrix(
        c(
          0L, 2L + 0.001, 2L + 0.001,
          4L + 0.001, 0L, 4L + 0.001,
          3L + 0.001, 1L + 0.001, 0L
        ),
        nr = 3,
        dimnames = list(
          region_orig = c("a", "b", "A"),
          region_dest = c("a", "b", "A")
        )
      )
    ),
    colsums = list(
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
    ),
    rowsums = list(
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
    ),
    classif_vars = data.frame(age = 0:1),
    n_external = 1L
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_inputs_multi' works with valid inputs - counts columns for 'totals' are numeric, reported does not include all combinations of region within values for classif vars", {
  totals_in <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 1:4
  )
  totals_out <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 11:14
  )
  reported_int <- data.frame(
    age = 0:1,
    region_orig = rep(c("b", "a"), each = 2),
    region_dest = rep(c("a", "b"), each = 2),
    count = 1:4
  )[-1, ]
  reported_im <- data.frame(
    age = 0:1,
    region_orig = "A",
    region_dest = rep(c("a", "b"), each = 2),
    count = 1:4
  )[-2, ]
  reported_em <- data.frame(
    age = 0:1,
    region_orig = rep(c("a", "b"), each = 2),
    region_dest = "A",
    count = 4:1
  )[-3, ]
  ans_obtained <- make_inputs_multi(
    totals_in = totals_in,
    totals_out = totals_out,
    reported_int = reported_int,
    reported_im = reported_im,
    reported_em = reported_em,
    epsilon = 0.001
  )
  ans_expected <- list(
    m = list(
      matrix(
        c(
          0L, 0L + 0.001, 1L + 0.001,
          3L + 0.001, 0L, 3L + 0.001,
          4L + 0.001, 0L + 0.001, 0L
        ),
        nr = 3,
        dimnames = list(
          region_orig = c("a", "b", "A"),
          region_dest = c("a", "b", "A")
        )
      ),
      matrix(
        c(
          0L, 2L + 0.001, 0L + 0.001,
          4L + 0.001, 0L, 4L + 0.001,
          3L + 0.001, 1L + 0.001, 0L
        ),
        nr = 3,
        dimnames = list(
          region_orig = c("a", "b", "A"),
          region_dest = c("a", "b", "A")
        )
      )
    ),
    colsums = list(
      c(a = 1L, b = 3L, A = NA),
      c(a = 2L, b = 4L, A = NA)
    ),
    rowsums = list(
      c(a = 11L, b = 13L, A = NA),
      c(a = 12L, b = 14L, A = NA)
    ),
    classif_vars = data.frame(age = 0:1),
    n_external = 1L
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_inputs_multi' works with valid inputs - counts columns for 'totals' are numeric, reported completely omits one age group", {
  totals_in <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 1:4
  )
  totals_out <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 11:14
  )
  reported_int <- data.frame(
    age = 0:1,
    region_orig = rep(c("b", "a"), each = 2),
    region_dest = rep(c("a", "b"), each = 2),
    count = 1:4
  )[-c(1, 3), ]
  reported_im <- data.frame(
    age = 0:1,
    region_orig = "A",
    region_dest = rep(c("a", "b"), each = 2),
    count = 1:4
  )[-c(1, 3), ]
  reported_em <- data.frame(
    age = 0:1,
    region_orig = rep(c("a", "b"), each = 2),
    region_dest = "A",
    count = 4:1
  )[-c(2, 4), ]
  ans_obtained <- make_inputs_multi(
    totals_in = totals_in,
    totals_out = totals_out,
    reported_int = reported_int,
    reported_im = reported_im,
    reported_em = reported_em,
    epsilon = 0.001
  )
  ans_expected <- list(
    m = list(
      matrix(
        c(
          0L, 0L + 0.001, 0L + 0.001,
          0L + 0.001, 0L, 0L + 0.001,
          4L + 0.001, 2L + 0.001, 0L
        ),
        nr = 3,
        dimnames = list(
          region_orig = c("a", "b", "A"),
          region_dest = c("a", "b", "A")
        )
      ),
      matrix(
        c(
          0L, 2L + 0.001, 2L + 0.001,
          4L + 0.001, 0L, 4L + 0.001,
          0L + 0.001, 0L + 0.001, 0L
        ),
        nr = 3,
        dimnames = list(
          region_orig = c("a", "b", "A"),
          region_dest = c("a", "b", "A")
        )
      )
    ),
    colsums = list(
      c(a = 1L, b = 3L, A = NA),
      c(a = 2L, b = 4L, A = NA)
    ),
    rowsums = list(
      c(a = 11L, b = 13L, A = NA),
      c(a = 12L, b = 14L, A = NA)
    ),
    classif_vars = data.frame(age = 0:1),
    n_external = 1L
  )
  expect_identical(ans_obtained, ans_expected)
})





## 'make_tab_reported' --------------------------------------------------------

test_that("'make_tab_reported' works with valid inputs - reported_int", {
  reported_int <- data.frame(
    age = 0:1,
    region_orig = rep(c("c", "a"), each = 2),
    region_dest = rep(c("a", "c"), each = 2),
    count = 1:4
  )
  levels_orig <- c("a", "b", "c")
  levels_dest <- c("a", "b", "c")
  name <- "reported_int"
  ans_obtained <- make_tab_reported(
    reported = reported_int,
    levels_orig = levels_orig,
    levels_dest = levels_dest,
    name = name
  )
  ans_expected <- data.frame(age = 0:1)
  ans_expected$reported_int <- list(
    matrix(c(0L, 0L, 1L, 0L, 0L, 0L, 3L, 0L, 0L),
      nr = 3,
      dimnames = list(
        region_orig = c("a", "b", "c"),
        region_dest = c("a", "b", "c")
      )
    ),
    matrix(c(0L, 0L, 2L, 0L, 0L, 0L, 4L, 0L, 0L),
      nr = 3,
      dimnames = list(
        region_orig = c("a", "b", "c"),
        region_dest = c("a", "b", "c")
      )
    )
  )
  expect_identical(ans_obtained, ans_expected)
})


test_that("'make_tab_reported' works with valid inputs - reported_im", {
  reported_im <- data.frame(
    age = 0:1,
    region_orig = rep(c("A", "B"), each = 2),
    region_dest = rep(c("a", "b"), each = 4),
    count = 1:8
  )
  levels_orig <- c("A", "B")
  levels_dest <- c("a", "b", "c")
  name <- "reported_im"
  ans_obtained <- make_tab_reported(
    reported = reported_im,
    levels_orig = levels_orig,
    levels_dest = levels_dest,
    name = name
  )
  ans_expected <- data.frame(age = 0:1)
  ans_expected$reported_im <- list(
    matrix(c(1L, 3L, 5L, 7L, 0L, 0L),
      nr = 2,
      dimnames = list(
        region_orig = c("A", "B"),
        region_dest = c("a", "b", "c")
      )
    ),
    matrix(c(2L, 4L, 6L, 8L, 0L, 0L),
      nr = 2,
      dimnames = list(
        region_orig = c("A", "B"),
        region_dest = c("a", "b", "c")
      )
    )
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'make_tab_totals' ----------------------------------------------------------

test_that("'make_tab_totals' works with valid inputs - numeric 'count'", {
  totals_in <- data.frame(
    age = 0:1,
    region = rep(c("a", "b"), each = 2),
    count = 1:4
  )
  levels_reg <- c("a", "b", "c")
  name <- "totals_in"
  ans_obtained <- make_tab_totals(
    totals = totals_in,
    levels_reg = levels_reg,
    name = name
  )
  ans_expected <- data.frame(age = 0:1)
  ans_expected$totals_in <- list(
    c(a = 1L, b = 3L, c = 0L),
    c(a = 2L, b = 4L, c = 0L)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_tab_totals' works with valid inputs - list 'count'", {
  totals_in <- data.frame(
    age = 0:1,
    region = rep(c("c", "a"), each = 2)
  )
  totals_in$count <- list(1:2, 3:4, 5:6, 7:8)
  levels_reg <- c("a", "b", "c")
  name <- "totals_in"
  ans_obtained <- make_tab_totals(
    totals = totals_in,
    levels_reg = levels_reg,
    name = name
  )
  ans_expected <- data.frame(age = 0:1)
  ans_expected$totals_in <- list(
    matrix(c(5L, 0L, 1L, 6L, 0L, 2L),
      nr = 3,
      dimnames = list(region = c("a", "b", "c"), iter = NULL)
    ),
    matrix(c(7L, 0L, 3L, 8L, 0L, 4L),
      nr = 3,
      dimnames = list(region = c("a", "b", "c"), iter = NULL)
    )
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'make_vals_add' ------------------------------------------------------------

test_that("'make_vals_add' gives correct answer with valid inputs", {
  vals_reported <- cbind(
    internal = 4:2,
    external = 2:4
  )
  ans_obtained <- make_vals_add(vals_reported)
  ans_expected <- matrix(0, nrow = 3, ncol = 2)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_vals_alter' ----------------------------------------------------------

test_that("'make_vals_alter' gives correct answer with valid inputs - simple", {
  totals <- data.frame(
    age = c("0-4", "5+"),
    count = c(1, 3)
  )
  reported <- data.frame(
    age = c("0-4", "5+"),
    internal = c(10, 11),
    external = 2:1
  )
  alter <- list(internal = data.frame(
    age = c("0-4", "5+"),
    alter = c(2, 3)
  ))
  ans_obtained <- make_vals_alter(
    totals = totals,
    reported = reported,
    alter = alter
  )
  ans_expected <- cbind(
    internal = c(2, 3),
    external = c(1, 1)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_vals_alter' gives correct answer with valid inputs - NULL", {
  totals <- data.frame(
    age = c("0-4", "5+"),
    count = c(1, 3)
  )
  reported <- data.frame(
    age = c("0-4", "5+"),
    internal = c(10, 11),
    external = 2:1
  )
  alter <- NULL
  ans_obtained <- make_vals_alter(
    totals = totals,
    reported = reported,
    alter = alter
  )
  ans_expected <- cbind(
    internal = c(1, 1),
    external = c(1, 1)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_vals_alter' gives correct answer with valid inputs - complicated", {
  totals <- data.frame(
    age = c("0-4", "0-4", "5+"),
    sex = c("F", "M", "F"),
    count = c(1, 3, 2)
  )
  reported <- data.frame(
    age = c("0-4", "5+", "0-4"),
    sex = c("F", "F", "M"),
    internal = c(10, 11, 8),
    external = 2:0
  )
  alter <- list(internal = data.frame(
    sex = c("M", "F"),
    alter = c(2, 3)
  ))
  ans_obtained <- make_vals_alter(
    totals = totals,
    reported = reported,
    alter = alter
  )
  ans_expected <- cbind(
    internal = c(3, 2, 3),
    external = c(1, 1, 1)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_vals_alter' gives correct answer with valid inputs - list column", {
  totals <- data.frame(age = c("0-4", "5+"))
  totals$count <- list(1:3, 2:4)
  reported <- data.frame(
    age = c("0-4", "5+"),
    internal = c(10, 11),
    external = 2:1
  )
  alter <- list(internal = data.frame(
    age = c("0-4", "5+"),
    alter = c(2, 3)
  ))
  ans_obtained <- make_vals_alter(
    totals = totals,
    reported = reported,
    alter = alter
  )
  ans_expected <- cbind(
    internal = rep(c(2, 3), each = 3),
    external = rep(1, 6)
  )
  expect_identical(ans_obtained, ans_expected)
})





## 'make_vals_reported' -------------------------------------------------------

test_that("'make_vals_reported' gives correct answer with valid inputs - simple", {
  totals <- data.frame(
    age = c("0-4", "5+"),
    count = c(1, 3)
  )
  reported <- data.frame(
    age = c("0-4", "5+"),
    internal = c(10, 11),
    external = 2:1
  )
  ans_obtained <- make_vals_reported(
    totals = totals,
    reported = reported
  )
  ans_expected <- matrix(c(10, 11, 2, 1),
    nrow = 2
  )
  colnames(ans_expected) <- c("internal", "external")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_vals_reported' gives correct answer with valid inputs - complicated", {
  totals <- data.frame(
    age = c("0-4", "5+", "5+", "0-4"),
    sex = c("F", "F", "M", "M"),
    count = 1:4,
    time = c(2001, 2000, 2001, 2000)
  )
  reported <- data.frame(
    time = 2000:2001,
    internal = c(5, NA),
    external = 2:1
  )
  ans_obtained <- make_vals_reported(
    totals = totals,
    reported = reported
  )
  ans_expected <- cbind(
    internal = c(NA, 5, NA, 5),
    external = c(1, 2, 1, 2)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_vals_reported' replaces rows of zeros with rows of ones", {
  totals <- data.frame(
    age = c("0-4", "5+", "5+", "0-4"),
    sex = c("F", "F", "M", "M"),
    count = 1:4,
    time = c(2001, 2000, 2001, 2000)
  )
  reported <- data.frame(
    time = 2000:2001,
    internal = c(5, 0),
    external = c(2, 0)
  )
  ans_obtained <- make_vals_reported(
    totals = totals,
    reported = reported
  )
  ans_expected <- cbind(
    internal = c(1, 5, 1, 5),
    external = c(1, 2, 1, 2)
  )
  expect_identical(ans_obtained, ans_expected)
})



## 'make_vals_total' ----------------------------------------------------------

test_that("'make_vals_total' gives correct answer with valid inputs - numeric count", {
  totals <- data.frame(
    age = c("0-4", "5+"),
    count = c(1, 3)
  )
  ans_obtained <- make_val_total(totals)
  ans_expected <- totals$count
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_vals_total' gives correct answer with valid inputs - list count", {
  totals <- data.frame(age = c("0-4", "5+"))
  totals$count <- list(c(1, 5), c(3, 1))
  ans_obtained <- make_val_total(totals)
  ans_expected <- c(1, 5, 3, 1)
  expect_identical(ans_obtained, ans_expected)
})
