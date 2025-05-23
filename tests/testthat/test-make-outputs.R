## 'make_ans' -----------------------------------------------------------------

test_that("'make_classif_vars' gives correct answer with valid inputs - numeric counts", {
  classif_vars <- data.frame(
    age = c("0-4", "0-4", "5+"),
    sex = c("F", "M", "F")
  )
  vals_split <- cbind(
    internal = 1:3,
    external = 11:13
  )
  ans_obtained <- make_ans(
    classif_vars = classif_vars,
    vals_split = vals_split
  )
  ans_expected <- data.frame(
    age = c("0-4", "0-4", "5+"),
    sex = c("F", "M", "F"),
    internal = 1:3,
    external = 11:13
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_classif_vars' gives correct answer with valid inputs - list counts", {
  classif_vars <- data.frame(
    age = c("0-4", "0-4", "5+"),
    sex = c("F", "M", "F")
  )
  vals_split <- cbind(
    internal = 1:9,
    external = 11:19
  )
  ans_obtained <- make_ans(
    classif_vars = classif_vars,
    vals_split = vals_split
  )
  ans_expected <- data.frame(
    age = c("0-4", "0-4", "5+"),
    sex = c("F", "M", "F")
  )
  ans_expected$internal <- list(1:3, 4:6, 7:9)
  ans_expected$external <- list(11:13, 14:16, 17:19)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_classif_vars' --------------------------------------------------------

test_that("'make_classif_vars' gives correct answer with valid inputs", {
  totals <- data.frame(
    age = c("0-4", "0-4", "5+"),
    sex = c("F", "M", "F"),
    count = c(1, 3, 2)
  )
  ans_obtained <- make_classif_vars(totals = totals)
  ans_expected <- data.frame(
    age = c("0-4", "0-4", "5+"),
    sex = c("F", "M", "F")
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'make_outputs_multi' -------------------------------------------------------

test_that("'make_outputs_multi' gives expected answer - totals_in$count is numeric", {
  vals <- list(
    matrix(c(0, 2:3, 4, 0, 5, 6:7, 0),
      nr = 3,
      dimnames = list(
        region_orig = c("a", "b", "A"),
        region_dest = c("a", "b", "A")
      )
    ),
    matrix(c(0, 12:13, 14, 0, 15, 16:17, 0),
      nr = 3,
      dimnames = list(
        region_orig = c("a", "b", "A"),
        region_dest = c("a", "b", "A")
      )
    )
  )
  classif_vars <- data.frame(sex = c("f", "m"), stringsAsFactors = TRUE)
  n_external <- 1L
  ans_obtained <- make_outputs_multi(
    vals = vals,
    classif_vars = classif_vars,
    n_external = n_external
  )
  ans_expected <- list(
    internal = data.frame(
      sex = c("f", "f", "f", "f", "m", "m", "m", "m"),
      region_orig = c("a", "b", "a", "b", "a", "b", "a", "b"),
      region_dest = c("a", "a", "b", "b", "a", "a", "b", "b"),
      count = c(0, 2, 4, 0, 0, 12, 14, 0),
      stringsAsFactors = TRUE
    ),
    immigration = data.frame(
      sex = c("f", "f", "m", "m"),
      region_orig = c("A", "A", "A", "A"),
      region_dest = c("a", "b", "a", "b"),
      count = c(3, 5, 13, 15),
      stringsAsFactors = TRUE
    ),
    emigration = data.frame(
      sex = c("f", "f", "m", "m"),
      region_orig = c("a", "b", "a", "b"),
      region_dest = c("A", "A", "A", "A"),
      count = c(6, 7, 16, 17),
      stringsAsFactors = TRUE
    )
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_outputs_multi' gives expected answer - totals_in$count is list", {
  vals <- list(
    array(c(0, 2:3, 4, 0, 5, 6:7, 0),
      dim = c(3, 3, 2),
      dimnames = list(
        region_orig = c("a", "b", "A"),
        region_dest = c("a", "b", "A"),
        iter = NULL
      )
    ),
    array(c(0, 12:13, 14, 0, 15, 16:17, 0),
      dim = c(3, 3, 2),
      dimnames = list(
        region_orig = c("a", "b", "A"),
        region_dest = c("a", "b", "A"),
        iter = NULL
      )
    )
  )
  classif_vars <- data.frame(sex = c("f", "m"), stringsAsFactors = TRUE)
  n_external <- 1L
  ans_obtained <- make_outputs_multi(
    vals = vals,
    classif_vars = classif_vars,
    n_external = n_external
  )
  internal <- data.frame(
    sex = c("f", "f", "f", "f", "m", "m", "m", "m"),
    region_orig = c("a", "b", "a", "b", "a", "b", "a", "b"),
    region_dest = c("a", "a", "b", "b", "a", "a", "b", "b"),
    stringsAsFactors = TRUE
  )
  internal$count <- lapply(c(0, 2, 4, 0, 0, 12, 14, 0), rep, times = 2)
  immigration <- data.frame(
    sex = c("f", "f", "m", "m"),
    region_orig = c("A", "A", "A", "A"),
    region_dest = c("a", "b", "a", "b"),
    stringsAsFactors = TRUE
  )
  immigration$count <- lapply(c(3, 5, 13, 15), rep, times = 2)
  emigration <- data.frame(
    sex = c("f", "f", "m", "m"),
    region_orig = c("a", "b", "a", "b"),
    region_dest = c("A", "A", "A", "A"),
    stringsAsFactors = TRUE
  )
  emigration$count <- lapply(c(6, 7, 16, 17), rep, times = 2)
  ans_expected <- list(
    internal = internal,
    immigration = immigration,
    emigration = emigration
  )
  expect_identical(ans_obtained, ans_expected)
})
