## 'check_alter' --------------------------------------------------------------

test_that("'check_alter' returns TRUE when 'alter' is valid", {
  alter <- list(
    internal = 3,
    external = data.frame(
      age = c("0-4", "5+"),
      alter = 2:1
    )
  )
  expect_true(check_alter(alter))
  alter <- NULL
  expect_true(check_alter(NULL))
})

test_that("'check_alter' throws correct error when 'alter' is not a list or NULL", {
  expect_error(
    check_alter(1L),
    "'alter' has class 'integer'"
  )
})

test_that("'check_alter' throws correct error when 'alter' does not have names", {
  alter <- list(
    data.frame(
      age = c("0-4", "5+"),
      alter = 1:2
    ),
    data.frame(
      age = c("0-4", "5+"),
      alter = 2:1
    )
  )
  expect_error(
    check_alter(alter),
    "'alter' does not have names"
  )
})

test_that("'check_alter' throws correct error when 'alter' has duplicated names", {
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = 1:2
    ),
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = 2:1
    )
  )
  expect_error(
    check_alter(alter),
    "'alter' has duplicated names"
  )
})

test_that("'check_alter' throws correct error when numeric element of 'alter' does not have length 1", {
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = 1:2
    ),
    external = 1:2
  )
  expect_error(
    check_alter(alter),
    "element 2 of 'alter' has length 2"
  )
})

test_that("'check_alter' throws correct error when numeric element of 'alter' is NA", {
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = 1:2
    ),
    external = NA_real_
  )
  expect_error(
    check_alter(alter),
    "element 2 of 'alter' is NA"
  )
})

test_that("'check_alter' throws correct error when numeric element of 'alter' is negative", {
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = 1:2
    ),
    external = -1
  )
  expect_error(
    check_alter(alter),
    "element 2 of 'alter' is negative"
  )
})

test_that("'check_alter' throws correct error when element of 'alter' is not numeric or data frame", {
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = 1:2
    ),
    external = "a"
  )
  expect_error(
    check_alter(alter),
    "element 2 of 'alter' has class 'character'"
  )
})

test_that("'check_alter' throws correct error when element of 'alter' has less than 2 columns", {
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = 1:2
    ),
    external = data.frame(alter = 1:2)
  )
  expect_error(
    check_alter(alter),
    "element 2 of 'alter' has less than 2 columns"
  )
})

test_that("'check_alter' throws correct error when element of 'alter' has column called 'count'", {
  alter <- list(
    internal = data.frame(
      count = c("0-4", "5+"),
      alter = 1:2
    ),
    external = data.frame(
      age = c("0-4", "5+"),
      alter = 2:1
    )
  )
  expect_error(
    check_alter(alter),
    "element 1 of 'alter' has a column called 'count'"
  )
})

test_that("'check_alter' throws correct error when element of 'alter' does not have column called 'alter'", {
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = 1:2
    ),
    external = data.frame(
      age = c("0-4", "5+"),
      wrong = 2:1
    )
  )
  expect_error(
    check_alter(alter),
    "element 2 of 'alter' does not have a column called 'alter'"
  )
})

test_that("'check_alter' throws correct error when 'alter' column of element of 'alter' is non-numeric", {
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = 1:2
    ),
    external = data.frame(
      age = c("0-4", "5+"),
      alter = as.character(2:1)
    )
  )
  expect_error(
    check_alter(alter),
    "'alter' column of element 2 of 'alter' has class 'character'"
  )
})

test_that("'check_alter' throws correct error when 'alter' column of element of 'alter' has NAs", {
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = 1:2
    ),
    external = data.frame(
      age = c("0-4", "5+"),
      alter = c(NA, 1)
    )
  )
  expect_error(
    check_alter(alter),
    "'alter' column of element 2 of 'alter' has NAs"
  )
})

test_that("'check_alter' throws correct error when 'alter' column of element of 'alter' has negative values", {
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = 1:2
    ),
    external = data.frame(
      age = c("0-4", "5+"),
      alter = c(-1, 1)
    )
  )
  expect_error(
    check_alter(alter),
    "'alter' column of element 2 of 'alter' has negative values"
  )
})


## 'check_epsilon' ------------------------------------------------------------

test_that("'check_epsilon' returns TRUE when input valid", {
  expect_true(check_epsilon(0.001))
  expect_true(check_epsilon(0))
})

test_that("'check_epsilon' throws expected error when input not numeric", {
  expect_error(
    check_epsilon("0"),
    "'epsilon' has class 'character'"
  )
})

test_that("'check_epsilon' throws expected error when length wrong", {
  expect_error(
    check_epsilon(numeric()),
    "'epsilon' has length 0"
  )
  expect_error(
    check_epsilon(1:2),
    "'epsilon' has length 2"
  )
})

test_that("'check_epsilon' throws expected error when input is NA", {
  expect_error(
    check_epsilon(NA_real_),
    "'epsilon' is NA"
  )
})

test_that("'check_epsilon' throws expected error when input is negative", {
  expect_error(
    check_epsilon(-0.2),
    "'epsilon' is negative"
  )
})


## 'check_max_iter' -----------------------------------------------------------

test_that("'check_max_iter' returns TRUE when input valid", {
  expect_true(check_max_iter(1000L))
  expect_true(check_max_iter(1))
})

test_that("'check_max_iter' throws expected error when input not numeric", {
  expect_error(
    check_max_iter("0"),
    "'max_iter' has class 'character'"
  )
})

test_that("'check_max_iter' throws expected error when length wrong", {
  expect_error(
    check_max_iter(numeric()),
    "'max_iter' has length 0"
  )
  expect_error(
    check_max_iter(1:2),
    "'max_iter' has length 2"
  )
})

test_that("'check_max_iter' throws expected error when input is NA", {
  expect_error(
    check_max_iter(NA_real_),
    "'max_iter' is NA"
  )
})

test_that("'check_max_iter' throws expected error when input is less than 1", {
  expect_error(
    check_max_iter(-2),
    "'max_iter' is less than 1"
  )
})

test_that("'check_max_iter' throws expected error when input not an integer", {
  expect_error(
    check_max_iter(2.3),
    "'max_iter' is not a whole number"
  )
})


## 'check_reported' -----------------------------------------------------------

test_that("'check_reported' returns TRUE when 'reported' is valid", {
  reported <- data.frame(
    age = c("0-4", "5+"),
    internal = c(1, NA),
    external = c(2, 3)
  )
  expect_true(check_reported(reported))
})

test_that("'check_reported' throws correct error when 'reported' is not a data frame", {
  expect_error(
    check_reported(NULL),
    "'reported' has class 'NULL'"
  )
})

test_that("'check_reported' throws correct error when 'reported' has zero rows", {
  reported <- data.frame(age = character(), internal = numeric(), external = numeric())
  expect_error(
    check_reported(reported),
    "'reported' has zero rows"
  )
})


## 'check_reported_multi' -----------------------------------------------------------

test_that("'check_reported_multi' returns TRUE when 'reported' is valid", {
  reported <- data.frame(
    age = c("0-4", "5+"),
    region_orig = c(1, 2),
    region_dest = c(2, 1),
    count = c(1, 2)
  )
  expect_true(check_reported_multi(reported, name = "reported_int"))
})

test_that("'check_reported_multi' throws correct error when 'reported' is not a data frame", {
  expect_error(
    check_reported_multi(NULL, name = "reported_int"),
    "'reported_int' has class 'NULL'"
  )
})

test_that("'check_reported_multi' throws correct error when 'reported' has zero rows", {
  reported <- data.frame(
    age = character(), region_orig = numeric(),
    region_dest = numeric(), count = numeric()
  )
  expect_error(
    check_reported_multi(reported, name = "reported_im"),
    "'reported_im' has zero rows"
  )
})

test_that("'check_reported_multi' throws correct error when no 'region_orig' column", {
  reported <- data.frame(
    age = c("0-4", "5+"),
    wrong = c(1, 2),
    region_dest = c(2, 3),
    count = c(1, 2)
  )
  expect_error(
    check_reported_multi(reported, name = "reported_em"),
    "'reported_em' does not have a column called 'region_orig'"
  )
})

test_that("'check_reported_multi' throws correct error when no 'region_dest' column", {
  reported <- data.frame(
    age = c("0-4", "5+"),
    wrong = c(1, 2),
    region_orig = c(2, 3),
    count = c(1, 2)
  )
  expect_error(
    check_reported_multi(reported, name = "reported_em"),
    "'reported_em' does not have a column called 'region_dest'"
  )
})

test_that("'check_reported_multi' throws correct error when 'region_orig' column has NA", {
  reported <- data.frame(
    age = c("0-4", "5+"),
    region_orig = c(NA, 2),
    region_dest = c(2, 1),
    count = c(1, NA)
  )
  expect_error(
    check_reported_multi(reported, name = "reported_em"),
    "'region_orig' column in 'reported_em' has NAs"
  )
})

test_that("'check_reported_multi' throws correct error when 'region_dest' column has NA", {
  reported <- data.frame(
    age = c("0-4", "5+"),
    region_dest = c(NA, 2),
    region_orig = c(2, 1),
    count = c(1, 2)
  )
  expect_error(
    check_reported_multi(reported, name = "reported_em"),
    "'region_dest' column in 'reported_em' has NAs"
  )
})

test_that("'check_reported_multi' throws correct error for 'reported_int' when 'region_orig' and 'region_dest' equal", {
  reported <- data.frame(
    age = c("0-4", "5+", "5+"),
    region_dest = c(2, 2, 1),
    region_orig = c(2, 1, 2),
    count = c(1, 2, 3)
  )
  expect_error(
    check_reported_multi(reported, name = "reported_int"),
    "'region_orig' and 'region_dest' in row 1 of 'reported_int' both have value '2'"
  )
})

test_that("'check_reported_multi' throws correct error when no 'count' column", {
  reported <- data.frame(
    age = c("0-4", "5+"),
    region_orig = c(1, 2),
    region_dest = c(2, 3),
    wrong = c(1, 2)
  )
  expect_error(
    check_reported_multi(reported, name = "reported_em"),
    "'reported_em' does not have a column called 'count'"
  )
})

test_that("'check_reported_multi' throws correct error when classif vars have duplicates", {
  reported <- data.frame(
    age = c("0-4", "5+", "5+"),
    region_orig = c(1, 3, 3),
    region_dest = c(2, 3, 3),
    count = 1:3
  )
  expect_error(
    check_reported_multi(reported, name = "reported_em"),
    "'reported_em' has two rows with the following values : 5\\+, 3, 3"
  )
})

test_that("'check_reported_multi' throws correct error when 'count' has NAs", {
  reported <- data.frame(
    age = c("0-4", "5+"),
    region_orig = c(1, 2),
    region_dest = c(2, 3),
    count = c(1, NA)
  )
  expect_error(
    check_reported_multi(reported, name = "reported_em"),
    "'count' column in 'reported_em' has NAs"
  )
})

test_that("'check_reported_multi' throws correct error when 'count' has negative values", {
  reported <- data.frame(
    age = c("0-4", "5+"),
    region_orig = c(1, 2),
    region_dest = c(2, 3),
    count = c(1, -1)
  )
  expect_error(
    check_reported_multi(reported, name = "reported_em"),
    "'count' column in 'reported_em' has negative values"
  )
})


## 'check_tolerance' ----------------------------------------------------------

test_that("'check_tolerance' returns TRUE when input valid", {
  expect_true(check_tolerance(0.001))
  expect_true(check_tolerance(1L))
})

test_that("'check_tolerance' throws expected error when input not numeric", {
  expect_error(
    check_tolerance("0"),
    "'tolerance' has class 'character'"
  )
})

test_that("'check_tolerance' throws expected error when length wrong", {
  expect_error(
    check_tolerance(numeric()),
    "'tolerance' has length 0"
  )
  expect_error(
    check_tolerance(1:2),
    "'tolerance' has length 2"
  )
})

test_that("'check_tolerance' throws expected error when input is NA", {
  expect_error(
    check_tolerance(NA_real_),
    "'tolerance' is NA"
  )
})

test_that("'check_tolerance' throws expected error when input is non-positive", {
  expect_error(
    check_tolerance(0),
    "'tolerance' is non-positive"
  )
})


## 'check_totals' -------------------------------------------------------------

test_that("'check_totals' returns TRUE when 'totals' is valid", {
  totals <- data.frame(
    age = c("0-4", "5+"),
    count = c(1, NA)
  )
  expect_true(check_totals(totals))
})

test_that("'check_totals' throws correct error when 'totals' is not a data frame", {
  expect_error(
    check_totals(NULL),
    "'totals' has class 'NULL'"
  )
})

test_that("'check_totals' throws correct error when 'totals' has zero rows", {
  totals <- data.frame(age = character(), count = numeric())
  expect_error(
    check_totals(totals),
    "'totals' has zero rows"
  )
})

test_that("'check_totals' throws correct error when 'totals' has less than 2 columns", {
  totals <- data.frame(age = "a")
  expect_error(
    check_totals(totals),
    "'totals' has 1 column\\(s\\)"
  )
})

test_that("'check_totals' throws correct error when 'totals' does not have a column called 'count'", {
  totals <- data.frame(age = "0", wrong = 1)
  expect_error(
    check_totals(totals),
    "'totals' does not have a column called 'count'"
  )
})

test_that("'check_totals' throws correct error when classification variables have duplicates", {
  totals <- data.frame(age = c("0", "0"), sex = c("f", "f"), count = 1:2)
  expect_error(
    check_totals(totals),
    "'totals' has two rows with the following values : 0, f"
  )
})

test_that("'check_totals' throws correct error when numeric 'count' column has negative values", {
  totals <- data.frame(age = c("0", "1"), count = c(NA, -1))
  expect_error(
    check_totals(totals),
    "'count' column in 'totals' has negative values"
  )
})

test_that("'check_totals' throws correct error when elements of list 'count' column have different lengths", {
  totals <- data.frame(age = c("0", "1"))
  totals$count <- list(1:2, c(NA, 1, 3))
  expect_error(
    check_totals(totals),
    "elements of 'count' column in 'totals' have different lengths"
  )
})

test_that("'check_totals' throws correct error when list 'count' column has non-numeric values", {
  totals <- data.frame(age = c("0", "1"))
  totals$count <- list(1:2, c("a", 1))
  expect_error(
    check_totals(totals),
    "'count' column in 'totals' has non-numeric values"
  )
})

test_that("'check_totals' throws correct error when list 'count' column has negative values", {
  totals <- data.frame(age = c("0", "1"))
  totals$count <- list(1:2, c(NA, -1))
  expect_error(
    check_totals(totals),
    "'count' column in 'totals' has negative values"
  )
})

test_that("'check_totals' throws correct error when 'count' column is not numeric or list", {
  totals <- data.frame(age = c("0", "1"), count = c("1", "2"))
  expect_error(
    check_totals(totals),
    "'count' column in 'totals' has class 'character'"
  )
})


## 'check_totals_in_out' ------------------------------------------------------

test_that("'check_totals_in_out' returns TRUE when 'totals_in' and 'totals_out' valid", {
  totals_in <- data.frame(
    age = c("0-4", "5+"),
    region = c("a", "b"),
    count = c(1, 2)
  )
  totals_out <- data.frame(
    age = c("0-4", "5+"),
    region = c("a", "b"),
    count = c(2, 1)
  )
  expect_true(check_totals_in_out(
    totals_in = totals_in,
    totals_out = totals_out
  ))
})

test_that("'check_totals_in_out' throws correct error when 'totals_in' and 'totals_out' have different columns", {
  totals_in <- data.frame(
    age = c("0-4", "5+"),
    region = c("a", "b"),
    count = c(1, 2)
  )
  totals_out <- data.frame(
    sex = c("f", "f"),
    region = c("a", "b"),
    count = c(2, 1)
  )
  expect_error(
    check_totals_in_out(
      totals_in = totals_in,
      totals_out = totals_out
    ),
    "'totals_in' and 'totals_out' have different column names"
  )
})

test_that("'check_totals_in_out' throws correct error when 'totals_in' has value that 'totals_out' does not", {
  totals_in <- data.frame(
    age = c("0-4", "5+"),
    region = c("a", "b"),
    count = c(1, 2)
  )
  totals_out <- data.frame(
    age = c("0-4", "5+"),
    region = c("a", "a"),
    count = c(2, 1)
  )
  expect_error(
    check_totals_in_out(
      totals_in = totals_in,
      totals_out = totals_out
    ),
    "'region' column of 'totals_in' has value 'b' but 'region' column of 'totals_out' does not"
  )
})

test_that("'check_totals_in_out' throws correct error when 'totals_out' has value that 'totals_in' does not", {
  totals_in <- data.frame(
    age = c("0-4", "5+"),
    region = c("a", "a"),
    count = c(1, 2)
  )
  totals_out <- data.frame(
    age = c("0-4", "5+"),
    region = c("a", "b"),
    count = c(2, 1)
  )
  expect_error(
    check_totals_in_out(
      totals_in = totals_in,
      totals_out = totals_out
    ),
    "'region' column of 'totals_out' has value 'b' but 'region' column of 'totals_in' does not"
  )
})


## 'check_totals_in_reported' -------------------------------------------------

test_that("'check_totals_in_reported' returns TRUE when 'totals_in' and reported valid", {
  totals_in <- data.frame(
    age = c("0-4", "5+", "0-4"),
    region = c("a", "b", "c"),
    count = c(1, NA, 1)
  )
  reported_int <- data.frame(
    age = c("0-4", "0-4", "5+", "5+"),
    region_orig = c("a", "b", "a", "b"),
    region_dest = c("b", "a", "b", "a"),
    count = 1:4
  )
  expect_true(check_totals_in_reported(
    totals_in = totals_in,
    reported = reported_int,
    name = "reported_int"
  ))
})

test_that("'check_totals_in_reported' throws correct error when reported has column not in 'totals_in'", {
  totals_in <- data.frame(
    age = c("0-4", "5+"),
    region = c("a", "b"),
    count = c(1, NA)
  )
  reported_em <- data.frame(
    age = c("0-4", "0-4", "5+", "5+"),
    sex = c("f", "f", "f", "f"),
    region_orig = c("a", "b", "a", "b"),
    region_dest = c("b", "a", "b", "a"),
    count = 1:4
  )
  expect_error(
    check_totals_in_reported(
      totals_in = totals_in,
      reported = reported_em,
      name = "reported_em"
    ),
    "'reported_em' has column called 'sex' but 'totals_in' does not"
  )
})

test_that("'check_totals_in_reported_int' throws correct error when column in 'totals_in' has different values from same column in 'reported_im'", {
  totals_in <- data.frame(
    age = c("0-4", "5+"),
    region = c("a", "a"),
    count = c(1, NA)
  )
  reported_im <- data.frame(
    age = c("0-4", "0-4", "5+", "5+"),
    region_orig = c("A", "A", "A", "A"),
    region_dest = c("b", "a", "b", "a"),
    count = 1:4
  )
  expect_error(
    check_totals_in_reported(
      totals_in = totals_in,
      reported = reported_im,
      name = "reported_im"
    ),
    "'region_dest' column of 'reported_im' has value 'b' but 'region' column of 'totals_in' does not"
  )
})


## 'check_totals_multi' -------------------------------------------------------

test_that("'check_totals_multi' returns TRUE when 'totals' is valid", {
  totals <- data.frame(
    age = c("0-4", "5+"),
    region = c("a", "b"),
    count = c(1, 2)
  )
  expect_true(check_totals_multi(totals, name = "totals_in"))
})

test_that("'check_totals_multi' throws correct error when 'totals' is not a data frame", {
  expect_error(
    check_totals_multi(NULL, name = "totals_in"),
    "'totals_in' has class 'NULL'"
  )
})

test_that("'check_totals_multi' throws correct error when 'totals' has zero rows", {
  totals <- data.frame(age = character(), region = character(), count = numeric())
  expect_error(
    check_totals_multi(totals, name = "totals_in"),
    "'totals_in' has zero rows"
  )
})

test_that("'check_totals_multi' throws correct error when 'totals' does not have a column called 'region'", {
  totals <- data.frame(age = "0", wrong = 1)
  expect_error(
    check_totals_multi(totals, name = "totals_in"),
    "'totals_in' does not have a column called 'region'"
  )
})

test_that("'check_totals_multi' throws correct error when numeric 'region' column has NAs", {
  totals <- data.frame(age = c("0", "1"), region = c(NA, 1), count = 1:2)
  expect_error(
    check_totals_multi(totals, name = "totals_out"),
    "'region' column in 'totals_out' has NAs"
  )
})

test_that("'check_totals_multi' throws correct error when 'totals' does not have a column called 'count'", {
  totals <- data.frame(age = "0", region = "a", wrong = 1)
  expect_error(
    check_totals_multi(totals, name = "totals_out"),
    "'totals_out' does not have a column called 'count'"
  )
})

test_that("'check_totals_multi' throws correct error when classif vars have duplicates", {
  totals <- data.frame(age = c("0", "0"), region = c("a", "a"), count = 1:2)
  expect_error(
    check_totals_multi(totals, name = "totals_out"),
    "'totals_out' has two rows with the following values : 0, a"
  )
})

test_that("'check_totals_multi' throws correct error when numeric 'count' column has NAs", {
  totals <- data.frame(age = c("0", "1"), region = c("a", "a"), count = c(2, NA))
  expect_error(
    check_totals_multi(totals, name = "totals_out"),
    "'count' column in 'totals_out' has NAs"
  )
})

test_that("'check_totals_multi' throws correct error when numeric 'count' column has negative values", {
  totals <- data.frame(age = c("0", "1"), region = c("a", "a"), count = c(2, -1))
  expect_error(
    check_totals_multi(totals, name = "totals_out"),
    "'count' column in 'totals_out' has negative values"
  )
})

test_that("'check_totals_multi' throws correct error when elements of list 'count' column have different lengths", {
  totals <- data.frame(age = c("0", "1"), region = c("a", "a"))
  totals$count <- list(1:2, c(2, 1, 3))
  expect_error(
    check_totals_multi(totals, name = "totals_out"),
    "elements of 'count' column in 'totals_out' have different lengths"
  )
})

test_that("'check_totals_multi' throws correct error when list 'count' column has non-numeric values", {
  totals <- data.frame(age = c("0", "1"), region = c("a", "a"))
  totals$count <- list(1:2, c("a", 1))
  expect_error(
    check_totals_multi(totals, name = "totals_out"),
    "'count' column in 'totals_out' has non-numeric values"
  )
})

test_that("'check_totals_multi' throws correct error when list 'count' column has NAs", {
  totals <- data.frame(age = c("0", "1"), region = c("a", "a"))
  totals$count <- list(1:2, c(2, NA))
  expect_error(
    check_totals_multi(totals, name = "totals_out"),
    "'count' column in 'totals_out' has NAs"
  )
})

test_that("'check_totals_multi' throws correct error when list 'count' column has negative values", {
  totals <- data.frame(age = c("0", "1"), region = c("a", "a"))
  totals$count <- list(1:2, c(2, -1))
  expect_error(
    check_totals_multi(totals, name = "totals_out"),
    "'count' column in 'totals_out' has negative values"
  )
})

test_that("'check_totals_multi' throws correct error when 'count' column is not numeric or list", {
  totals <- data.frame(age = c("0", "1"), region = c("a", "a"), count = c("1", "2"))
  expect_error(
    check_totals_multi(totals, name = "totals_out"),
    "'count' column in 'totals_out' has class 'character'"
  )
})


## 'check_totals_reported' ----------------------------------------------------

test_that("'check_totals_reported' returns TRUE when 'totals' and 'reported' valid", {
  totals <- data.frame(
    age = c("0-4", "5+"),
    count = c(1, NA)
  )
  reported <- data.frame(
    age = c("0-4", "5+"),
    internal = c(1, NA),
    external = 2:1
  )
  zeros_to_ones <- FALSE
  expect_true(check_totals_reported(
    totals = totals,
    reported = reported,
    zeros_to_ones = zeros_to_ones
  ))
})

test_that("'check_totals_reported' throws correct error when classif variable in 'reported' missing levels", {
  totals <- data.frame(
    age = c("0-4", "5-9", "10+"),
    count = c(1, NA, 2)
  )
  reported <- data.frame(
    age = c("0-4", "5-9"),
    internal = c(1, NA),
    external = 2:1
  )
  zeros_to_ones <- FALSE
  expect_error(
    check_totals_reported(
      totals = totals,
      reported = reported,
      zeros_to_ones = zeros_to_ones
    ),
    "column 'age' in 'totals' has value '10\\+', but column 'age' in 'reported' does not"
  )
})

test_that("'check_totals_reported' throws correct error when count variable in 'reported' non-numeric", {
  totals <- data.frame(
    age = c("0-4", "5-9"),
    count = c(1, NA)
  )
  reported <- data.frame(
    age = c("0-4", "5-9"),
    internal = c(1, "2"),
    external = 2:1
  )
  zeros_to_ones <- FALSE
  expect_error(
    check_totals_reported(
      totals = totals,
      reported = reported,
      zeros_to_ones = zeros_to_ones
    ),
    "column 'internal' in 'reported' is non-numeric"
  )
})

test_that("'check_totals_reported' throws correct error when count variable in 'reported' has negative value", {
  totals <- data.frame(
    age = c("0-4", "5-9"),
    count = c(1, NA)
  )
  reported <- data.frame(
    age = c("0-4", "5-9"),
    internal = c(NA, -3),
    external = 2:1
  )
  zeros_to_ones <- FALSE
  expect_error(
    check_totals_reported(
      totals = totals,
      reported = reported,
      zeros_to_ones = zeros_to_ones
    ),
    "column 'internal' in 'reported' has negative value"
  )
})

test_that("'check_totals_reported' returns TRUE when reported all zero but total also all zero or NA (zeros_to_ones is FALSE)", {
  totals_zero_num <- data.frame(
    age = c("0-4", "5-9"),
    count = c(1, 0)
  )
  totals_na_num <- data.frame(
    age = c("0-4", "5-9"),
    count = c(1, NA)
  )
  totals_na_zero_list <- data.frame(age = c("0-4", "5-9"))
  totals_na_zero_list$count <- list(c(1, 0, 3), c(0, 0, NA))
  reported <- data.frame(
    age = c("0-4", "5-9"),
    internal = c(1, 0),
    external = c(2, 0)
  )
  zeros_to_ones <- FALSE
  expect_true(check_totals_reported(
    totals = totals_zero_num,
    reported = reported,
    zeros_to_ones = zeros_to_ones
  ))
  expect_true(check_totals_reported(
    totals = totals_na_num,
    reported = reported,
    zeros_to_ones = zeros_to_ones
  ))
  expect_true(check_totals_reported(
    totals = totals_na_zero_list,
    reported = reported,
    zeros_to_ones = zeros_to_ones
  ))
})

test_that("'check_totals_reported' returns TRUE when reported all zero and NA, even when total non-zero (zeros_to_ones is FALSE)", {
  totals <- data.frame(
    age = c("0-4", "5-9"),
    count = c(1, 2)
  )
  reported <- data.frame(
    age = c("0-4", "5-9"),
    internal = c(1, 0),
    external = c(2, NA)
  )
  zeros_to_ones <- FALSE
  expect_true(check_totals_reported(
    totals = totals,
    reported = reported,
    zeros_to_ones = zeros_to_ones
  ))
})

test_that("'check_totals_reported' throws correct error when reported all zero, and total non-zero (zeros_to_ones is FALSE)", {
  totals <- data.frame(
    age = c("0-4", "5-9"),
    count = c(1, 2)
  )
  reported_one <- data.frame(
    age = c("0-4", "5-9"),
    internal = c(1, 0),
    external = c(2, 0)
  )
  reported_two <- data.frame(
    age = c("0-4", "5-9"),
    internal = c(0, 0),
    external = c(0, 0)
  )
  zeros_to_ones <- FALSE
  expect_error(
    check_totals_reported(
      totals = totals,
      reported = reported_one,
      zeros_to_ones = zeros_to_ones
    ),
    paste(
      "1 case where values in 'reported' are all zero",
      "but corresponding value in 'totals' is non-zero :",
      "consider setting 'zeros_to_ones' to TRUE"
    )
  )
  expect_error(
    check_totals_reported(
      totals = totals,
      reported = reported_two,
      zeros_to_ones = zeros_to_ones
    ),
    paste(
      "2 cases where values in 'reported' are all zero",
      "but corresponding value in 'totals' is non-zero :",
      "consider setting 'zeros_to_ones' to TRUE"
    )
  )
})

test_that("'check_totals_reported' returns TRUE when reported all zero, and total non-zero (zeros_to_ones is TRUE)", {
  totals <- data.frame(
    age = c("0-4", "5-9"),
    count = c(1, 2)
  )
  reported <- data.frame(
    age = c("0-4", "5-9"),
    internal = c(1, 0),
    external = c(2, 0)
  )
  zeros_to_ones <- TRUE
  expect_true(check_totals_reported(
    totals = totals,
    reported = reported,
    zeros_to_ones = zeros_to_ones
  ))
})


## 'check_totals_reported_alter' ----------------------------------------------

test_that("'check_totals_reported_alter' returns TRUE when 'totals', 'reported', and 'alter' valid", {
  totals <- data.frame(
    age = c("0-4", "5+"),
    count = c(1, NA)
  )
  reported <- data.frame(
    age = c("0-4", "5+"),
    internal = c(1, NA),
    external = 2:1
  )
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = c(1, 0.2)
    ),
    external = data.frame(
      age = c("0-4", "5+"),
      alter = c(1, 3)
    )
  )
  expect_true(check_totals_reported_alter(
    totals = totals,
    reported = reported,
    alter = alter
  ))
})

test_that("'check_totals_reported_alter' throws correct error when name from 'alter' not found in columns of 'reported'", {
  totals <- data.frame(
    age = c("0-4", "5+"),
    count = c(1, NA)
  )
  reported <- data.frame(
    age = c("0-4", "5+"),
    internal = c(1, NA),
    external = 2:1
  )
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5+"),
      alter = c(1, 0.2)
    ),
    wrong = data.frame(
      age = c("0-4", "5+"),
      alter = c(1, 3)
    )
  )
  expect_error(
    check_totals_reported_alter(
      totals = totals,
      reported = reported,
      alter = alter
    ),
    "'alter' has an element called 'wrong', but 'reported' does not have a column called 'wrong'"
  )
})

test_that("'check_totals_reported_alter' throws correct error when column from 'alter' not found in columns of 'totals'", {
  totals <- data.frame(
    age = c("0-4", "5-9"),
    count = c(1, NA)
  )
  reported <- data.frame(
    age = c("0-4", "5-9"),
    internal = c(1, NA),
    external = 2:1
  )
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5-9"),
      alter = c(1, 0.2)
    ),
    external = data.frame(
      age = c("0-4", "5-9"),
      sex = c("F", "M"),
      alter = c(1, 3)
    )
  )
  expect_error(
    check_totals_reported_alter(
      totals = totals,
      reported = reported,
      alter = alter
    ),
    "element 'external' of 'alter' has a column called 'sex', but 'totals' does not have a column called 'sex'"
  )
})

test_that("'check_totals_reported_alter' throws correct error when value from 'totals' not found in 'alter'", {
  totals <- data.frame(
    age = c("0-4", "5-9", "10+"),
    count = c(1, NA, 2)
  )
  reported <- data.frame(
    age = c("0-4", "5-9", "10+"),
    internal = c(1, NA, 3),
    external = 2:0
  )
  alter <- list(
    internal = data.frame(
      age = c("0-4", "5-9"),
      alter = c(1, 0.2)
    ),
    external = data.frame(
      age = c("0-4", "5-9"),
      alter = c(1, 3)
    )
  )
  expect_error(
    check_totals_reported_alter(
      totals = totals,
      reported = reported,
      alter = alter
    ),
    paste(
      "column 'age' in 'totals' has value '10\\+', but column 'age' in element",
      "'internal' in 'alter' does not have value '10\\+'"
    )
  )
})


## 'check_zeros_to_ones' ------------------------------------------------------

test_that("'check_zeros_to_ones' returns TRUE when input valid", {
  expect_true(check_zeros_to_ones(TRUE))
  expect_true(check_zeros_to_ones(FALSE))
})

test_that("'check_zeros_to_ones' throws expected error when input not logical", {
  expect_error(
    check_zeros_to_ones("TRUE"),
    "'zeros_to_ones' has class 'character'"
  )
})

test_that("'check_zeros_to_ones' throws expected error when length wrong", {
  expect_error(
    check_zeros_to_ones(logical()),
    "'zeros_to_ones' has length 0"
  )
  expect_error(
    check_zeros_to_ones(c(TRUE, FALSE)),
    "'zeros_to_ones' has length 2"
  )
})

test_that("'check_zeros_to_ones' throws expected error when input is NA", {
  expect_error(
    check_zeros_to_ones(NA),
    "'zeros_to_ones' is NA"
  )
})
