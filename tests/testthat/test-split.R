## 'split_single' -------------------------------------------------------------

test_that("'split_single' gives expected answer with valid inputs - 'counts' is numeric, 'alter' is NULL ", {
  totals <- data.frame(
    age = rep(0:2, times = 2),
    sex = rep(c("F", "M"), each = 3),
    count = 11:16
  )
  reported <- data.frame(
    sex = c("F", "M"),
    f1 = 1:2,
    f2 = 2:1,
    f3 = 3:4
  )
  ans_obtained <- split_single(
    totals = totals,
    reported = reported
  )
  ans_expected <- data.frame(
    age = rep(0:2, times = 2),
    sex = rep(c("F", "M"), each = 3),
    f1 = rep(c(1 / 6, 2 / 7), each = 3) * totals$count,
    f2 = rep(c(2 / 6, 1 / 7), each = 3) * totals$count,
    f3 = rep(c(3 / 6, 4 / 7), each = 3) * totals$count
  )
  expect_equal(ans_obtained, ans_expected)
})

test_that("'split_single' gives expected answer with valid inputs - 'counts' is list, 'alter' is NULL ", {
  totals <- data.frame(
    age = rep(0:2, times = 2),
    sex = rep(c("F", "M"), each = 3)
  )
  totals$count <- list(11:12, 13:14, 15:16, 17:18, 19:20, 21:22)
  reported <- data.frame(
    sex = c("F", "M"),
    f1 = 1:2,
    f2 = 2:1,
    f3 = 3:4
  )
  ans_obtained <- split_single(
    totals = totals,
    reported = reported
  )
  ans_expected <- data.frame(
    age = rep(0:2, times = 2),
    sex = rep(c("F", "M"), each = 3)
  )
  ans_expected$f1 <- list(
    (1 / 6) * totals$count[[1]],
    (1 / 6) * totals$count[[2]],
    (1 / 6) * totals$count[[3]],
    (2 / 7) * totals$count[[4]],
    (2 / 7) * totals$count[[5]],
    (2 / 7) * totals$count[[6]]
  )
  ans_expected$f2 <- list(
    (2 / 6) * totals$count[[1]],
    (2 / 6) * totals$count[[2]],
    (2 / 6) * totals$count[[3]],
    (1 / 7) * totals$count[[4]],
    (1 / 7) * totals$count[[5]],
    (1 / 7) * totals$count[[6]]
  )
  ans_expected$f3 <- list(
    (3 / 6) * totals$count[[1]],
    (3 / 6) * totals$count[[2]],
    (3 / 6) * totals$count[[3]],
    (4 / 7) * totals$count[[4]],
    (4 / 7) * totals$count[[5]],
    (4 / 7) * totals$count[[6]]
  )
  expect_equal(ans_obtained, ans_expected)
})

test_that("'split_single' gives expected answer with valid inputs - 'counts' is numeric, 'alter' has one element ", {
  totals <- data.frame(
    age = rep(0:2, times = 2),
    sex = rep(c("F", "M"), each = 3),
    count = 11:16
  )
  reported <- data.frame(
    sex = c("F", "M"),
    f1 = 1:2,
    f2 = 2:1,
    f3 = 3:4
  )
  alter <- list(f1 = data.frame(sex = c("F", "M"), alter = 1:2))
  ans_obtained <- split_single(
    totals = totals,
    reported = reported,
    alter = alter
  )
  ans_expected <- data.frame(
    age = rep(0:2, times = 2),
    sex = rep(c("F", "M"), each = 3),
    f1 = rep(1:2, each = 3) + rep(c(1 / 6, 4 / 9), each = 3) * (totals$count - rep(6:7, each = 3)),
    f2 = rep(2:1, each = 3) + rep(c(2 / 6, 1 / 9), each = 3) * (totals$count - rep(6:7, each = 3)),
    f3 = rep(3:4, each = 3) + rep(c(3 / 6, 4 / 9), each = 3) * (totals$count - rep(6:7, each = 3))
  )
  expect_equal(ans_obtained, ans_expected)
})

test_that("'split_single' propogates NA in 'totals' in expected way", {
  totals <- data.frame(
    age = rep(0:2, times = 2),
    sex = rep(c("F", "M"), each = 3),
    count = c(11:15, NA)
  )
  reported <- data.frame(
    sex = c("F", "M"),
    f1 = 1:2,
    f2 = 2:1,
    f3 = 3:4
  )
  ans_obtained <- split_single(
    totals = totals,
    reported = reported
  )
  ans_expected <- data.frame(
    age = rep(0:2, times = 2),
    sex = rep(c("F", "M"), each = 3),
    f1 = rep(c(1 / 6, 2 / 7), each = 3) * totals$count,
    f2 = rep(c(2 / 6, 1 / 7), each = 3) * totals$count,
    f3 = rep(c(3 / 6, 4 / 7), each = 3) * totals$count
  )
  ans_expected[6, 3:5] <- NA
  expect_equal(ans_obtained, ans_expected)
})

test_that("'split_single' propogates NA in 'counts' in expected way", {
  totals <- data.frame(
    age = rep(0:2, times = 2),
    sex = rep(c("F", "M"), each = 3),
    count = 11:16
  )
  reported <- data.frame(
    sex = c("F", "M"),
    f1 = 1:2,
    f2 = 2:1,
    f3 = c(3, NA)
  )
  ans_obtained <- split_single(
    totals = totals,
    reported = reported
  )
  ans_expected <- data.frame(
    age = rep(0:2, times = 2),
    sex = rep(c("F", "M"), each = 3),
    f1 = rep(c(1 / 6, NA), each = 3) * totals$count,
    f2 = rep(c(2 / 6, NA), each = 3) * totals$count,
    f3 = rep(c(3 / 6, NA), each = 3) * totals$count
  )
  expect_equal(ans_obtained, ans_expected)
})


## 'split_multi' --------------------------------------------------------------

test_that("'split_multi' works with valid inputs - counts columns for 'totals' are numeric", {
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
  ans_obtained <- split_multi(
    totals_in = totals_in,
    totals_out = totals_out,
    reported_int = reported_int,
    reported_im = reported_im,
    reported_em = reported_em
  )
  ## immigration same
  im_obtained <- (xtabs(count ~ region_dest + age, ans_obtained$internal)
  + xtabs(count ~ region_dest + age, ans_obtained$immigration))
  im_obtained <- array(im_obtained, dim = dim(im_obtained), dimnames = dimnames(im_obtained))
  names(dimnames(im_obtained))[[1L]] <- "region"
  im_expected <- xtabs(count ~ region + age, totals_in)
  im_expected <- array(im_expected, dim = dim(im_expected), dimnames = dimnames(im_expected))
  expect_equal(im_obtained, im_expected)
  ## emigration same
  em_obtained <- (xtabs(count ~ region_orig + age, ans_obtained$internal)
  + xtabs(count ~ region_orig + age, ans_obtained$emigration))
  em_obtained <- array(em_obtained, dim = dim(em_obtained), dimnames = dimnames(em_obtained))
  names(dimnames(em_obtained))[[1L]] <- "region"
  em_expected <- xtabs(count ~ region + age, totals_out)
  em_expected <- array(em_expected, dim = dim(em_expected), dimnames = dimnames(em_expected))
  expect_equal(em_obtained, em_expected, tolerance = 1e-6)
})

test_that("'split_multi' works with valid inputs - counts columns for 'totals' are lists", {
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
  ans_obtained <- split_multi(
    totals_in = totals_in,
    totals_out = totals_out,
    reported_int = reported_int,
    reported_im = reported_im,
    reported_em = reported_em
  )
  for (i in 1:2) {
    internal_tmp <- ans_obtained$internal
    immigration_tmp <- ans_obtained$immigration
    emigration_tmp <- ans_obtained$emigration
    totals_in_tmp <- totals_in
    totals_out_tmp <- totals_out
    internal_tmp$count <- sapply(internal_tmp$count, function(x) x[i])
    immigration_tmp$count <- sapply(immigration_tmp$count, function(x) x[i])
    emigration_tmp$count <- sapply(emigration_tmp$count, function(x) x[i])
    totals_in_tmp$count <- sapply(totals_in$count, function(x) x[i])
    totals_out_tmp$count <- sapply(totals_out$count, function(x) x[i])
    ## immigration same
    im_obtained <- (xtabs(count ~ region_dest + age, internal_tmp)
    + xtabs(count ~ region_dest + age, immigration_tmp))
    im_obtained <- array(im_obtained, dim = dim(im_obtained), dimnames = dimnames(im_obtained))
    names(dimnames(im_obtained))[[1L]] <- "region"
    im_expected <- xtabs(count ~ region + age, totals_in_tmp)
    im_expected <- array(im_expected, dim = dim(im_expected), dimnames = dimnames(im_expected))
    expect_equal(im_obtained, im_expected, tolerance = 1e-5)
    ## emigration same
    em_obtained <- (xtabs(count ~ region_orig + age, internal_tmp)
    + xtabs(count ~ region_orig + age, emigration_tmp))
    em_obtained <- array(em_obtained, dim = dim(em_obtained), dimnames = dimnames(em_obtained))
    names(dimnames(em_obtained))[[1L]] <- "region"
    em_expected <- xtabs(count ~ region + age, totals_out_tmp)
    em_expected <- array(em_expected, dim = dim(em_expected), dimnames = dimnames(em_expected))
    expect_equal(em_obtained, em_expected, tolerance = 1e-5)
  }
})

test_that("'split_multi' treats missing combinations of categories in 'reported*' data frames as zeros", {
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
  reported_int_complete <- data.frame(
    age = 0:1,
    region_orig = rep(c("b", "a"), each = 2),
    region_dest = rep(c("a", "b"), each = 2),
    count = 0:3
  )
  reported_int_incomplete <- data.frame(
    age = c(1L, 0L, 1L),
    region_orig = c("b", "a", "a"),
    region_dest = c("a", "b", "b"),
    count = 1:3
  )
  reported_im_complete <- data.frame(
    age = 0:1,
    region_orig = "A",
    region_dest = rep(c("a", "b"), each = 2),
    count = c(0L, 0L, 3:4)
  )
  reported_im_incomplete <- data.frame(
    age = 0:1,
    region_orig = "A",
    region_dest = c("b", "b"),
    count = 3:4
  )
  reported_em <- data.frame(
    age = 0:1,
    region_orig = rep(c("a", "b"), each = 2),
    region_dest = "A",
    count = 4:1
  )
  ans_obtained_complete <- split_multi(
    totals_in = totals_in,
    totals_out = totals_out,
    reported_int = reported_int_complete,
    reported_im = reported_im_complete,
    reported_em = reported_em
  )
  ans_obtained_incomplete <- split_multi(
    totals_in = totals_in,
    totals_out = totals_out,
    reported_int = reported_int_incomplete,
    reported_im = reported_im_incomplete,
    reported_em = reported_em
  )
  expect_equal(ans_obtained_complete, ans_obtained_incomplete)
})
