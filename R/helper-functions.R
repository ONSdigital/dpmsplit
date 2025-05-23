## HAS_TESTS
#' Turn a 3D array into a list data frame
#'
#' Turn a 3-dimensional array with iterations
#' as the third dimension into a three-column
#' data frame, where the third column is a
#' list of vectors.
#'
#' @param x A 3D array
#'
#' @return A data frame.
#'
#' @noRd
array3d_to_listdf <- function(x) {
  ans <- expand.grid(dimnames(x)[1:2], KEEP.OUT.ATTRS = FALSE)
  count <- matrix(x, ncol = dim(x)[[3L]])
  ans$count <- apply(count, 1L, function(y) y, simplify = FALSE)
  ans
}


## HAS_TESTS
#' Iterative proportional fitting - multiple rowsums and colsums
#'
#' Apply function 'ipf' multiple times, using same 'm' but
#' different rowsums and colsums - which is the situation
#' when 'm' comes from a single dataset, and the
#' rowsums and colsums are multiple draws from
#' a posterior distribution.
#'
#' @param m A matrix
#' @param rowsums A matrix, with each column a different set
#' of totals
#' @param colsums A matrix, with each column a different set
#' of totals
#' @param max_iter Maximum number of iterations
#' @param tolerance Largest permitted absolute difference
#' between actual and derived rowsums/colsums.
#'
#' @return An array
#'
#' @noRd
ipf_iter <- function(m, rowsums, colsums, max_iter, tolerance) {
  n_iter <- ncol(rowsums)
  ans <- array(
    dim = c(dim(m), n_iter),
    dimnames = c(dimnames(m), list(iter = NULL))
  )
  for (i_iter in seq_len(n_iter)) {
    ans[, , i_iter] <- ipf(
      m = m,
      rowsums = rowsums[, i_iter],
      colsums = colsums[, i_iter],
      max_iter = max_iter,
      tolerance = tolerance
    )
  }
  ans
}


## HAS_TESTS
#' Infer the names of the counts columns in 'reported'
#'
#' Given the 'totals' and 'reported' data frames,
#' infer the names of the counts columns
#' in 'reported', by subtracting off the
#' names of the classification variables.
#'
#' @param totals A data frame
#' @param reported A data frame
#'
#' @return A character vector.
#'
#' @noRd
nms_counts_reported <- function(totals, reported) {
  nms_totals <- names(totals)
  nms_reported <- names(reported)
  nms_classif_totals <- setdiff(nms_totals, "count")
  setdiff(nms_reported, nms_classif_totals)
}


## HAS_TESTS
#' Simple left join
#'
#' A simple left join that
#' - assumes 'x' and 'y' are data frames
#' - joins on common columns in 'x' and 'y'
#' - assumes that every row in 'x' has a corresonding row in 'y'
#' - is fast
#' - gives results that match the row order of 'x'
#'
#' @param x A data frame
#' @param y A data frame
#'
#' @return A data frame with the same number of rows as 'x'
#'
#' @noRd
simple_left_join <- function(x, y) {
  nms_x <- names(x)
  nms_y <- names(y)
  nms_xy <- intersect(nms_x, nms_y)
  j_x <- match(nms_xy, nms_x)
  j_y <- match(nms_xy, nms_y)
  s_y <- seq_along(y)
  c_j_y <- setdiff(s_y, j_y)
  id_x <- do.call(paste, x[j_x])
  id_y <- do.call(paste, y[j_y])
  i <- match(id_x, id_y)
  ans_y <- y[i, c_j_y, drop = FALSE]
  ans <- data.frame(x, ans_y)
  attr(ans, "row.names") <- attr(x, "row.names") ## 'rownames' function does not work!
  ans
}


## HAS_TESTS
#' Create a list column
#'
#' Given a vector of values 'x', and a number of
#' iterations 'n_iter', create a list column in which
#' each element is a vector of length 'n_iter'.
#'
#' @param x A vector
#' @param n_iter An integer scalar
#'
#' @return A list of length length(x) / n_iter.
#'
#' @noRd
to_list_col <- function(x, n_iter) {
  m <- matrix(x, ncol = n_iter, byrow = TRUE)
  apply(m, 1L, "[", simplify = FALSE)
}

## HAS_TESTS
## Sort the rows of a data frame, with the left-most columns
## varying slowest and right-most fastest.
sort_df <- function(x, ignore = NULL) {
  if (is.null(ignore)) {
    args <- x
  } else {
    i_ignore <- match(ignore, names(x), nomatch = 0L)
    i_invalid <- match(0L, i_ignore, nomatch = 0L)
    if (i_invalid > 0L) {
      stop("\"", ignore[[i_invalid]], "\" is not a valid column name")
    }
    args <- x[-i_ignore]
  }
  ord <- do.call(order, args = args)
  ans <- x[ord, ]
  rownames(ans) <- NULL
  ans
}
