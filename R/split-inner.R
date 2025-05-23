## HAS_TESTS
#' Split flows for a single population
#'
#' Function to do the calculations for
#' splitting flows for a single population.
#' The algorithm avoids negative values for
#' the split flows. The \code{vals_add} argument
#' is needed to implement this algorithm.
#'
#' @param val_total Counts of inflows or outflows that need to
#' be split. Vector of length n.
#' @param vals_reported Reported flows, disaggregated
#' by type. Matrix with dimensions n x b.
#' @param vals_alter Values for alterability parameter.
#' Matrix with dimensions n x b.
#' @param vals_add Values to be added to the answer
#' before returning it. Matrix with dimensions n x b.
#'
#' @return Disaggregated migration flows. Matrix with
#' dimensions n x b.
#'
#' @noRd
split_single_inner <- function(val_total,
                               vals_reported,
                               vals_alter,
                               vals_add) {
  total_reported <- rowSums(vals_reported)
  resid <- val_total - total_reported
  weights <- vals_reported * vals_alter
  weights <- proportions(weights, margin = 1L)
  ans <- vals_reported + weights * resid
  ans[val_total == 0, ] <- 0
  is_neg <- ans < 0
  if (any(is_neg, na.rm = TRUE)) {
    vals_rep_neg <- vals_reported * is_neg
    val_total <- val_total - rowSums(vals_rep_neg)
    vals_add <- vals_add + vals_rep_neg
    vals_reported[is_neg] <- 0
    Recall(
      val_total = val_total,
      vals_reported = vals_reported,
      vals_alter = vals_alter,
      vals_add = vals_add
    )
  } else {
    ans <- ans + vals_add
    ans
  }
}


## HAS_TESTS
#' Split flows for multiple calculations
#'
#' Split out internal and external migration
#' flows using IPF.
#'
#' @param m A list of matrices giving reported values
#' @param colsums A list of vectors or (if there
#' are multiple iterations) matrices giving control
#' totals for columns
#' @param rowsums A list of vectors or (if there
#' are multiple iterations) matrices giving control
#' totals for rows
#'
#' @return List of matrices.
#'
#' @noRd
split_multi_inner <- function(m,
                              colsums,
                              rowsums,
                              max_iter,
                              tolerance) {
  has_iter <- is.matrix(rowsums[[1L]])
  fun <- if (has_iter) ipf_iter else ipf
  mapply(fun,
    m = m,
    rowsums = rowsums,
    colsums = colsums,
    MoreArgs = list(
      max_iter = max_iter,
      tolerance = tolerance
    ),
    SIMPLIFY = FALSE
  )
}
