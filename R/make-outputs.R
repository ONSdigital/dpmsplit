## HAS_TESTS
#' Combine matrix of estimated migration flows with original
#' classification variables
#'
#' Combine output from 'split-single-inner' with
#' classification variables, creating list columns where
#' necessary.
#'
#' @param classif_vars Data frame
#' @param vals_split Matrix with one column for each migration flow.
#'
#' @return A data frame
#'
#' @noRd
make_ans <- function(classif_vars, vals_split) {
  n_iter <- nrow(vals_split) / nrow(classif_vars)
  is_list_col <- n_iter > 1L
  if (is_list_col) {
    ## creating a data frame with list columns
    ## is a bit awkward
    ans <- classif_vars
    n_flow <- ncol(vals_split)
    nms_flow <- colnames(vals_split)
    for (i in seq_len(n_flow)) {
      nm <- nms_flow[[i]]
      val <- vals_split[, i]
      ans[[nm]] <- to_list_col(x = val, n_iter = n_iter)
    }
  } else {
    ans <- data.frame(classif_vars, vals_split)
  }
  ans
}


## HAS_TESTS
#' Extract classification variables from 'totals'
#'
#' Extract classification variables from 'totals'
#' by assuming that the only non-classification
#' column is called 'count'.
#'
#' @param totals A data frame
#'
#' @return A data frame.
#'
#' @noRd
make_classif_vars <- function(totals) {
  i_count <- match("count", names(totals))
  totals[-i_count]
}


## HAS_TESTS
#' Turn output from IPF into three data frames
#'
#' Turn output from IPF, plus data frame with classification
#' variables, into a list of data frames.
#'
#' @param vals List. Output from calling IPF on values
#' from each combination of classification variables
#' @param classif_vars Data frame with classification
#' variables, excluding region variables
#' @param n_external Number of external regions.
#'
#' @return A named list with data frames
#' "internal", "immigration", and "emigration"
#'
#' @noRd
make_outputs_multi <- function(vals, classif_vars, n_external) {
  dim_vals <- dim(vals[[1L]])
  has_iter <- length(dim_vals) == 3L
  n_combined <- dim_vals[[1L]]
  n_internal <- n_combined - n_external
  s_internal <- seq_len(n_internal)
  s_external <- seq.int(from = n_internal + 1L, to = n_combined)
  if (has_iter) {
    internal <- lapply(vals, function(x) x[s_internal, s_internal, , drop = FALSE])
    immigration <- lapply(vals, function(x) x[s_external, s_internal, , drop = FALSE])
    emigration <- lapply(vals, function(x) x[s_internal, s_external, , drop = FALSE])
    internal <- lapply(internal, array3d_to_listdf)
    immigration <- lapply(immigration, array3d_to_listdf)
    emigration <- lapply(emigration, array3d_to_listdf)
  } else {
    internal <- lapply(vals, function(x) x[s_internal, s_internal, drop = FALSE])
    immigration <- lapply(vals, function(x) x[s_external, s_internal, drop = FALSE])
    emigration <- lapply(vals, function(x) x[s_internal, s_external, drop = FALSE])
    internal <- lapply(internal, as.data.frame.table, responseName = "count")
    immigration <- lapply(immigration, as.data.frame.table, responseName = "count")
    emigration <- lapply(emigration, as.data.frame.table, responseName = "count")
  }
  idx_row_internal <- rep(seq_along(internal), times = vapply(internal, nrow, 0L))
  idx_row_immigration <- rep(seq_along(immigration), times = vapply(immigration, nrow, 0L))
  idx_row_emigration <- rep(seq_along(emigration), times = vapply(emigration, nrow, 0L))
  internal <- cbind(
    classif_vars[idx_row_internal, , drop = FALSE],
    do.call(rbind, internal)
  )
  immigration <- cbind(
    classif_vars[idx_row_immigration, , drop = FALSE],
    do.call(rbind, immigration)
  )
  emigration <- cbind(
    classif_vars[idx_row_emigration, , drop = FALSE],
    do.call(rbind, emigration)
  )
  rownames(internal) <- NULL
  rownames(immigration) <- NULL
  rownames(emigration) <- NULL
  list(
    internal = internal,
    immigration = immigration,
    emigration = emigration
  )
}
