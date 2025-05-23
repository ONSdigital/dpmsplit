## HAS_TESTS
#' Reformat a 'reported' data frame to have a list column
#' with tabulations of counts by region_orig by region_dest
#'
#' Reformat a 'reported' data frame so that it
#' holds regional orig-dest crosstabs,
#' for each combination
#' of the classifying variables
#'
#' Does not assume that every region
#' is present for every combination of
#' classifying variables.
#'
#' Note that 'levels_orig' and 'levels_dest' will typically
#' differ if 'reported' is 'reported_im' or 'reported_em'.
#'
#' @param reported A data frame
#' @param levels_reg Character vector with complete
#' set of regions
#' @param name Name of data frame - "reported_int",
#' "reported_im", or "reported_em".
#'
#' @return A data frame with a list column
#' holding orig-dest matrices.
#'
#' @noRd
make_tab_reported <- function(reported, levels_orig, levels_dest, name) {
  reported$region_orig <- factor(reported$region_orig, levels = levels_orig)
  reported$region_dest <- factor(reported$region_dest, levels = levels_dest)
  nms_f <- setdiff(names(reported), c("region_orig", "region_dest", "count"))
  fun <- function(y) {
    val <- y[1L, nms_f, drop = FALSE]
    tab <- stats::xtabs(count ~ region_orig + region_dest, data = y)
    tab <- array(tab, dim = dim(tab), dimnames = dimnames(tab))
    val[[name]] <- list(tab)
    val
  }
  ans <- split(reported, f = reported[nms_f])
  ans <- lapply(ans, fun)
  ans <- do.call(rbind, ans)
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Reformat a 'totals' data frame to have a list column
#' with tabulations of counts by region
#'
#' Reformat a 'totals' data frame so that it
#' holds regional totals, possibly tabulated against
#' iteration, for each combination
#' of the classifying variables
#'
#' Does not assume that every region
#' is present for every combination of
#' classifying variables.
#'
#' Does not pad out vectors with NAs to account for
#' external regions. Assume that this is done
#' later in the process.
#'
#' @param totals A data frame
#' @param levels_reg Character vector with complete
#' set of regions
#' @param name Name of data frame - "totals_in" or "totals_out".
#'
#' @return A data frame with a list column
#' holding vectors if totals$count is numeric,
#' or matrices where each column is an iteration
#' if totals$count is a list.
#'
#' @noRd
make_tab_totals <- function(totals, levels_reg, name) {
  has_iter <- is.list(totals$count)
  if (has_iter) {
    n_iter <- length(totals$count[[1L]])
    n_region <- length(levels_reg)
    zeros <- rep(0L, times = n_iter)
    fun <- function(y) { ## fun is a closure - it takes arguments from its environment
      val <- y[1L, nms_f, drop = FALSE]
      tab <- y$count[match(levels_reg, y$region)]
      tab[vapply(tab, is.null, TRUE)] <- list(zeros)
      tab <- do.call(rbind, tab)
      dimnames(tab) <- list(region = levels_reg, iter = NULL)
      val[[name]] <- list(tab)
      val
    }
  } else {
    fun <- function(y) {
      val <- y[1L, nms_f, drop = FALSE]
      tab <- y$count[match(levels_reg, y$region)]
      tab[is.na(tab)] <- 0L
      names(tab) <- levels_reg
      val[[name]] <- list(tab)
      val
    }
  }
  nms_f <- setdiff(names(totals), c("region", "count"))
  ans <- split(totals, f = totals[nms_f])
  ans <- lapply(ans, fun)
  ans <- do.call(rbind, ans)
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Combine totals and reported values to create
#' inputs needed for IPF and for assembling outputs
#'
#' Main function for assembling inputs for
#' \code{split_multi}. Blank matrices are created
#' for combinations of classifying variables
#' not found in the 'reported' data frames.
#'
#' Include dimnames in result for debugging, documentation.
#'
#' @param totals_in, totals_out Data frame with 'region' column and
#  with 'count' column that is possibly a list.
#' @param reported_int, reported_im, reported_em Data frame
#' with 'region_orig', 'region_dest', and 'count' columns.
#' @param epsilon Small amount added to off-diagonal entries
#' in reported_int, and all entries in reported_im
#' and reported_em.
#'
#' @return A named list with components
#' "m", "colsums', and "rowsums",
#' "classif_vars", and "n_external"
#'
#' @noRd
make_inputs_multi <- function(totals_in,
                              totals_out,
                              reported_int,
                              reported_im,
                              reported_em,
                              epsilon) {
  has_iter <- is.list(totals_in$count)
  if (has_iter) {
    n_iter <- length(totals_in$count[[1L]])
  }
  if (is.factor(totals_in$region)) {
    levels_internal <- levels(totals_in$region)
  } else {
    levels_internal <- unique(totals_in$region)
  }
  if (is.factor(reported_im$region_orig)) {
    levels_external <- levels(reported_im$region_orig)
  } else {
    levels_external <- unique(reported_im$region_orig)
  }
  totals_in <- make_tab_totals(
    totals = totals_in,
    levels_reg = levels_internal,
    name = "totals_in"
  )
  totals_out <- make_tab_totals(
    totals = totals_out,
    levels_reg = levels_internal,
    name = "totals_out"
  )
  reported_int <- make_tab_reported(
    reported = reported_int,
    levels_orig = levels_internal,
    levels_dest = levels_internal,
    name = "reported_int"
  )
  reported_im <- make_tab_reported(
    reported = reported_im,
    levels_orig = levels_external,
    levels_dest = levels_internal,
    name = "reported_im"
  )
  reported_em <- make_tab_reported(
    reported = reported_em,
    levels_orig = levels_internal,
    levels_dest = levels_external,
    name = "reported_em"
  )
  combined <- Reduce(
    function(x, y) merge(x, y, all.x = TRUE),
    list(
      totals_in,
      totals_out,
      reported_int,
      reported_im,
      reported_em
    )
  )
  ## classif_vars
  nms_classif_vars <- setdiff(
    names(combined),
    c(
      "totals_in",
      "totals_out",
      "reported_int",
      "reported_im",
      "reported_em"
    )
  )
  classif_vars <- combined[nms_classif_vars]
  ## m
  n_internal <- length(levels_internal)
  n_external <- length(levels_external)
  m_int <- matrix(epsilon,
    nrow = n_internal,
    ncol = n_internal,
    dimnames = list(levels_internal, levels_internal)
  )
  diag(m_int) <- 0
  m_im <- matrix(epsilon,
    nrow = n_external,
    ncol = n_internal,
    dimnames = list(levels_external, NULL)
  )
  m_em <- matrix(epsilon,
    nrow = n_internal,
    ncol = n_external,
    dimnames = list(NULL, levels_external)
  )
  m_ex <- matrix(0,
    nrow = n_external,
    ncol = n_external
  )
  build_m <- function(reported_int, reported_im, reported_em) {
    combn_vars_missing_int <- isTRUE(all.equal(reported_int, NA))
    combn_vars_missing_im <- isTRUE(all.equal(reported_im, NA))
    combn_vars_missing_em <- isTRUE(all.equal(reported_em, NA))
    if (combn_vars_missing_int) {
      reported_int <- m_int
    } else {
      reported_int <- reported_int + m_int
    }
    if (combn_vars_missing_im) {
      reported_im <- m_im
    } else {
      reported_im <- reported_im + m_im
    }
    if (combn_vars_missing_em) {
      reported_em <- m_em
    } else {
      reported_em <- reported_em + m_em
    }
    ans <- rbind(
      cbind(reported_int, reported_em),
      cbind(reported_im, m_ex)
    )
    names(dimnames(ans)) <- c("region_orig", "region_dest")
    ans
  }
  m <- with(
    combined,
    mapply(build_m,
      reported_int, reported_im, reported_em,
      SIMPLIFY = FALSE
    )
  )
  ## rowsums, colsums
  if (has_iter) {
    nas <- matrix(NA,
      nrow = n_external,
      ncol = n_iter,
      dimnames = list(levels_external, NULL)
    )
    add_nas_for_external <- function(x) {
      ans <- rbind(x, nas)
      names(dimnames(ans)) <- c("region", "iter")
      ans
    }
  } else {
    nas <- rep(NA, times = n_external)
    names(nas) <- levels_external
    add_nas_for_external <- function(x) c(x, nas)
  }
  colsums <- lapply(combined$totals_in, add_nas_for_external)
  rowsums <- lapply(combined$totals_out, add_nas_for_external)
  ## return
  list(
    m = m,
    colsums = colsums,
    rowsums = rowsums,
    classif_vars = classif_vars,
    n_external = n_external
  )
}


## HAS_TESTS
#' Make 'vals_add'
#'
#' Make 'vals_add', the matrix used by
#' 'split_inner' to deal with negative
#' values.
#'
#' @param vals_reported A matrix
#'
#' @return A matrix of zeros.
#'
#' @noRd
make_vals_add <- function(vals_reported) {
  matrix(0,
    nrow = nrow(vals_reported),
    ncol = ncol(vals_reported)
  )
}


## HAS_TESTS
#' Make 'vals_alter'
#'
#' Make 'vals_alter', a matrix with values for 'alterability'.
#' When a column in 'reported' has a corresondinging
#' element in 'alter', we use the value (if numeric)
#' or we use a left join with 'totals' (if data frame)
#' to obtain the column in 'vals_alter'. When a column
#' in 'reported' does not have a corresponding element
#' in 'alter', we make a column of 1s.
#'
#' If the 'count' column in 'totals' is numeric,
#' then the return value has nrow(total) rows;
#' if the 'count' column is a list, then
#' the return value has length(unlist(total$count))
#' rows.
#'
#' @param totals A data frame
#' @param reported A data frame
#' @param alter A named list or NULL
#'
#' @return A matrix
#'
#' @noRd
make_vals_alter <- function(totals, reported, alter) {
  nms_counts_reported <- nms_counts_reported(
    reported = reported,
    totals = totals
  )
  ## revise name of any 'alter' column in 'totals'
  nms_unique <- make.unique(c("alter", names(totals)))
  names(totals) <- nms_unique[-1L]
  ## create object to hold answer
  ans <- vector(mode = "list", length = length(nms_counts_reported))
  names(ans) <- nms_counts_reported
  ## expand data frames in 'alter'
  nms_alter <- names(alter)
  for (nm in nms_counts_reported) {
    if (nm %in% nms_alter) {
      element <- alter[[nm]]
      if (is.numeric(element)) {
        ans[[nm]] <- rep(element, times = nrow(totals))
      } else if (is.data.frame(element)) {
        element <- simple_left_join(x = totals, y = element)
        ans[[nm]] <- element[["alter"]]
      } else {
        stop("element '", nm, "' of alter has class '", class(element), "'",
          call. = FALSE
        )
      }
    } else {
      ans[[nm]] <- rep(1, times = nrow(totals))
    }
  }
  ## expand answer if 'count' in 'total' is a list column
  count <- totals[["count"]]
  if (is.list(count)) {
    n_iter <- length(count[[1L]])
    ans[] <- lapply(ans, rep, each = n_iter)
  }
  ## return result
  ans <- do.call(cbind, ans)
  ans
}


## HAS_TESTS
#' Make 'vals_reported'
#'
#' Make a matrix holding values for reported counts
#' that are aligned to totals.
#'
#' Rows with all zeros are turned into rows with
#' all ones. This should always happen when the
#' corresponding values in 'total' are zero.
#' If the corresponding values in 'total' are
#' non-zero, then, to have passed 'check_totals_reported',
#' we must have 'zeros_to_ones' is TRUE, in which
#' case the zeros should also be converted to ones.
#'
#' If the 'count' column in 'totals' is numeric,
#' then the return value has nrow(total) rows;
#' if the 'count' column is a list, then
#' the return value has length(unlist(total$count))
#' rows.
#'
#' @param totals A data frame
#' @param reported A data frame
#'
#' @return A matrix.
#'
#' @noRd
make_vals_reported <- function(totals, reported) {
  cols_common <- intersect(names(totals), names(reported))
  id_totals <- totals[cols_common]
  combined <- simple_left_join(x = totals, y = reported)
  nms_counts_reported <- nms_counts_reported(
    reported = reported,
    totals = totals
  )
  ans <- combined[nms_counts_reported]
  all_zero <- rowSums(ans) == 0L
  all_zero[is.na(all_zero)] <- FALSE
  ans[all_zero, ] <- 1L
  count <- totals[["count"]]
  if (is.list(count)) {
    n_iter <- length(count[[1L]])
    ans <- lapply(ans, rep, each = n_iter)
    ans <- do.call(cbind, ans)
  } else {
    ans <- as.matrix(ans)
  }
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Make 'vals_total'
#'
#' Make a vector holding values for totals.
#'
#' If the 'count' column in 'totals' is numeric,
#' then the return value has length nrow(total);
#' if the 'count' column is a list, then
#' the return value has length length(unlist(total$count).
#'
#' @param totals A data frame
#'
#' @return A numeric vector
#'
#' @noRd
make_val_total <- function(totals) {
  ans <- totals[["count"]]
  if (is.list(ans)) {
    ans <- unlist(ans)
  }
  ans
}
