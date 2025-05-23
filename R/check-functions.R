## HAS_TESTS
#' Check 'alterability' values
#'
#' Check 'alter', a list of data frames holding
#' alterability scores for flows, or NULL.
#'
#' @param alter A named list of data frames or NULL.
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_alter <- function(alter) {
  ## if 'alter' is NULL, return early
  if (is.null(alter)) {
    return(invisible(TRUE))
  }
  ## 'alter' is list
  if (!is.list(alter)) {
    stop("'alter' has class '", class(alter), "'",
      call. = FALSE
    )
  }
  ## 'alter' has names
  nms <- names(alter)
  if (is.null(nms)) {
    stop("'alter' does not have names",
      call. = FALSE
    )
  }
  ## names do not have duplicates
  if (any(duplicated(nms))) {
    stop("'alter' has duplicated names",
      call. = FALSE
    )
  }
  for (i in seq_along(alter)) {
    element <- alter[[i]]
    ## is numeric
    if (is.numeric(element)) {
      if (length(element) != 1L) {
        stop("element ", i, " of 'alter' has length ", length(element),
          call. = FALSE
        )
      }
      if (is.na(element)) {
        stop("element ", i, " of 'alter' is NA",
          call. = FALSE
        )
      }
      if (element < 0) {
        stop("element ", i, " of 'alter' is negative",
          call. = FALSE
        )
      }
    }
    ## is data frame
    else if (is.data.frame(element)) {
      ## has length >= 2
      if (length(element) < 2L) {
        stop("element ", i, " of 'alter' has less than 2 columns",
          call. = FALSE
        )
      }
      ## does not have column called 'count'
      if ("count" %in% names(element)) {
        stop("element ", i, " of 'alter' has a column called 'count'",
          call. = FALSE
        )
      }
      ## has column called 'alter'
      i_alter <- match("alter", names(element), nomatch = 0L)
      if (i_alter == 0L) {
        stop("element ", i, " of 'alter' does not have a column called 'alter'",
          call. = FALSE
        )
      }
      col_alter <- element[[i_alter]]
      ## 'alter' column is numeric
      if (!is.numeric(col_alter)) {
        stop("'alter' column of element ", i, " of 'alter' has class '",
          class(col_alter), "'",
          call. = FALSE
        )
      }
      ## 'alter' column has no NAs
      if (anyNA(col_alter)) {
        stop("'alter' column of element ", i, " of 'alter' has NAs",
          call. = FALSE
        )
      }
      ## 'alter' column is has no negative values
      if (any(col_alter < 0)) {
        stop("'alter' column of element ", i, " of 'alter' has negative values",
          call. = FALSE
        )
      }
    } else {
      stop("element ", i, " of 'alter' has class '", class(element), "'",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check 'epsilon'
#'
#' @param epsilon
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_epsilon <- function(epsilon) {
  if (!is.numeric(epsilon)) {
    stop("'epsilon' has class '", class(epsilon), "'",
      call. = FALSE
    )
  }
  if (!identical(length(epsilon), 1L)) {
    stop("'epsilon' has length ", length(epsilon),
      call. = FALSE
    )
  }
  if (is.na(epsilon)) {
    stop("'epsilon' is NA",
      call. = FALSE
    )
  }
  if (epsilon < 0) {
    stop("'epsilon' is negative",
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check 'max_iter'
#'
#' @param max_iter
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_max_iter <- function(max_iter) {
  if (!is.numeric(max_iter)) {
    stop("'max_iter' has class '", class(max_iter), "'",
      call. = FALSE
    )
  }
  if (!identical(length(max_iter), 1L)) {
    stop("'max_iter' has length ", length(max_iter),
      call. = FALSE
    )
  }
  if (is.na(max_iter)) {
    stop("'max_iter' is NA",
      call. = FALSE
    )
  }
  if (max_iter < 1L) {
    stop("'max_iter' is less than 1",
      call. = FALSE
    )
  }
  if (!isTRUE(all.equal(round(max_iter), max_iter))) {
    stop("'max_iter' is not a whole number",
      call. = FALSE
    )
  }
  invisible(TRUE)
}



## HAS_TESTS
#' Check reported values
#'
#' Check 'reported', a data frame holding
#' reported counts for each flow, plus
#' classification variables.
#'
#' @param reported A data frame.
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_reported <- function(reported) {
  ## 'reported' is a data frame
  if (!is.data.frame(reported)) {
    stop("'reported' has class '", class(reported), "'",
      call. = FALSE
    )
  }
  ## has at least one row
  if (nrow(reported) == 0L) {
    stop("'reported' has zero rows",
      call. = FALSE
    )
  }
  ## return
  invisible(TRUE)
}


## HAS_TESTS
#' Check reported values for internal
#' and international migration
#'
#' Check 'reported', a data frame holding
#' reported counts for internal and international
#' flows, plus region and classification
#' variables.
#'
#' @param reported A data frame.
#' @param name Name of the data frame.
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_reported_multi <- function(reported, name) {
  ## 'reported' is a data frame
  if (!is.data.frame(reported)) {
    stop("'", name, "' has class '", class(reported), "'",
      call. = FALSE
    )
  }
  ## has at least one row
  if (nrow(reported) == 0L) {
    stop("'", name, "' has zero rows",
      call. = FALSE
    )
  }
  for (nm in c("region_orig", "region_dest")) {
    ## has 'region_orig', 'region_dest' columns
    i_region <- match(nm, names(reported), nomatch = 0L)
    if (i_region == 0L) {
      stop("'", name, "' does not have a column called '", nm, "'",
        call. = FALSE
      )
    }
    ## region column has no NAs
    region <- reported[[i_region]]
    if (anyNA(region)) {
      stop("'", nm, "' column in '", name, "' has NAs",
        call. = FALSE
      )
    }
  }
  region_orig <- reported$region_orig
  region_dest <- reported$region_dest
  ## if is 'reported_int', 'region_orig' != 'region_dest'
  if (identical(name, "reported_int")) {
    is_reg_equal <- region_orig == region_dest
    i_reg_equal <- match(TRUE, is_reg_equal, nomatch = 0L)
    if (i_reg_equal > 0L) {
      stop("'region_orig' and 'region_dest' in row ",
        i_reg_equal, " of '", name, "' both have value '",
        region_orig[[i_reg_equal]], "'",
        call. = FALSE
      )
    }
  }
  ## has 'count' column
  i_count <- match("count", names(reported), nomatch = 0L)
  if (i_count == 0L) {
    stop("'", name, "' does not have a column called 'count'",
      call. = FALSE
    )
  }
  count <- reported[[i_count]]
  ## non-count columns have no duplicates
  classif_vars <- reported[-i_count]
  is_duplicated <- duplicated(classif_vars)
  i_duplicated <- match(TRUE, is_duplicated, nomatch = 0L)
  if (i_duplicated > 0L) {
    stop("'", name, "' has two rows with the following values : ",
      paste(classif_vars[i_duplicated, ], collapse = ", "),
      call. = FALSE
    )
  }
  ## 'count' has no NAs
  if (anyNA(count)) {
    stop("'count' column in '", name, "' has NAs",
      call. = FALSE
    )
  }
  ## 'counts' column is numeric:
  if (is.numeric(count)) {
    if (any(count < 0)) {
      stop("'count' column in '", name, "' has negative values",
        call. = FALSE
      )
    }
  }
  ## return
  invisible(TRUE)
}


## HAS_TESTS
#' Check 'tolerance'
#'
#' @param tolerance
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_tolerance <- function(tolerance) {
  if (!is.numeric(tolerance)) {
    stop("'tolerance' has class '", class(tolerance), "'",
      call. = FALSE
    )
  }
  if (!identical(length(tolerance), 1L)) {
    stop("'tolerance' has length ", length(tolerance),
      call. = FALSE
    )
  }
  if (is.na(tolerance)) {
    stop("'tolerance' is NA",
      call. = FALSE
    )
  }
  if (tolerance <= 0) {
    stop("'tolerance' is non-positive",
      call. = FALSE
    )
  }
  invisible(TRUE)
}



## HAS_TESTS
#' Check target totals
#'
#' Check 'totals', a data frame holding
#' fixed totals in a column called 'count',
#' plus classification variables.
#'
#' @param totals A data frame.
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_totals <- function(totals, name) {
  ## is a data frame
  if (!is.data.frame(totals)) {
    stop("'totals' has class '", class(totals), "'",
      call. = FALSE
    )
  }
  ## has at least one row
  if (nrow(totals) == 0L) {
    stop("'totals' has zero rows",
      call. = FALSE
    )
  }
  ## has at least two columns
  if (ncol(totals) < 2L) {
    stop("'totals' has ", ncol(totals), " column(s)",
      call. = FALSE
    )
  }
  ## has 'count' column
  i_count <- match("count", names(totals), nomatch = 0L)
  if (i_count == 0L) {
    stop("'totals' does not have a column called 'count'",
      call. = FALSE
    )
  }
  count <- totals[[i_count]]
  ## non-count columns have no duplicates
  classif_vars <- totals[-i_count]
  is_duplicated <- duplicated(classif_vars)
  i_duplicated <- match(TRUE, is_duplicated, nomatch = 0L)
  if (i_duplicated > 0L) {
    stop("'totals' has two rows with the following values : ",
      paste(classif_vars[i_duplicated, ], collapse = ", "),
      call. = FALSE
    )
  }
  ## 'counts' column is numeric:
  if (is.numeric(count)) {
    if (any(count < 0, na.rm = TRUE)) {
      stop("'count' column in 'totals' has negative values",
        call. = FALSE
      )
    }
  }
  ## 'counts' column is a list:
  else if (is.list(count)) {
    ## same length
    if (nrow(totals) > 1L) {
      lengths <- vapply(count, length, 1L)
      if (any(lengths[-1L] != lengths[[1L]])) {
        stop("elements of 'count' column in 'totals' have different lengths",
          call. = FALSE
        )
      }
    }
    count_unlist <- unlist(count)
    ## all numeric
    if (!is.numeric(count_unlist)) {
      stop("'count' column in 'totals' has non-numeric values",
        call. = FALSE
      )
    }
    ## all non-negative
    if (any(count_unlist < 0, na.rm = TRUE)) {
      stop("'count' column in 'totals' has negative values",
        call. = FALSE
      )
    }
  } else {
    stop("'count' column in 'totals' has class '", class(count), "'",
      call. = FALSE
    )
  }
  ## return
  invisible(TRUE)
}


## HAS_TESTS
#' Check that totals_in and totals_out
#' are consistent with each other
#'
#' Check that 'totals_in' and 'totals_out'
#' have the same classification variables
#' (not necessarily in the same order)
#' and the same values.
#'
#' @param totals_in A data frame.
#' @param totals_out A data frame.
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_totals_in_out <- function(totals_in, totals_out) {
  nms_classif_vars_in <- setdiff(names(totals_in), "count")
  nms_classif_vars_out <- setdiff(names(totals_out), "count")
  ## same columns
  if (!setequal(nms_classif_vars_in, nms_classif_vars_out)) {
    stop("'totals_in' and 'totals_out' have different column names",
      call. = FALSE
    )
  }
  ## contents of columns same
  for (nm in nms_classif_vars_in) {
    unique_in <- unique(totals_in[[nm]])
    unique_out <- unique(totals_out[[nm]])
    is_found_out <- unique_in %in% unique_out
    i_not_found_out <- match(FALSE, is_found_out, nomatch = 0L)
    if (i_not_found_out > 0L) {
      stop("'", nm, "' column of 'totals_in' has value '", unique_in[[i_not_found_out]],
        "' but '", nm, "' column of 'totals_out' does not",
        call. = FALSE
      )
    }
    is_found_in <- unique_out %in% unique_in
    i_not_found_in <- match(FALSE, is_found_in, nomatch = 0L)
    if (i_not_found_in > 0L) {
      stop("'", nm, "' column of 'totals_out' has value '", unique_out[[i_not_found_in]],
        "' but '", nm, "' column of 'totals_in' does not",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check totals_in and totals_out
#' independently of each other
#'
#' Check 'totals', a data frame holding
#' fixed totals in a column called 'count',
#' a column called 'region',
#' and optional classification variables.
#'
#' @param totals A data frame.
#' @param name Name of the data frame.
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_totals_multi <- function(totals, name) {
  ## is a data frame
  if (!is.data.frame(totals)) {
    stop("'", name, "' has class '", class(totals), "'",
      call. = FALSE
    )
  }
  ## has at least one row
  if (nrow(totals) == 0L) {
    stop("'", name, "' has zero rows",
      call. = FALSE
    )
  }
  ## has 'region' column
  i_region <- match("region", names(totals), nomatch = 0L)
  if (i_region == 0L) {
    stop("'", name, "' does not have a column called 'region'",
      call. = FALSE
    )
  }
  region <- totals[[i_region]]
  ## 'region' column has no NAs
  if (anyNA(region)) {
    stop("'region' column in '", name, "' has NAs",
      call. = FALSE
    )
  }
  ## has 'count' column
  i_count <- match("count", names(totals), nomatch = 0L)
  if (i_count == 0L) {
    stop("'", name, "' does not have a column called 'count'",
      call. = FALSE
    )
  }
  count <- totals[[i_count]]
  ## non-count columns have no duplicates
  classif_vars <- totals[-i_count]
  is_duplicated <- duplicated(classif_vars)
  i_duplicated <- match(TRUE, is_duplicated, nomatch = 0L)
  if (i_duplicated > 0L) {
    stop("'", name, "' has two rows with the following values : ",
      paste(classif_vars[i_duplicated, ], collapse = ", "),
      call. = FALSE
    )
  }
  ## 'counts' column is numeric:
  if (is.numeric(count)) {
    ## no NAs
    if (anyNA(count)) {
      stop("'count' column in '", name, "' has NAs",
        call. = FALSE
      )
    }
    ## no negatives
    if (any(count < 0, na.rm = TRUE)) {
      stop("'count' column in '", name, "' has negative values",
        call. = FALSE
      )
    }
  }
  ## 'counts' column is a list:
  else if (is.list(count)) {
    ## same length
    if (nrow(totals) > 1L) {
      lengths <- vapply(count, length, 1L)
      if (any(lengths[-1L] != lengths[[1L]])) {
        stop("elements of 'count' column in '", name, "' have different lengths",
          call. = FALSE
        )
      }
    }
    count_unlist <- unlist(count)
    ## all numeric
    if (!is.numeric(count_unlist)) {
      stop("'count' column in '", name, "' has non-numeric values",
        call. = FALSE
      )
    }
    ## no NAs
    if (anyNA(count_unlist)) {
      stop("'count' column in '", name, "' has NAs",
        call. = FALSE
      )
    }
    ## all non-negative
    if (any(count_unlist < 0)) {
      stop("'count' column in '", name, "' has negative values",
        call. = FALSE
      )
    }
  } else {
    stop("'count' column in '", name, "' has class '", class(count), "'",
      call. = FALSE
    )
  }
  ## return
  invisible(TRUE)
}


## HAS_TESTS
#' Check that totals_in and totals_out
#' are consistent with each other
#'
#' Check that 'totals_in' and 'totals_out'
#' have the same classification variables
#' (not necessarily in the same order)
#' and the same values.
#'
#' @param totals_in A data frame.
#' @param totals_out A data frame.
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_totals_in_out <- function(totals_in, totals_out) {
  nms_classif_vars_in <- setdiff(names(totals_in), "count")
  nms_classif_vars_out <- setdiff(names(totals_out), "count")
  ## same columns
  if (!setequal(nms_classif_vars_in, nms_classif_vars_out)) {
    stop("'totals_in' and 'totals_out' have different column names",
      call. = FALSE
    )
  }
  ## contents of columns same
  for (nm in nms_classif_vars_in) {
    unique_in <- unique(totals_in[[nm]])
    unique_out <- unique(totals_out[[nm]])
    is_found_out <- unique_in %in% unique_out
    i_not_found_out <- match(FALSE, is_found_out, nomatch = 0L)
    if (i_not_found_out > 0L) {
      stop("'", nm, "' column of 'totals_in' has value '", unique_in[[i_not_found_out]],
        "' but '", nm, "' column of 'totals_out' does not",
        call. = FALSE
      )
    }
    is_found_in <- unique_out %in% unique_in
    i_not_found_in <- match(FALSE, is_found_in, nomatch = 0L)
    if (i_not_found_in > 0L) {
      stop("'", nm, "' column of 'totals_out' has value '", unique_out[[i_not_found_in]],
        "' but '", nm, "' column of 'totals_in' does not",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that totals_in and reported_*
#' are consistent with each other
#'
#' Check that classification variables in
#' 'reported_int', 'reported_in', or 'reported_out'
#' are a subset of those in 'totals_in'
#' (allowing for 'region' vs 'region_orig', region_dest'),
#' and variables have same range of values.
#'
#' @param totals_in A data frame.
#' @param reported A data frame.
#' @param name Name of 'reported' ("reported_int",
#' "reported_im", or "reported_em").
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_totals_in_reported <- function(totals_in, reported, name) {
  nms_tot_withreg <- setdiff(names(totals_in), "count")
  nms_tot_noreg <- setdiff(nms_tot_withreg, "region")
  nms_rep <- setdiff(
    names(reported),
    c("region_orig", "region_dest", "count")
  )
  ## same classifying variables (excluding region)
  is_in_tot <- nms_rep %in% nms_tot_noreg
  i_not_in_tot <- match(FALSE, is_in_tot, nomatch = 0L)
  if (i_not_in_tot > 0L) {
    stop("'", name, "' has column called '", nms_rep[[i_not_in_tot]],
      "' but 'totals_in' does not",
      call. = FALSE
    )
  }
  ## classifying variables in reported are subset of
  ## classifying variables in totals
  for (nm_tot in nms_tot_withreg) {
    if (nm_tot == "region") {
      if (name == "reported_im") {
        nm_rep <- "region_dest"
      } else {
        nm_rep <- "region_orig"
      }
    } else {
      nm_rep <- nm_tot
    }
    unique_tot <- unique(totals_in[[nm_tot]])
    unique_rep <- unique(reported[[nm_rep]])
    is_in_tot <- unique_rep %in% unique_tot
    i_not_in_tot <- match(FALSE, is_in_tot, nomatch = 0L)
    if (i_not_in_tot > 0L) {
      stop("'", nm_rep, "' column of '", name, "' has value '", unique_rep[[i_not_in_tot]],
        "' but '", nm_tot, "' column of 'totals_in' does not",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'totals' and 'reported' are consistent,
#' and that the count variables in 'reported' are valid
#'
#' Check that the classification variables in 'reported'
#' have all the levels of the corresponding variables
#' in 'totals'. Also check the validity of the counts
#' columns in 'reported' (which can only be identified
#' by comparing names in 'reported' and 'totals'.)
#'
#' @param totals A data frame
#' @param reported A data frame
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_totals_reported <- function(totals, reported, zeros_to_ones) {
  nms_totals <- names(totals)
  nms_reported <- names(reported)
  nm_count_totals <- "count"
  nms_classif_totals <- setdiff(nms_totals, nm_count_totals)
  nms_counts_reported <- nms_counts_reported(
    totals = totals,
    reported = reported
  )
  nms_classif_reported <- setdiff(nms_reported, nms_counts_reported)
  ## classif variables in 'reported' have all the levels of
  ## the correspondings classif variables in 'totals'
  for (nm in nms_classif_reported) {
    levels_tot <- unique(totals[[nm]])
    levels_rep <- unique(reported[[nm]])
    is_in_rep <- levels_tot %in% levels_rep
    i_not_in_rep <- match(FALSE, is_in_rep, nomatch = 0L)
    if (i_not_in_rep > 0L) {
      stop("column '", nm, "' in 'totals' ",
        "has value '", levels_tot[[i_not_in_rep]],
        "', but column '", nm, "' in 'reported' does not",
        call. = FALSE
      )
    }
  }
  for (nm in nms_counts_reported) {
    col_count <- reported[[nm]]
    ## count variable in 'reported' is numeric
    if (!is.numeric(col_count)) {
      stop("column '", nm, "' in 'reported' is non-numeric",
        call. = FALSE
      )
    }
    ## count variable in 'reported' has no negative values
    if (any(col_count < 0, na.rm = TRUE)) {
      stop("column '", nm, "' in 'reported' has negative value",
        call. = FALSE
      )
    }
  }
  ## if zeros_to_ones is FALSE, check for rows where
  ## reported values are all zero, but totals are not
  if (!zeros_to_ones) {
    count_totals <- totals[[nm_count_totals]]
    if (is.list(count_totals)) {
      totals_has_nonzero <- vapply(
        count_totals,
        function(x) any(x > 0L, na.rm = TRUE),
        TRUE
      )
    } else {
      totals_has_nonzero <- !is.na(count_totals) & (count_totals > 0L)
    }
    reported_all_zero <- rowSums(reported[nms_counts_reported]) == 0L
    reported_all_zero[is.na(reported_all_zero)] <- FALSE
    n_nonzero_zero <- sum(totals_has_nonzero & reported_all_zero, na.rm = TRUE)
    if (n_nonzero_zero > 0L) {
      if (n_nonzero_zero == 1L) {
        msg <- "1 case"
      } else {
        msg <- sprintf("%d cases", n_nonzero_zero)
      }
      msg <- paste(
        msg,
        "where values in 'reported' are all zero",
        "but corresponding value in 'totals' is non-zero :",
        "consider setting 'zeros_to_ones' to TRUE"
      )
      stop(msg, call. = FALSE)
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'totals', 'reported', and 'alter' are consistent
#'
#' Check that names of elements of 'alter' refer to counts
#' variables in 'report', and that columns of 'alter'
#' exist in 'totals'. Also check that 'alter' is not
#' mssing any levels from 'totals'.
#'
#' @param totals A data frame
#' @param reported A data frame
#' @param alter NULL or a named list of data frames
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_totals_reported_alter <- function(totals, reported, alter) {
  ## if 'alter' is NULL, return early
  if (is.null(alter)) {
    return(invisible(TRUE))
  }
  ## extract names
  nms_totals <- names(totals)
  nms_reported <- names(reported)
  nms_alter <- names(alter)
  nm_count_totals <- "count"
  nms_classif_totals <- setdiff(nms_totals, nm_count_totals)
  nms_counts_reported <- nms_counts_reported(
    totals = totals,
    reported = reported
  )
  nms_classif_reported <- setdiff(nms_reported, nms_counts_reported)
  ## names of 'alter' subset of names of count columns in 'reported'
  is_in_report <- nms_alter %in% nms_counts_reported
  i_not_in_report <- match(FALSE, is_in_report, nomatch = 0L)
  if (i_not_in_report > 0L) {
    stop("'alter' has an element called '",
      nms_alter[[i_not_in_report]],
      "', but 'reported' does not have a column called '",
      nms_alter[[i_not_in_report]], "'",
      call. = FALSE
    )
  }
  for (i in seq_along(alter)) {
    element <- alter[[i]]
    nm_element <- nms_alter[[i]]
    nms_classif_el <- setdiff(names(element), "alter")
    is_in_total <- nms_classif_el %in% nms_classif_totals
    i_not_in_total <- match(FALSE, is_in_total, nomatch = 0L)
    if (i_not_in_total > 0L) {
      stop("element '", nm_element, "' of 'alter' ",
        "has a column called '",
        nms_classif_el[[i_not_in_total]],
        "', but 'totals' does not have a column called '",
        nms_classif_el[[i_not_in_total]], "'",
        call. = FALSE
      )
    }
    for (nm in nms_classif_el) {
      levels_tot <- unique(totals[[nm]])
      levels_alt <- unique(element[[nm]])
      is_in_alt <- levels_tot %in% levels_alt
      i_not_in_alt <- match(FALSE, is_in_alt, nomatch = 0L)
      if (i_not_in_alt > 0L) {
        stop("column '", nm, "' in 'totals' ",
          "has value '", levels_tot[[i_not_in_alt]],
          "', but column '", nm, "' in element '",
          nm_element, "' in 'alter' does not have value '",
          levels_tot[[i_not_in_alt]], "'",
          call. = FALSE
        )
      }
    }
  }
  invisible(TRUE)
}


#' Check 'zeros_to_ones' logical flag
#'
#' @param zeros_to_ones
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_zeros_to_ones <- function(zeros_to_ones) {
  if (!is.logical(zeros_to_ones)) {
    stop("'zeros_to_ones' has class '", class(zeros_to_ones), "'",
      call. = FALSE
    )
  }
  if (!identical(length(zeros_to_ones), 1L)) {
    stop("'zeros_to_ones' has length ", length(zeros_to_ones),
      call. = FALSE
    )
  }
  if (is.na(zeros_to_ones)) {
    stop("'zeros_to_ones' is NA",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
