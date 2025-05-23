## HAS_TESTS
#' Split flows for single population
#'
#' Given combined flows into or out of a single population,
#' split the flows into separate streams.
#'
#' The \code{totals} argument holds the combined flows
#' plus one or more classification variables.
#' For instance, \code{totals} might describe
#' all outflows from a population, disaggregated by age,
#' sex/gender, cohort, and time. The flows are held
#' in a variable called \code{"count"}.
#' The count variable can be an ordinary numeric vector,
#' or, if the flows are uncertain, a list column.
#' If \code{totals} was generated from the base model,
#' then the count variable will be a list column.
#'
#' The \code{reported} argument holds data on
#' the disaggregated flows, plus
#' one or more classification variables.
#' For instance, \code{reported} might have data on
#' (i) migration to other regions of the country,
#' held in a variable called \code{"internal_out"},
#' and (ii) migration to other countries, held in a
#' variable called \code{"emigration"}, as well as
#' classification variables \code{"age"} and \code{"time"}.
#' \code{reported} does not need to
#' have all the classification variables in \code{totals},
#' but if it does have a variable, then that variable
#' must have all the levels that are found in the
#' \code{totals} version.
#'
#' The \code{alter} argument holds values for the
#' 'alterability' parameter, which is applied
#' to the data in \code{reported}. \code{alter} is optional:
#' if no value is supplied, then all observations in
#' \code{reported} are assigned an alterability score of 1.
#' \code{alter} is a named list of numbers
#' or data frames. Each number or
#' data frame refers to a type of flow in
#' \code{reported}. If a flow in \code{"reported"} does not have a
#' corresponding data frame in \code{"alter"}, then
#' all values for that flow receive an alterability
#' score of 1. If a flow in \code{"reported"} has a
#' number in \code{"alter"} (eg \code{1.7}), then
#' that number is used as the alterability score for
#' all values in the flow.
#' If a flow has a data frame, then different elements
#' within the flow receive different alterability scores
#' (found in the \code{alter} value column of the data frame.)
#' As with \code{report}, \code{alter} does not have to have all
#' the classification variables that are found in \code{totals},
#' but if it does have a variable, that variable must have
#' a complete set of levels.
#'
#' The returned value is identical to \code{totals},
#' except that the \code{"count"} column is replaced by
#' the flow columns for \code{"reported"}, scaled so that
#' the flows add up to the values in \code{"count"}.
#' Values with high alterability scores are modified more
#' than values with low alterability scores.
#'
#' The adjustment process is based on [Quenneville and Rancourt 2005](https://ec.europa.eu/eurostat/documents/3888793/5837169/KS-DT-05-016-EN.PDF.pdf/9f326f78-ca08-4344-a3ef-5b1f2cb515c9?t=1414779260000). Sometimes, if reported values
#' are being scaled downwards and a reported value is near zero but
#' has a high alterability score, the original Quenneville and  Rancourt
#' can produce negative values for flows. \code{split_single}
#' avoids negative values by setting the flow to zero and recalculating:
#' see the vignette to this package for details.
#'
#' In small populations, sometimes all values in a
#' row of \code{reported} are zero while a corresponding
#' value in \code{totals} is non-zero, and the usual formula
#' breaks down. A simple solution is to
#' change all values in the row of \code{reported} to 1,
#' which implicitly weights each data source equally.
#' To apply this solution, set \code{zeros_to_ones} to
#' \code{TRUE}. If zero cells are common, a more sophisticated
#' approach, such as smoothing counts in \code{reported}
#' before passing it to function \code{split_single}, might
#' be appropriate.
#'
#' \code{NA}s are permitted in \code{totals} and \code{reported},
#' and will propagate through to the results. \code{NA}s are not
#' permitted in \code{alter}.
#'
#' @param totals Marginal totals.
#' A data frame with columns of 'classification
#' variables', plus outcome variable \code{"count"}, in any order.
#' The \code{"count"} column can be a numeric vector, or can be
#' a list of numeric vectors of equal length.
#' @param reported Reported migration counts.
#' A data frame with a subset of the classification
#' variables from \code{totals}, and, instead of \code{"count"},
#' two or more columns of reported migration flows.
#' @param alter Alterability values. A named
#' list of data frames. The data frames specify
#' alterability values for each type of reported migration
#' flow. The data frames each have a subset of the classification
#' variables from \code{reported}, plus a column
#' called \code{"alter"}. If no data frame is supplied for a
#' migration flow, then every level of that flow is assumed to have
#' an alterability value of 1. If no value for \code{alter}
#' is supplied, then all alterability values are set to 1.
#' @param zeros_to_ones If a row of \code{reported}
#' consists entirely of zeros, replace it with a row of ones.
#' Defaults to \code{FALSE}.
#'
#' @return A data frame with the same classification variables
#' as \code{totals}, but with the \code{"count"} column split
#' into separate flows.
#'
#' @seealso \code{\link{split_multi}}
#'
#' @examples
#' totals <- data.frame(
#'   age = c("20-24", "25-29", "20-24", "25-29"),
#'   sex = c("Female", "Female", "Male", "Male"),
#'   count = c(20, 24, 15, 8)
#' )
#' reported <- data.frame(
#'   age = c("20-24", "25-29", "20-24", "25-29"),
#'   sex = c("Female", "Female", "Male", "Male"),
#'   internal = c(10, 15, 12, 3),
#'   cross_border = c(7, 8, 5, 0),
#'   international = c(3, 5, 1, 4)
#' )
#' alter <- list(
#'   cross_border = 1.2,
#'   international = data.frame(
#'     age = c("20-24", "25-29"),
#'     alter = c(2, 2.5)
#'   )
#' )
#'
#' split_single(
#'   totals = totals,
#'   reported = reported,
#'   alter = alter
#' )
#'
#' @export
split_single <- function(totals,
                         reported,
                         alter = NULL,
                         zeros_to_ones = FALSE) {
  ## check inputs
  check_totals(totals, name = "totals")
  check_reported(reported)
  check_alter(alter)
  check_zeros_to_ones(zeros_to_ones)
  check_totals_reported(
    totals = totals,
    reported = reported,
    zeros_to_ones = zeros_to_ones
  )
  check_totals_reported_alter(
    totals = totals,
    reported = reported,
    alter = alter
  )
  ## make inputs to calculations
  val_total <- make_val_total(totals)
  vals_reported <- make_vals_reported(
    reported = reported,
    totals = totals
  )
  vals_alter <- make_vals_alter(
    alter = alter,
    totals = totals,
    reported = reported
  )
  vals_add <- make_vals_add(vals_reported)
  ## do calculations
  vals_split <- split_single_inner(
    val_total = val_total,
    vals_reported = vals_reported,
    vals_alter = vals_alter,
    vals_add = vals_add
  )
  ## make outputs and return
  classif_vars <- make_classif_vars(totals)
  ans <- make_ans(
    classif_vars = classif_vars,
    vals_split = vals_split
  )
  ans
}


#' Split flows for multiple populations
#'
#' Simultaneously split out migration flows
#' for multiple populations. Flows include
#' internal migration (ie between areas within
#' the country), immigration and emigration.
#' Internally, the splitting is done via
#' iterative proportional fitting.
#'
#' Data frames \code{totals_in} and \code{totals_out}
#' hold total inflows and total outflows by region,
#' and always have \code{region} and \code{count} variables.
#' \code{totals_in} and \code{totals_out} typically
#' also have additional variables holding classification
#' variables such as age,
#' sex/gender, and time. The \code{count} variables
#' of \code{totals_in} and \code{totals_out} can
#' be numeric vectors or, to represent
#' draws from a posterior distribution, lists of numeric vectors.
#' \code{totals_in} and \code{totals_out} are
#' typically constructed by concatenating
#' \code{immigration} and \code{emigration} estimates
#' obtained by running function \code{estimate_account} in package
#' \code{dpmaccount} on multiple populations.
#'
#' Data frames \code{reported_int}, \code{reported_im},
#' and \code{reported_em} hold reported values for all
#' internal migration, immigration, and emigration
#' flows that are being estimated. All three data frames
#' must have \code{region_orig}, \code{region_dest},
#' and \code{count} variables. The categories
#' included in \code{region_orig} and
#' \code{region_dest} varies with the data frame:
#'
#' |   |  \code{region_orig} | \code{region_dest} |
#' |:--|---------------------|--------------------|
#' | \code{reported_int} | Areas inside country | Areas inside country |
#' | \code{reported_im} | Areas outside country | Areas inside country |
#' | \code{reported_em} | Areas inside country | Areas outside country |
#'
#' All three data frames can, in addition,
#' have classifying variables. These variables
#' must be the same as, or a subset of,
#' the classifying variables
#' in \code{totals_in} and \code{totals_out}.
#' The values of the classification variables
#' in \code{reported_int},
#' \code{reported_im}, and \code{reported_em}
#' must be the same as, or a subset of, the
#' values of the classification variables
#' in \code{totals_in} and \code{totals_out}.
#' The count variable in \code{reported_int},
#' \code{reported_im}, and \code{reported_em}
#' must be numeric: unlike in \code{totals_in}
#' and \code{totals_out}, it cannot be a list.
#'
#' The value \code{epsilon} is added to
#' off-diagonal elements of \code{reported_int}
#' and all elements of \code{reported_im} and
#' \code{reported_em} to
#' speed convergence and avoid numeric problems
#' with sparse data. \code{epsilon} defaults to
#' 0.001, but can be set to 0.
#'
#' @param totals_in,totals_out Data frames holding total estimated
#' ins and outs. Both include a \code{region} variable,
#' a \code{count} variable (which can be numeric or a list of numeric vectors),
#' and, optionally, further classifying variables.
#' @param reported_int,reported_im,reported_em Data frames holding
#' detailed data on internal migration, immigration, and emigration.
#' All three include variables \code{region_orig},
#' \code{region_dest}, and \code{count}, and, optionally
#' further classifying variables.
#' @param epsilon Small quantity added to off-diagonal
#' elements of \code{reported_int} and all elements of
#' \code{reported_im} and \code{reported_em}.
#' Defaults to 0.001.
#' @param max_iter Maximum number of iterations allowed in iterative
#' proportional fitting. Defaults to 1000.
#' @param tolerance Maximum absolute difference between supplied
#' row/column totals and calculated row/column totals in
#' iterative proportional fitting. Defaults to 0.000001.
#'
#' @return A named list, containing data frames
#' \code{internal}, \code{immigration},
#' and \code{emigration}.
#' .
#'
#' @seealso \code{\link{split_single}}
#'
#' @examples
#' totals_in <- data.frame(
#'   year = c(2020, 2020, 2021, 2021),
#'   region = c("a", "b", "a", "b"),
#'   count = c(13, 8, 5, 12)
#' )
#' totals_out <- data.frame(
#'   year = c(2020, 2020, 2021, 2021),
#'   region = c("a", "b", "a", "b"),
#'   count = c(9, 11, 8, 11)
#' )
#' reported_int <- data.frame(
#'   year = c(2020, 2020, 2021, 2021),
#'   region_orig = c("a", "b", "a", "b"),
#'   region_dest = c("b", "a", "b", "a"),
#'   count = c(3, 8, 4, 2)
#' )
#' reported_im <- data.frame(
#'   year = c(2020, 2020, 2021, 2021),
#'   region_orig = c("x", "x"),
#'   region_dest = c("a", "b"),
#'   count = c(4, 1)
#' )
#' reported_em <- data.frame(
#'   year = c(2020, 2020, 2021, 2021),
#'   region_orig = c("a", "b"),
#'   region_dest = c("x", "x"),
#'   count = c(3, 5)
#' )
#' split_multi(
#'   totals_in = totals_in,
#'   totals_out = totals_out,
#'   reported_int = reported_int,
#'   reported_im = reported_im,
#'   reported_em = reported_em
#' )
#' @md
#' @export
split_multi <- function(totals_in,
                        totals_out,
                        reported_int,
                        reported_im,
                        reported_em,
                        epsilon = 0.001,
                        max_iter = 1000L,
                        tolerance = 1e-6) {
  ## check inputs
  check_totals_multi(totals_in, name = "totals_in")
  check_totals_multi(totals_out, name = "totals_out")
  check_reported_multi(reported_int, name = "reported_int")
  check_reported_multi(reported_im, name = "reported_im")
  check_reported_multi(reported_em, name = "reported_em")
  check_totals_in_out(
    totals_in = totals_in,
    totals_out = totals_out
  )
  check_totals_in_reported(
    totals_in = totals_in,
    reported = reported_int,
    name = "reported_int"
  )
  check_totals_in_reported(
    totals_in = totals_in,
    reported = reported_im,
    name = "reported_im"
  )
  check_totals_in_reported(
    totals_in = totals_in,
    reported = reported_em,
    name = "reported_em"
  )
  check_epsilon(epsilon)
  check_max_iter(max_iter)
  max_iter <- as.integer(max_iter)
  check_tolerance(tolerance)
  ## make inputs to calculations
  inputs <- make_inputs_multi(
    totals_in = totals_in,
    totals_out = totals_out,
    reported_int = reported_int,
    reported_im = reported_im,
    reported_em = reported_em,
    epsilon = epsilon
  )
  m <- inputs$m
  rowsums <- inputs$rowsums
  colsums <- inputs$colsums
  classif_vars <- inputs$classif_vars
  n_external <- inputs$n_external
  ## do calculations (can convert to parallel later)
  vals <- split_multi_inner(
    m = m,
    rowsums = rowsums,
    colsums = colsums,
    max_iter = max_iter,
    tolerance = tolerance
  )
  ## make outputs and return
  make_outputs_multi(
    vals = vals,
    classif_vars = classif_vars,
    n_external = n_external
  )
}
