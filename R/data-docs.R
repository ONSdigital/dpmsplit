#' Simulated estimates for total outflows
#'
#' Simulated estimates of outflows from a population,
#' mimicking the results from running the base model,
#' though with only 10 iterations.
#'
#' @format A data frame with 38 rows and 3 columns:
#' \describe{
#'   \item{age}{Five-year age group.}
#'   \item{time}{2021 or 2022.}
#'   \item{count}{A list column, containing numeric vectors of length 10}
#' }
"sim_totals"


#' Simulated reported outflows, by type
#'
#' Simulated outflows, distinguishing "internal"
#' flows (ie to other regions within the country)
#' from "external" flows (ie to other countries).
#'
#' @format A data frame with 38 rows and 3 columns:
#' \describe{
#'   \item{age}{Five-year age group.}
#'   \item{time}{2021 or 2022.}
#'   \item{internal}{Reported migration to other regions
#'     within the country.}
#'   \item{external}{Reported migration to other countries.}
#' }
"sim_reported"


#' Simulated estimates for total inflows for multiple populations
#'
#' Simulated estimates of inflows from multiple
#' populations, mimicking the results from running
#' the base model, though with only 10 iterations.
#'
#' @format A data frame with 76 rows and 4 columns:
#' \describe{
#'   \item{age}{Five-year age group.}
#'   \item{time}{2021 or 2022.}
#'   \item{region}{a or b.}
#'   \item{count}{A list column, containing numeric vectors of length 10}
#' }
"sim_totals_in"


#' Simulated estimates for total outflows for multiple populations
#'
#' Simulated estimates of outflows from multiple
#' populations, mimicking the results from running
#' the base model, though with only 10 iterations.
#'
#' @format A data frame with 76 rows and 4 columns:
#' \describe{
#'   \item{age}{Five-year age group.}
#'   \item{time}{2021 or 2022.}
#'   \item{region}{a or b.}
#'   \item{count}{A list column, containing numeric vectors of length 10}
#' }
"sim_totals_out"


#' Simulated reported flows for internal migration
#'
#' Simulated flows between two regions, mimicking
#' reported internal migration data.
#'
#' @format A data frame with 76 rows and 5 columns:
#' \describe{
#'   \item{age}{Five-year age group.}
#'   \item{time}{2021 or 2022.}
#'   \item{region_orig}{a or b.}
#'   \item{region_dest}{a or b.}
#'   \item{count}{Numeric column containing migration values.}
#' }
"sim_reported_int"


#' Simulated reported flows for external immigration
#'
#' Simulated inflows from one external region to
#' two regions in the same country, mimicking reported
#' external immigration data.
#'
#' @format A data frame with 76 rows and 5 columns:
#' \describe{
#'   \item{age}{Five-year age group.}
#'   \item{time}{2021 or 2022.}
#'   \item{region_orig}{x.}
#'   \item{region_dest}{a or b.}
#'   \item{count}{Numeric column containing inflow values.}
#' }
"sim_reported_im"


#' Simulated reported flows for external emigration
#'
#' Simulated outflows from two regions in the
#' same country to one external region, mimicking
#' reported external emigration data.
#'
#' @format A data frame with 76 rows and 5 columns:
#' \describe{
#'   \item{age}{Five-year age group.}
#'   \item{time}{2021 or 2022.}
#'   \item{region_orig}{a or b.}
#'   \item{region_dest}{x.}
#'   \item{count}{Numeric column containing outflow values.}
#' }
"sim_reported_em"
