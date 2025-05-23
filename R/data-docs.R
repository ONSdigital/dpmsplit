#' Simulated estimates for total outflows
#'
#' Simulated estimates of outflows from a population,
#' mimicking the results from running the base model,
#' though with only 10 interations.
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
