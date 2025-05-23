#include <Rcpp.h>
using namespace Rcpp;

// HAS_TESTS
//' Iterative proportional fitting
//'
//' Use iterative proportional fitting to
//' adjust \code{m} so that it is consistent
//' with \code{rowsums} and \code{colsums}.
//' \code{rowsums} and \code{colsums} can
//' include \code{NA}s, which indicate that
//' the associated row/column is not constrained.
//'
//' \code{m} is assumed to have no NAs or
//' negative values. Zeros are OK.
//'
//' \code{rowsums} and \code{colsums} are assumed
//' to have no negative values. NAs and zeros
//' are OK.
//'
//' @param m A matrix of doubles (not necessarily square)
//' @param rowsums A vector of doubles.
//' @param colsums A vector of doubles.
//' @param max_iter Maximum number of iterations.
//' @param tolerance Maximum absolute difference
//' between supplied values for rowsums/colsums
//' and derived values.
//'
//' @return A matrix with the same dimensions as \code{m}.
//'
//' @noRd
// [[Rcpp::export]]
NumericMatrix ipf(NumericMatrix m,
                  NumericVector rowsums,
                  NumericVector colsums,
                  int max_iter,
                  double tolerance) {
  NumericMatrix m_working = clone(m);
  int I = m_working.nrow();
  int J = m_working.ncol();
  NumericVector rowsums_current = rep(0.0, I);
  LogicalVector is_row_within_tolerance = rep(true, I);
  LogicalVector is_rowsum_non_na = !is_na(rowsums);
  LogicalVector is_colsum_non_na = !is_na(colsums);
  LogicalVector is_rowsum_pos = is_rowsum_non_na & (rowsums > 0);
  LogicalVector is_colsum_pos = is_colsum_non_na & (colsums > 0);

  // calculate current rowsums, and also check whether we are within tolerance
  for (int i = 0; i < I; i++) {
    if (is_rowsum_non_na[i] & is_rowsum_pos[i]) {
      double rowsum_target = rowsums[i];
      double rowsum_current = sum(m_working(i, _));
      rowsums_current[i] = rowsum_current;
      is_row_within_tolerance[i] = std::abs(rowsum_current - rowsum_target) < tolerance;
    }
  }

  // rescale rows/columns where rowsum/colsum equal to zero
  for (int i = 0; i < I; i++) {
    if (is_rowsum_non_na[i] & !is_rowsum_pos[i]) {
      m_working(i, _) = rep(0.0, J);
    }
  }

  for (int j = 0; j < J; j++) {
    if (is_colsum_non_na[j] & !is_colsum_pos[j]) {
      m_working(_, j) = rep(0.0, I);
    }
  }

  // main IPF loop
  for (int iter = 0; iter < max_iter; iter++) {
    // scale the rows
    for (int i = 0; i < I; i++) {
      if (is_rowsum_non_na[i] & is_rowsum_pos[i]) {
        double rowsum_target = rowsums[i];
        double rowsum_current = rowsums_current[i];
        double multiplier = rowsum_target / rowsum_current;
        m_working(i, _) = multiplier * m_working(i, _);
      }
    }

    // scale the columns
    for (int j = 0; j < J; j++) {
      if (is_colsum_non_na[j] & is_colsum_pos[j]) {
        double colsum_target = colsums[j];
        double colsum_current = sum(m_working(_, j));
        double multiplier = colsum_target / colsum_current;
        m_working(_, j) = multiplier * m_working(_, j);
      }
    }

    // calculate current rowsums, and also check
    // whether we are within tolerance
    for (int i = 0; i < I; i++) {
      if (is_rowsum_non_na[i] & is_rowsum_pos[i]) {
        double rowsum_target = rowsums[i];
        double rowsum_current = sum(m_working(i, _));
        rowsums_current[i] = rowsum_current;
        is_row_within_tolerance[i] = std::abs(rowsum_current - rowsum_target) < tolerance;
      }
    }
    // check if we are within tolerances, and return the answer if we are
    if (is_true(all(is_row_within_tolerance)))
      return(m_working);

  }
  // completed 'max_iter' iterations without finding answer
  Rcpp::Rcout << "rowsums:" << std::endl << rowsums << std::endl << std::endl;
  Rcpp::Rcout << "colsums:" << std::endl << colsums << std::endl << std::endl;
  Rcpp::Rcout << "current estimates for flows:" << std::endl << m_working << std::endl << std::endl;
  stop("IPF failed to converge after %d iterations", max_iter);
}

