#' Estimate of gamma
#'
#' @param mat_covariance matrix containing sample covariance matrix.
#' @param mat_structurded matrix containing structured covariance matrix.
#' @param na.rm logical indicating whether \code{NA} values should be stripped
#'    before the computation proceeds.
#'
#' @return matrix of shape \eqn{p \times p}.
gamma_hat <- function(mat_covariance, mat_structurded, na.rm) {

  mat_cov_diff_sq <- (mat_covariance - mat_structurded)^2

  sum(mat_cov_diff_sq, na.rm = na.rm)

}
