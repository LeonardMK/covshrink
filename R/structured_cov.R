#' Estimate of the structured covariance matrix
#'
#' @param mat_covariance matrix containing sample covariance estimates.
#' @param na.rm logical value indicating whether \code{NA} values should be
#'    stripped before the computation proceeds.
#' @param mat_obs_weight matrix of pairwise observations. If provided an
#'    observations weighted average of the correlation coefficients is
#'    computed giving higher weight to coefficients computed from more data.
#'
#' @return matrix containing the \eqn{\bar{\rho} \times s_{i} \times s_{j}} on
#'    for variable \eqn{i} and \eqn{j} on the nondiagonal and \eqn{s_i^2} on
#'    the diagonal.
structured_cov <- function(mat_covariance, na.rm = FALSE, mat_obs_weight = NULL) {

  mat_correlation <- stats::cov2cor(mat_covariance)
  vec_cor <- as.vector(mat_correlation[lower.tri(mat_correlation)])
  vec_stdev <- sqrt(diag(mat_covariance))

  int_nrow <- nrow(mat_covariance)

  # Calculate a weighted mean if mat_obs_weight is given
  if (purrr::is_null(mat_obs_weight)) {
    dbl_r_bar <- mean(vec_cor, na.rm = na.rm)
  } else {

    int_sum_obs_nodiag <- sum(mat_obs_weight[lower.tri(mat_obs_weight)],
                              na.rm = na.rm)
    mat_weight <- mat_obs_weight / int_sum_obs_nodiag
    vec_weights <- as.vector(mat_weight[lower.tri(mat_weight)])
    dbl_r_bar <- sum(vec_weights * vec_cor, na.rm = na.rm)

  }

  mat_structured <- dbl_r_bar * outer(vec_stdev, vec_stdev)
  diag(mat_structured) <- vec_stdev^2

  list(structured = mat_structured, r_bar = dbl_r_bar)

}
