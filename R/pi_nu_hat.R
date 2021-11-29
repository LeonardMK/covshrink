#' Estimator of pi_ij and nu_ij
#'
#' @param data matrix or data.frame containing only numeric variables
#' @param mat_cov matrix containing sample covariance estimates.
#' @param mat_obs matrix of pairwise observations.
#' @param na.rm logical value indicating whether \code{NA} values should be
#'    stripped before the computation proceeds.
#'
#' @return matrix of individual estimates for \eqn{\pi_{ij}} and the entries
#'    \eqn{\sqrt{s_{j}^2 / s_{i}^2} \times \nu_{ii, ij}} and
#'    \eqn{\sqrt{s_{i}^2 / s_{j}^2} \times \nu_{jj, ij}}.
pi_nu_hat <- function(data,
                       mat_cov,
                       mat_obs,
                       na.rm = FALSE) {

  # Demean data
  mat_data_demeaned <- scale(data, TRUE, FALSE)

  # Create index to iterate over
  int_nrow_mat_cov <- nrow(mat_cov)
  vec_index_i <- 1:int_nrow_mat_cov

  # Create empty matrices to store results
  mat_pi_hat <- matrix(nrow = int_nrow_mat_cov, col = int_nrow_mat_cov)
  mat_nu_hat_ii <- matrix(nrow = int_nrow_mat_cov, col = int_nrow_mat_cov)
  mat_nu_hat_jj <- matrix(nrow = int_nrow_mat_cov, col = int_nrow_mat_cov)

  # Calculate pi and rho in a single step
  for(int_i in vec_index_i){

    vec_index_j <- vec_index_i[vec_index_i >= int_i]

    for(int_j in vec_index_j){

      # Calculate pi hat
      dbl_s_ij <- mat_cov[int_i, int_j]
      vec_y_it <- mat_data_demeaned[, int_i]
      vec_y_jt <- mat_data_demeaned[, int_j]

      dbl_pi_hat_ij <- mean((vec_y_it * vec_y_jt - dbl_s_ij)^2, na.rm = na.rm)
      mat_pi_hat[int_i, int_j] <- dbl_pi_hat_ij

      # Calculate rho hat
      dbl_s_ii <- mat_cov[int_i, int_i]
      dbl_s_jj <- mat_cov[int_j, int_j]
      vec_nu_hat_i <- (vec_y_it^2 - dbl_s_ii) * (vec_y_it * vec_y_jt - dbl_s_ij)
      vec_nu_hat_j <- (vec_y_jt^2 - dbl_s_jj) * (vec_y_it * vec_y_jt - dbl_s_ij)
      dbl_nu_hat_ii <- mean(vec_nu_hat_i, na.rm = na.rm)
      dbl_nu_hat_jj <- mean(vec_nu_hat_j, na.rm = na.rm)

      mat_nu_hat_ii[int_i, int_j] <- sqrt(dbl_s_jj / dbl_s_ii) * dbl_nu_hat_ii
      mat_nu_hat_jj[int_i, int_j] <- sqrt(dbl_s_ii / dbl_s_jj) * dbl_nu_hat_jj

    }

  }

  mat_pi_hat[upper.tri(mat_pi_hat)] <- mat_pi_hat[lower.tri(mat_pi_hat)]
  mat_nu_hat_ii[upper.tri(mat_nu_hat_ii)] <- mat_nu_hat_ii[lower.tri(mat_nu_hat_ii)]
  mat_nu_hat_jj[upper.tri(mat_nu_hat_jj)] <- mat_nu_hat_jj[lower.tri(mat_nu_hat_jj)]

  list(pi_hat = mat_pi_hat, nu_hat_ii = mat_nu_hat_ii, nu_hat_jj = mat_nu_hat_jj)

}
