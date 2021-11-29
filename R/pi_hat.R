pi_hat <- function(data,
                   cov_mat = NULL,
                   mat_pairwise_obs = NULL,
                   na.rm = TRUE,
                   use = NULL) {

  # Center dataset
  data_demeaned <- scale(data, TRUE, FALSE)

  mat_pi_hat <- matrix(nrow = ncol(data), ncol = ncol(data))

  # Compute row wise products
  mat_cov_single <- row_wise_full_product(data_demeaned)

  # Subtract covariance entries

}
