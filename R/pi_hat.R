pi_hat <- function(data,
                   cov_mat = NULL,
                   mat_pairwise_obs = NULL,
                   na.rm = TRUE,
                   use = NULL) {

  if (!is.matrix(data)) data <- as.matrix(data)

  if (is_null(cov_mat)) {
    vec_s_ij <- cov_mat
  } else {
    if (is_null(use)) use <- "everything"
    vec_s_ij <- cov(data, use = use)
  }

  # Center dataset
  data_demeaned <- scale(data, TRUE, FALSE)

}
