test_that("Calculation of structured covariance estimator", {

  mat_cov <- matrix(c(1, 0.5, 1, 0.5, 4, 2, 1, 2, 9), nrow = 3, ncol = 3)

  mat_obs <- matrix(c(10, 9, 8, 8, 11, 10, 9, 10, 11), nrow = 3, ncol = 3)
  vec_weights <- c(9, 8, 10) / (9 + 8 + 10)

  rho_bar_true <- (0.5 / 2 + 1 / 3 + 2 / 6) / 3
  rho_bar_weighted_true <- as.vector(c(0.5 / 2, 1 / 3, 2 / 6) %*% vec_weights)

  mat_structured_cov_true <- matrix(
    c(1, 2, 3, 2, 4, 6, 3, 6, 9),
    nrow = 3,
    ncol = 3
    )

  mat_structured_cov_weighted_true <- matrix(
    c(1, 2, 3, 2, 4, 6, 3, 6, 9),
    nrow = 3,
    ncol = 3
  )

  mat_structured_cov_true <- rho_bar_true * mat_structured_cov_true
  mat_structured_cov_weighted_true <- (rho_bar_weighted_true *
                                         mat_structured_cov_weighted_true)

  diag(mat_structured_cov_true) <- c(1, 4, 9)
  diag(mat_structured_cov_weighted_true) <- c(1, 4, 9)

  mat_structured_cov <- structured_cov(mat_cov, TRUE, NULL)
  mat_structured_cov_weighted <- structured_cov(mat_cov, TRUE, mat_obs)

  expect_equal(mat_structured_cov, mat_structured_cov_true)
  expect_equal(mat_structured_cov_weighted, mat_structured_cov_weighted_true)

})
