test_that("Test of gamma_hat", {

  mat_cov <- matrix(c(4, 3, 4, 3, 9, 6, 4, 6, 16), nrow = 3, ncol = 3)

  rho_bar_true <- (3 / 6 + 4 / 8 + 6 / 12) / 3

  mat_structured <- matrix(
    c(4, 3, 4, 3, 9, 6, 4, 6, 16),
    nrow = 3,
    ncol = 3
  )

  dbl_gamma_hat_true <- 0
  dbl_gamma_hat <- gamma_hat(mat_cov, mat_structured, TRUE)

  expect_equal(dbl_gamma_hat, dbl_gamma_hat_true)

})
