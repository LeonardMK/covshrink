test_that("rowwise full product", {

  mat_X <- matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE)

  mat_fp <- row_wise_full_product(mat_X)
  dimnames(mat_fp) <- NULL

  mat_fp_true <- array(
    c(1, 16, 2, 20, 3, 24, 2, 20, 4, 25, 6, 30, 3, 24, 6, 30, 9, 36),
    dim = c(2, 3, 3)
      )

  expect_equal(mat_fp, mat_fp_true)

})
