test_that("Calculating the number of pairwise complete observations", {

  int_N <- 10
  x <- rnorm(int_N)
  y <- rnorm(int_N)
  z <- x + y

  mat_data <- cbind(x, y, z)

  mat_data_incomplete <- mat_data
  mat_data_incomplete[1:2, 1] <- NA
  mat_data_incomplete[2:5, 2] <- NA
  mat_data_incomplete[c(2, 7:9), 3] <- NA
  mat_data_obs <- pairwise_obs(mat_data_incomplete)

  mat_data_obs_correct <- matrix(
    c(10, 5, 5, 5, 10, 3, 5, 3, 10),
    nrow = 3,
    ncol = 3
  )

  # Test that the matrix is of correct dimension
  expect_equal(dim(mat_data_obs), c(3, 3))

  # Test that all entries are integers
  expect_true({
    mat_data_obs %>%
      as.vector() %>%
      map_lgl(~ is_integer(.x)) %>%
      all()
    })

  # Test that the matrix is equal
  expect_equal(mat_data_obs, mat_data_obs_correct)

})
