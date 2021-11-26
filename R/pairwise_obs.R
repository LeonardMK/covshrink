#' Compute the number of pairwise complete observations
#'
#' @param data dataframe or matrix
#'
#' @return matrix of integers giving the number of pairwise complete observations.
pairwise_obs <- function(data) {

  vec_index <- 1:ncol(data)

  mat_pairwise_obs <- matrix(nrow = ncol(data), ncol = ncol(data))

  # Matrix is symmetric compute only lower triangle including diagonal
  for(index_row in vec_index){

    vec_index_col <- vec_index[vec_index >= index_row]

    for(index_col in vec_index_col){

      if (index_row == index_col) {
        int_obs <- sum(!is.na(data[, index_row]))
      } else {

        mat_sub <- data[, c(index_row, index_col)]
        mat_sub <- !is.na(mat_sub)
        int_obs <- sum(rowSums(mat_sub) == 2)

      }

      mat_pairwise_obs[index_row, index_col] <- int_obs

    }

  }

  mat_pairwise_obs[lower.tri(mat_pairwise_obs)] <- (
    mat_pairwise_obs[upper.tri(mat_pairwise_obs)]
    )

  mat_pairwise_obs

}
