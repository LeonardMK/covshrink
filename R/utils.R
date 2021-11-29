#' Title
#'
#' @param data Dataframe or matrix of numeric values
#'
#' @return Array of rowwise full products
#'
row_wise_full_product <- function(data) {

  int_ncol <- ncol(data)

  vec_index <- data %>%
    nrow() %>%
    seq_len()

  list_fp <- vec_index %>%
    purrr::map(~ {
      vec_fp <- data[.x, ] %x% data[.x, ]

      array(vec_fp, dim = c(1, int_ncol, int_ncol))

    })

  do.call(abind::abind, list(list_fp, along = 1))

}
