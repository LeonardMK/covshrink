#' Shrinked Covariance Matrix
#'
#' This function computes the shrinked covariance matrix from the sample
#'   covariance matrix and a structured estimator. The optimal shrinkage
#'   intensity is estimated too.
#'
#' @usage
#' cov_shrink(data, na.rm = FALSE, use = "everything")
#'
#' @param data matrix or data.frame containing only numeric variables
#' @param use character string as in \code{\link[stats]{cov}}. Giving a method
#'    for computing covariances in the presence of missing values.This must be
#'    (an abbreviation of) one of the strings "everything", "all.obs",
#'    "complete.obs", "na.or.complete", or "pairwise.complete.obs".
#' @param na.rm matrix or data.frame containing only numeric variables
#'
#' @details
#'
#' @return
#'
#' @references  Ledoit, Olivier and Wolf, Michael, Honey, I Shrunk the Sample
#'    Covariance Matrix (June 2003). UPF Economics and Business Working Paper
#'    No. 691.
#'
#' @export
#'
#' @examples
cov_shrink <- function(data, na.rm = FALSE, use = "everything") {

  # Check that all elements are of type numeric. If not stop
  # if ()

  # Transform data to matrix then

  # get column names and if not

  # Depending on use value compute whole matrix

  # Compute optimal delta

  # Compute structured estimator

  # Compute shrinked estimator

  # Return delta_hat, covariance estimate

}
