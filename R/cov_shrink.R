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
#' @details The estimator computes a weighted mean of two covariance estimators.
#'    The unstructured estimator is the sample covariance matrix denoted as
#'    \eqn{S} and a structured estimator dentoed as \eqn{F}. The returned
#'    covariance matrix is computed by the following equation
#'    \deqn{\delta F + (1 - \delta) S}
#'    with \eqn{\delta} denoting the weighting term taking values between
#'    \eqn{0} and \eqn{1}.
#'
#'    The structured estimator contains the following entries
#'    \deqn{
#'    f_{ij} = s_{ij}^2 \qquad i = j \\
#'    f_{ij} = \bar{r} s_{ii} s_{jj}
#'    }
#'
#'    In case missing values are present setting \code{use} to
#'    \code{"pairwise.complete.obs"} and \code{na.rm} to \code{TRUE} estimates
#'    the sample covariance matrix by pairwise deletion. To account for the
#'    differing le
#'
#' @return list containing the shrinked covariance matrix and the optimal
#'    shrinkage parameter \eqn{\delta}.
#'
#' @references  Ledoit, Olivier and Wolf, Michael, Honey, I Shrunk the Sample
#'    Covariance Matrix (June 2003). UPF Economics and Business Working Paper
#'    No. 691.
#'
#' @export
#'
#' @examples
#' library(MASS)
#' library(matrixcalc)
#'
#' set.seed(30)
#' int_p <- 3
#' int_N <- 5
#' vec_mu <- rnorm(int_p)
#' mat_A <- matrix(runif(int_p^2) - 1, nrow = 3)
#' mat_Sigma <- cov2cor(t(mat_A) %*% mat_A)
#' X <- mvrnorm(int_N, vec_mu, mat_Sigma)
#'
#' cov_shrink(X, )
cov_shrink <- function(data, na.rm = FALSE, use = "everything") {

  # Check that all elements are of type numeric. If not stop
  lgl_numeric <- data %>% purrr::map_lgl(~ is.numeric(.x)) %>% all()
  if (!lgl_numeric) {
    stop("'data' has to contain only numeric variables", call. = FALSE)
  }

  # Transform data to matrix then
  if (any(class(data) %in% c("tbl_df", "tbl", "data.frame", "data.table"))) {
    data <- as.matrix(data)
  }

  # Depending on use value compute whole matrix
  mat_covariance <- stats::cov(data, use = use)

  if (use == "pairwise.complete.obs") {
    mat_pairwise_obs <- pairwise_obs(data)
  } else {
    mat_pairwise_obs <- NULL
  }

  # Compute structured estimator
  list_structured <- structured_cov(mat_covariance, na.rm, mat_pairwise_obs)
  mat_structured <- list_structured$structured
  dbl_r_bar <- list_structured$r_bar

  # Compute gamma hat
  dbl_gamma_hat <- gamma_hat(mat_covariance, mat_structured, na.rm)

  # Compute pi and nu hat
  list_pi_nu_hat <- pi_nu_hat(
    data,
    mat_covariance,
    mat_pairwise_obs,
    na.rm
  )

  mat_pi_hat <- list_pi_nu_hat$pi_hat
  dbl_pi_hat <- sum(mat_pi_hat, na.rm = na.rm)

  # Compute rho hat
  mat_nu_hat_ii <- (dbl_r_bar / 2) * list_pi_nu_hat$nu_hat_ii
  mat_nu_hat_jj <- (dbl_r_bar / 2) * list_pi_nu_hat$nu_hat_jj

  # Rho hat is not computed from diagonal entries of nu
  diag(mat_nu_hat_ii) <- 0
  diag(mat_nu_hat_jj) <- 0

  dbl_rho_hat <- dbl_pi_hat + sum(mat_nu_hat_ii + mat_nu_hat_jj, na.rm = na.rm)

  # Compute optimal delta
  dbl_kappa_hat <- (dbl_pi_hat - dbl_rho_hat) / dbl_gamma_hat

  if (use == "pairwise.complete.obs") {
    mat_intermediate <- dbl_kappa_hat /mat_pairwise_obs
    delta_hat <- pmax(0, pmin(mat_intermediate, 1))
  } else {
    int_T <- nrow(data)
    delta_hat <- max(0, min(dbl_kappa_hat / int_T, 1))
  }

  # Compute shrinked estimator
  mat_cov_shrinked <- delta_hat * mat_structured +
    (1 - delta_hat) * mat_covariance

  # Return delta_hat, covariance estimate
  list(cov = mat_cov_shrinked, delta = delta_hat)

}
