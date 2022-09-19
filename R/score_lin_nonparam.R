#' Nonparametric score function for distance-based kernel and continuous outcome.
#'
#' Description of the nonparametric score function for distance-based kernel
#' function and continuous outcome.
#'
#' This is the main function that calculates the p-value associated with a
#' nonparametric kernel test of association between the kernel and continuous
#' outcome variable. A null model (where the kernel is not associated with the
#' outcome) is initially fit. Then, the variance of
#' \eqn{Y_{i}|X_{i}}{Y_i | X_i} is used as the basis for the
#' score test,
#' \deqn{S\left(\rho\right) = \frac{Q_{\tau}\left(\hat{\beta_0},\rho\right)-\mu_Q}{\sigma_Q}.}{S(rho) = (Q_tau(beta_0, rho) - mu_Q) / (sigma_Q).}
#' However,
#' because \eqn{\rho}{rho} disappears under the null hypothesis, we run a grid search over a range of values of \eqn{\rho}{rho} (the bounds
#' of which were derived by Liu et al. in 2008). This grid search gets the upper bound for the score test's p-value.
#' This function is tailored for the underlying model
#' \deqn{y_{i} = h\left(z_{i}\right) + e_{i},}{y_i = h(z_i) + e_i,}
#' where
#' \eqn{h\left(\cdot\right)}{h(.)} is
#' the kernel function, \eqn{z_{i}}{z_i} is a multidimensional array of variables, and \eqn{y_{i}}{y_i} is a continuous outcome taking values in
#' in the real numbers.
#'
#' The function returns an numeric p-value for the kernel score test of association.
#'
#' @param outcome a numeric vector containing the continuous outcome variable (in the same ID order as dist_mat)
#' @param dist_mat a square distance matrix
#' @param grid_gran a numeric value specifying the grid search length, preset to 5000
#'
#' @seealso \code{\link{hms}}, \code{\link{ext_distance}}, \code{\link{ham_distance}}
#' \code{\link{score_log_semiparam}} for semiparametric score function of distance-based kernel functions and binary outcome.
#' \code{\link{score_log_nonparam}} for nonparametric score function of distance-based kernel functions and binary outcome.
#' \code{\link{score_cont_semiparam}} for semiparametric score function of distance-based kernel function and continuous outcome.
#'
#' @return the score function p-value
#'
#' @references Liu D, Ghosh D, and Lin X (2008) "Estimation and testing for the effect of a
#' genetic pathway on a disease outcome using logistic kernel machine regression via
#' logistic mixed models." BMC Bioinformatics, 9(1), 292. ISSN 1471-2105.
#' \doi{10.1186/1471-2105-9-292}.
#'
#' @examples
#'
#' data(simasd_hamil_df)
#' data(simasd_covars)
#'
#' hamil_matrix <- ham_distance(simasd_hamil_df)
#'
#'\donttest{
#' score_cont_nonparam(
#'   dist_mat = hamil_matrix,
#'   outcome = simasd_covars$verbal_IQ,
#'   grid_gran = 5000
#' )
#' }
#'
#' @export
score_cont_nonparam <- function(outcome, dist_mat, grid_gran = 5000) {
    if (grid_gran <= 1) {
        stop("Need to specify a grid search length of at least 2")
    }
    if (is.vector(outcome) == FALSE) {
        stop("Outcome must be specified as a vector")
    }
    if (is.numeric(outcome) == FALSE) {
        stop("Outcome vector must be numeric")
    }
    if (nrow(dist_mat) != ncol(dist_mat)) {
        stop("The distance matrix must be a square matrix")
    }
    if (nrow(dist_mat) != length(outcome)) {
        stop("The number of rows in the distance matrix must be equal to the length of the outcome vector")
    }

    n <- ncol(dist_mat)
    y <- outcome
    null_mod <- stats::glm(y ~ 1, family = "gaussian")  #Fitting the null model without the kernel
    pred_null <- stats::predict(null_mod, type = "response")  #Pulls the predicted values from the logistic regression

    D_0 <- diag((y - pred_null)^2)
    X <- as.matrix(stats::model.matrix(null_mod))
    P_0 <- D_0 - D_0 %*% X %*% solve(t(X) %*% D_0 %*% X) %*% t(X) %*% D_0

    bounds <- up_low(dist_mat)
    U <- max(bounds) * 100
    L <- min(bounds[bounds > 0]) * 0.1

    rho <- seq(L, U, length = grid_gran)  #Grid of rho values for kernel
    S <- rep(0, grid_gran)  #Row vector of zeros of grid length
    for (i in 1:grid_gran) {
        k <- kernel(dist_mat, rho[i])
        Q <- t(y - pred_null) %*% k %*% (y - pred_null)  #Test statistic
        mu <- tr(P_0 %*% k)
        sigma <- sqrt(2 * tr(P_0 %*% k %*% P_0 %*% k))
        S[i] <- (Q - mu)/sigma  #Standardized version of test statistic for each value of kernel
    }
    M <- max(S)  #Max value of standardized test statistic
    W <- 0
    for (j in 1:(grid_gran - 1)) {
        W <- W + abs(S[j + 1] - S[j])  #Total variation of S in grid
    }
    p_value <- stats::pnorm(-M) + W * exp(-M^2/2)/sqrt(8 * pi)  #(12) in Liu et al. (2008)
    return(p_value)
}
