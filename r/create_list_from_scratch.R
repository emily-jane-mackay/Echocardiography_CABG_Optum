## ---------------------------
##
## create_list_from_scratch function
##
## Author: Bo Zhang
##
##
## ---------------------------
##
## Notes: This file includes code from the create_list_from_scratch
## function from R package match2C.
##   
## ---------------------------

create_list_from_scratch <- function (Z, X, exact = NULL, soft_exact = FALSE, p = NULL, caliper_low = NULL, 
          caliper_high = NULL, k = NULL, alpha = 1, penalty = Inf, 
          method = "maha", dist_func = NULL) 
{
  if (is.null(k)) 
    k = length(Z) - sum(Z)
  if (is.vector(X)) 
    X = matrix(X, ncol = 1)
  if (method == "maha") {
    cat('maha', '\n')
    cov_matrix = chol(stats::cov(X))
    compute_maha_dist <- function(X_control, X_treated_i) {
      return(mvnfast::maha(X_control, t(as.matrix(X_treated_i)), 
                           cov_matrix, isChol = TRUE))
    }
    output = create_list_from_scratch_overall(Z, X, exact, 
                                              soft_exact, p, caliper_low, caliper_high, k, penalty, 
                                              dist_func = compute_maha_dist)
  }
  if (method == "Hamming") {
    compute_hamming_dist <- function(X_control, X_treated_i) {
      return(ncol(X_control) - rowSums(sweep(X_control, 
                                             2, as.matrix(X_treated_i)) == 0))
    }
    output = create_list_from_scratch_overall(Z, X, exact, 
                                              soft_exact, p, caliper_low, caliper_high, k, penalty, 
                                              dist_func = compute_hamming_dist)
  }
  if (method == "L1") {
    compute_L1_dist <- function(X_control, X_treated_i) {
      return(rowSums(abs(sweep(X_control, 2, as.matrix(X_treated_i)))))
    }
    output = create_list_from_scratch_overall(Z, X, exact, 
                                              soft_exact, p, caliper_low, caliper_high, k, penalty, 
                                              dist_func = compute_L1_dist)
  }
  if (method == "L1_convex") {
    compute_L1_convex_dist <- function(X_control, X_treated_i) {
      return(alpha * (-rowSums(sweep(X_control, 2, as.matrix(X_treated_i)))) - 
               0)
    }
    output = create_list_from_scratch_overall(Z, X, exact, 
                                              soft_exact, p, caliper_low, caliper_high, k, penalty, 
                                              dist_func = compute_L1_convex_dist)
  }
  if (method == "vanilla_directional") {
    compute_vanilla_dir_dist <- function(X_control, X_treated_i) {
      return(alpha * (-rowSums(sweep(X_control, 2, as.matrix(X_treated_i))) - 
                        0))
    }
    output = create_list_from_scratch_overall(Z, X, exact, 
                                              soft_exact, p, caliper_low, caliper_high, k, penalty, 
                                              dist_func = compute_vanilla_dir_dist)
  }
  if (method == "hockey_stick") {
    compute_hockey_stick_dist <- function(X_control, X_treated_i) {
      d_1 = pmax(-rowSums(sweep(X_control, 2, as.matrix(X_treated_i))), 
                 0)
      return(alpha * (d_1 - 0.01))
    }
    output = create_list_from_scratch_overall(Z, X, exact, 
                                              soft_exact, p, caliper_low, caliper_high, k, penalty, 
                                              dist_func = compute_hockey_stick_dist)
  }
  if (method == "0/1/directional") {
    compute_0_1_dir_dist <- function(X_control, X_treated_i) {
      d_1 = (((-rowSums(sweep(X_control, 2, as.matrix(X_treated_i)))) > 
                0) + 0)
      return(alpha * (d_1 - 0.01))
    }
    output = create_list_from_scratch_overall(Z, X, exact, 
                                              soft_exact, p, caliper_low, caliper_high, k, penalty, 
                                              dist_func = compute_0_1_dir_dist)
  }
  if (method == "0/1") {
    compute_01_dist <- function(X_control, X_treated_i) {
      return(1 - (rowSums(sweep(X_control, 2, X_treated_i) == 
                            0) == dim(X_control)[2]))
    }
    output = create_list_from_scratch_overall(Z, X, exact, 
                                              soft_exact, p, caliper_low, caliper_high, k, penalty, 
                                              dist_func = compute_01_dist)
  }
  if (method == "robust maha") {
    if (is.vector(X)) 
      X = matrix(X, ncol = 1)
    X <- as.matrix(X)
    n <- dim(X)[1]
    rownames(X) <- 1:n
    for (j in 1:dim(X)[2]) X[, j] <- rank(X[, j])
    cv <- stats::cov(X)
    vuntied <- stats::var(1:n)
    rat <- sqrt(vuntied/diag(cv))
    cv <- diag(rat) %*% cv %*% diag(rat)
    cov_matrix = chol(cv)
    compute_maha_dist <- function(X_control, X_treated_i) {
      return(mvnfast::maha(X_control, t(as.matrix(X_treated_i)), 
                           cov_matrix, isChol = TRUE))
    }
    output = create_list_from_scratch_overall(Z, X, exact, 
                                              soft_exact, p, caliper_low, caliper_high, k, penalty, 
                                              dist_func = compute_maha_dist)
  }
  if (method == "other") 
    output = create_list_from_scratch_overall(Z, X, exact, 
                                              soft_exact, p, caliper_low, caliper_high, k, penalty, 
                                              dist_func = dist_func)
  if (is.character(output)) {
    cat("Hard caliper fails. Please specify a soft caliper.", 
        "\n")
    return(NA)
  }
  else {
    start_n = output[[1]]
    end_n = output[[2]]
    d = output[[3]]
    return(list(start_n = unname(start_n), end_n = unname(end_n), 
                d = unname(d)))
  }
}


