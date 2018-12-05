# ao mle function prepared for the boot package.
ao_vm_mle_bootver <- function(data, inds, kp_max, ...) {
  aoristic_vm_mle(data[inds, ], kp_max,  ...)
}


print.aovmboot <- function(object, ...) print(object$tab, ...)

# Quick convenience functions for circular sd and means
force_neg_pi_pi  <- function(x) ((x + pi) %% (2*pi)) - pi
resultant_length <- function(x) sqrt(sum(sin(x))^2 + sum(cos(x))^2)/length(x)
circ_mean        <- function(x) atan2(sum(sin(x)), sum(cos(x)))
circ_var         <- function(x) 1 - resultant_length(x)
circ_sd          <- function(x) sqrt(-2 * log(resultant_length(x)))
circ_quantile    <- function(x, ...) {
  # The desired rotation before taking the quantile.
  rotation <- circ_mean(x)
  # Center the data, and move it as far away from 0 radians as possible by
  # th+rot. Then, apply the quantile function, and rotate back.
  force_neg_pi_pi(quantile(force_neg_pi_pi(x - rotation), ...) + rotation)
}




#' Obtain bootstrap standard errors for aoristic von Mises MLE's
#'
#' @param data Aoristic Data with columns t_start and t_end.
#' @param R Number of bootstrap samples.
#' @param probs Confidence interval probabilities.
#' @param kp_max Maximum possible value of concentration parameter kappa to
#'   consider in the optimization.
#' @param tol Tolerance of the optimization.
#' @param ... Further arguments to \code{boot}, such as arguments to enable
#'   parallel computation.
#'
#' @return An object of type \cpde{aovmboot} containing a table of results, the
#'   bootstrap sample, and the original \code{boot} object.
#' @export
#'
#' @examples
#' dat <- generateAoristicData()
#' aoristic_vm_bootstrap(dat, R = 4)
aoristic_vm_bootstrap <- function(data, R = 1000L, probs = c(.025, .975), kp_max = 100, tol =.01, ...) {

  # Compute the bootstrap using the boot package.
  suppressWarnings({
    aovmleboot <- boot::boot(data, ao_vm_mle_bootver, R = R, kp_max = kp_max, tol = tol, ...)
  })

  # Obtain the p-values for t0 > 0, which will be taken as 1 - p for t0 < 0.
  ps <- apply(aovmleboot$t, 2L, function(x) mean(x <= 0))

  # Obtain result
  mus <- aovmleboot$t[, 1]
  kps <- aovmleboot$t[, 2]
  boot_est <- c(circ_mean(mus), mean(kps))
  res <- list(tab = cbind(original        = aovmleboot$t0,
                          "boot estimate" = boot_est,
                          bias            = boot_est - aovmleboot$t0,
                          "std. error"    = c(circ_sd(mus), sd(kps)),
                          rbind(circ_quantile(mus, probs = probs), quantile(kps, probs = probs)),
                          "one-sided p-value"    = ifelse(aovmleboot$t0 > 0, ps, 1 - ps),
                          "two-sided p-value"    = ifelse(aovmleboot$t0 > 0, ps*2, (1 - ps) * 2 )),
              bootsam = aovmleboot$t,
              bootobj = aovmleboot)

  class(res) <- c("aovmboot", class(res))
  res
}

