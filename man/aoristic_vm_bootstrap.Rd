% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap_aovonmises.R
\name{aoristic_vm_bootstrap}
\alias{aoristic_vm_bootstrap}
\title{Obtain bootstrap standard errors for aoristic von Mises MLE's}
\usage{
aoristic_vm_bootstrap(data, R = 1000L, probs = c(0.025, 0.975),
  kp_max = 100, tol = 0.01, ...)
}
\arguments{
\item{data}{Aoristic Data with columns t_start and t_end.}

\item{R}{Number of bootstrap samples.}

\item{probs}{Confidence interval probabilities.}

\item{kp_max}{Maximum possible value of concentration parameter kappa to
consider in the optimization.}

\item{tol}{Tolerance of the optimization.}

\item{...}{Further arguments to \code{boot}, such as arguments to enable
parallel computation.}
}
\value{
An object of type \code{aovmboot} containing a table of results, the
  bootstrap sample, and the original \code{boot} object.
}
\description{
Obtain bootstrap standard errors for aoristic von Mises MLE's
}
\examples{
dat <- generateAoristicData()
aoristic_vm_bootstrap(dat, R = 4)
}
