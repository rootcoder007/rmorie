% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gentmt_dose_response_curve}
\alias{.gentmt_dose_response_curve}
\title{.gentmt_dose_response_curve}
\usage{
.gentmt_dose_response_curve(y, A, H, doses = NULL, degree = 1)
}
\arguments{
\item{y}{Passed to \code{.gentmt_vec}.}
\item{A}{Passed to \code{.gentmt_vec}.}
\item{H}{Passed to \code{.gentmt_treatment_density}.}
\item{doses}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{degree}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
}
\description{
A step of the gentmt_native implementation. Called by \code{morie_gentmt}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{se}, \code{doses}, \code{curve}, \code{slopes}, \code{coef}, \code{gps}, \code{n}, \code{degree}.
}
\references{
  Robins, J. M., Hernan, M. A. & Brumback, B. (2000) "Marginal structural
    models and causal inference in epidemiology", Epidemiology 11(5), 550-560.
  Imai, K. & van Dyk, D. A. (2004) "Causal inference with general treatment
    regimes: generalizing the propensity score", JASA 99(467), 854-866.
  Hirano, K. & Imbens, G. W. (2004) "The propensity score with continuous
    treatments", in Applied Bayesian Modeling and Causal Inference from
    Incomplete-Data Perspectives, Wiley, 73-84.
  Hernan M. A. & Robins J. M. (2020) Causal Inference: What If, Sec. 12.3.
}
\keyword{internal}
