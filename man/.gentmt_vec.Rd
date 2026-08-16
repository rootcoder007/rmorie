% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gentmt_vec}
\alias{.gentmt_vec}
\title{.gentmt_vec}
\usage{
.gentmt_vec(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
A step of the gentmt_native implementation. Called by \code{.gentmt_dose_response_curve}, \code{.gentmt_gps_subclassify}, \code{.gentmt_treatment_density} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
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
