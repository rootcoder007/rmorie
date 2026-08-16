% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gentmt_treatment_density}
\alias{.gentmt_treatment_density}
\title{.gentmt_treatment_density}
\usage{
.gentmt_treatment_density(A, H, kind = "normal")
}
\arguments{
\item{A}{Passed to \code{.gentmt_vec}.}
\item{H}{A matrix; passed to \code{as.matrix}.}
\item{kind}{Accepted by the signature and not used anywhere in the body. Defaults to \code{"normal"}.}
}
\description{
A step of the gentmt_native implementation. Called by \code{.gentmt_dose_response_curve}, \code{.gentmt_gps_subclassify}, \code{.gentmt_ip_weights}. See the file header for the source the module follows.
}
\value{
A list with \code{dens}, \code{info}.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.gentmt_treatment_density(V, V)
}
