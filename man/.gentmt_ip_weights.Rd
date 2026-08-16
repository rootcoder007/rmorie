% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gentmt_ip_weights}
\alias{.gentmt_ip_weights}
\title{.gentmt_ip_weights}
\usage{
.gentmt_ip_weights(A, H, kind = "normal", stabilize = TRUE, trim = NULL)
}
\arguments{
\item{A}{Numeric; passed to \code{mean}.}
\item{H}{Passed to \code{.gentmt_treatment_density}.}
\item{kind}{Passed to \code{.gentmt_treatment_density}. Defaults to \code{"normal"}.}
\item{stabilize}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{trim}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the gentmt_native implementation. Called by \code{morie_gentmt}. See the file header for the source the module follows.
}
\value{
A list with \code{w}, \code{info}.
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
.gentmt_ip_weights(V, V)
}
