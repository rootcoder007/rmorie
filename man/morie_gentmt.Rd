% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gentmt}
\alias{morie_gentmt}
\title{morie_gentmt}
\usage{
morie_gentmt(y, A, H, method = "weight", degree = 1, n_strata = 5, doses = NULL, trim = NULL)
}
\arguments{
\item{y}{See Usage.}
\item{A}{See Usage.}
\item{H}{See Usage.}
\item{method}{Defaults to \code{"weight"}.}
\item{degree}{Defaults to \code{1}.}
\item{n_strata}{Defaults to \code{5}.}
\item{doses}{Defaults to \code{NULL}.}
\item{trim}{Defaults to \code{NULL}.}
}
\description{
Part of the gentmt_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
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
morie_gentmt(y = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8), H = c(1, 2, 3, 4, 5, 6, 7, 8))
}
