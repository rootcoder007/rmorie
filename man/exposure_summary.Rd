% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{exposure_summary}
\alias{exposure_summary}
\title{exposure_summary}
\usage{
exposure_summary(A_history, how = "cumulative")
}
\arguments{
\item{A_history}{See Usage.}
\item{how}{One of \code{"cumulative"}, \code{"duration"}, \code{"final"}. Defaults to \code{"cumulative"}.}
}
\description{
A step of the polkrn_native implementation. Called by \code{morie_polkrn}, \code{Tmlspl}. See the file header for the source the module follows.
}
\value{
The value of \code{rowSums}.
}
\references{
----------
Hernan, M. A., Brumback, B. & Robins, J. M. (2002) "Estimating the
causal effect of zidovudine on CD4 count with a marginal structural
model for repeated measures", Statistics in Medicine 21(12),
1689-1709, doi:10.1002/sim.1144.

Robins, J. M., Hernan, M. A. & Brumback, B. (2000) "Marginal structural
models and causal inference in epidemiology", Epidemiology 11(5),
550-560, doi:10.1097/00001648-200009000-00011 -- the weights.

Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If,
Chapman & Hall/CRC, Sec. 21.2.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
exposure_summary(V)
}
