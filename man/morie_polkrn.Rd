% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_polkrn}
\alias{morie_polkrn}
\title{morie_polkrn}
\usage{
morie_polkrn(y, A_history, H_history, degree = 2, basis = "both", summary = "cumulative", n_centres = 5, width = NULL, kind = "binary", stabilize = TRUE, trim = NULL, grid = NULL)
}
\arguments{
\item{y}{Passed to \code{.polkrn_vec}.}
\item{A_history}{Coerced to list by the body, with \code{as.list}.}
\item{H_history}{Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.}
\item{degree}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.}
\item{basis}{One of \code{"both"}, \code{"kernel"}, \code{"polynomial"}. Defaults to \code{"both"}.}
\item{summary}{Carried through into a list the body builds. Defaults to \code{"cumulative"}.}
\item{n_centres}{Defaults to \code{5}.}
\item{width}{Defaults to \code{NULL}.}
\item{kind}{Passed to \code{.polkrn_ip_weights_history}. Defaults to \code{"binary"}.}
\item{stabilize}{Passed to \code{.polkrn_ip_weights_history}. Defaults to \code{TRUE}.}
\item{trim}{Passed to \code{.polkrn_ip_weights_history}.}
\item{grid}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
A step of the polkrn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
