% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03tmle}
\alias{.s03tmle}
\title{Targeted maximum likelihood for the ATE (van der Laan and Rubin 2006,}
\usage{
.s03tmle(y, D, X = NULL, trim = 0, link = "logit")
}
\arguments{
\item{y}{Passed to \code{.s03vec}.}
\item{D}{Passed to \code{.s03vec}.}
\item{X}{Passed to \code{.s03design}.}
\item{trim}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{link}{Accepted by the signature and not used anywhere in the body. Defaults to \code{"logit"}.}
}
\description{
Int. J. Biostatistics 2(1), art. 11).  The initial Qbar is fluctuated along the logistic submodel whose score is the clever covariate H = D/g - (1-D)/(1-g); eps solves the score equation by Newton, then psi = mean(Q*(1,X) - Q*(0,X)).  y is scaled to [0, 1] so the logistic fluctuation is valid for continuous outcomes (Gruber and van der Laan 2010).
}
\value{
A list with \code{psi}, \code{se}, \code{eps}, \code{g}, \code{q1}, \code{q0}, \code{inf}, \code{ey1}, \code{ey0}, \code{scale}, \code{shift}.
}
\keyword{internal}
\examples{
.s03tmle(y = 5L, D = c(1, 2, 3, 4, 5, 6, 7, 8))
}
