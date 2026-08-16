% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.survroc_empirical}
\alias{.survroc_empirical}
\title{.survroc_empirical}
\usage{
.survroc_empirical(T, E, M, c, t)
}
\arguments{
\item{T}{Passed to \code{<}.}
\item{E}{Passed to \code{==}.}
\item{M}{A vector; indexed elementwise.}
\item{c}{Passed to \code{>}.}
\item{t}{Passed to \code{<}.}
}
\description{
A step of the survroc_native implementation. Called by \code{.survroc_pair}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\references{
----------
Heagerty, P. J., Lumley, T. & Pepe, M. S. (2000) "Time-dependent ROC
curves for censored survival data and a diagnostic marker",
Biometrics 56(2), 337-344,
doi:10.1111/j.0006-341X.2000.00337.x. Sec. 2 (the cumulative
case / dynamic control definitions above), Sec. 2.1 (the
Kaplan-Meier estimator of sensitivity and specificity reproduced
here, and its reduction to the empirical estimator without
censoring), and the time-dependent area under the curve.
}
\keyword{internal}
\examples{
.survroc_empirical(T = c(1, 2, 3, 4, 5, 6, 7, 8), E = c(1, 2, 3, 4, 5, 6, 7, 8), M = 5L, c = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5))
}
