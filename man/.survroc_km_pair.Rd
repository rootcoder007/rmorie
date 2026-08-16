% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.survroc_km_pair}
\alias{.survroc_km_pair}
\title{.survroc_km_pair}
\usage{
.survroc_km_pair(T, E, M, c, t)
}
\arguments{
\item{T}{A vector; its length is taken and its elements indexed.}
\item{E}{A vector; indexed elementwise.}
\item{M}{Passed to \code{>}.}
\item{c}{Passed to \code{>}.}
\item{t}{Passed to \code{morie_survroc_kaplan_meier}.}
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
