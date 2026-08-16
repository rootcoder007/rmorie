% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survroc_specificity}
\alias{morie_survroc_specificity}
\title{morie_survroc_specificity}
\usage{
morie_survroc_specificity(times, events, marker, threshold, t, route = "km")
}
\arguments{
\item{times}{See Usage.}
\item{events}{See Usage.}
\item{marker}{See Usage.}
\item{threshold}{See Usage.}
\item{t}{See Usage.}
\item{route}{Defaults to \code{"km"}.}
}
\description{
Part of the survroc_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{[[}.
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
