% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.survroc_pair}
\alias{.survroc_pair}
\title{.survroc_pair}
\usage{
.survroc_pair(times, events, marker, c, t, route)
}
\arguments{
\item{times}{Passed to \code{.survroc_clean}.}
\item{events}{Passed to \code{.survroc_clean}.}
\item{marker}{Passed to \code{.survroc_clean}.}
\item{c}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{t}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{route}{Compared against \code{"empirical"}.}
}
\description{
A step of the survroc_native implementation. Called by \code{morie_survroc_roc_at}, \code{morie_survroc_sensitivity}, \code{morie_survroc_specificity}. See the file header for the source the module follows.
}
\value{
The value of \code{.survroc_km_pair}.
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
