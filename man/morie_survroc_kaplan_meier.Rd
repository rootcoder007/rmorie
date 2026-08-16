% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survroc_kaplan_meier}
\alias{morie_survroc_kaplan_meier}
\title{morie_survroc_kaplan_meier}
\usage{
morie_survroc_kaplan_meier(times, events, at = NULL)
}
\arguments{
\item{times}{Passed to \code{.survroc_clean}.}
\item{events}{Passed to \code{.survroc_clean}.}
\item{at}{Defaults to \code{NULL}.}
}
\description{
A step of the survroc_native implementation. Called by \code{.survroc_km_pair}, \code{morie_survroc}. See the file header for the source the module follows.
}
\value{
The value of \code{val}, as built in the body.
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
