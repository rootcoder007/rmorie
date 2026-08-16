% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_drdid_rc_native}
\alias{.morie_drdid_rc_native}
\title{Internal helper: Sant\'Anna-Zhao locally efficient DR DiD,}
\usage{
.morie_drdid_rc_native(y, post, D, X)
}
\description{
repeated-cross-section version The estimand and influence function of `DRDID::drdid_rc` (the locally efficient variant): a logistic propensity model plus four outcome regressions (control/treated x pre/post).
}
\value{
A list with \code{att}, \code{IF}, \code{se}.
}
\keyword{internal}
