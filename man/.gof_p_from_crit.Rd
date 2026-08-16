% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gof_p_from_crit}
\alias{.gof_p_from_crit}
\title{Internal helper: p-value from a critical-value row}
\usage{
.gof_p_from_crit(stat, crit, alpha)
}
\description{
Interpolates linearly in log(alpha) between the bracketing critical values. Outside the tabulated range the p-value is reported at the nearest tabulated bound; `bounded` records which side was clamped so callers can tell an exact 0.001 from "at most 0.001".
}
\value{
A list with \code{p}, \code{bounded}.
}
\keyword{internal}
\examples{
.gof_p_from_crit(stat = 5L, crit = 5L, alpha = 0.5)
}
