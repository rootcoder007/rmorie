% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fz_cs_common}
\alias{.fz_cs_common}
\title{.fz_cs_common}
\usage{
.fz_cs_common(x, t_grid, h, transform)
}
\arguments{
\item{x}{Passed to \code{.fz_check_sample}.}
\item{t_grid}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{transform}{Passed to \code{.fz_transform}.}
}
\description{
A step of the fauzi_native implementation. Called by \code{morie_fauzi_cumulative_survival_1}, \code{morie_fauzi_cumulative_survival_2}. See the file header for the source the module follows.
}
\value{
A list with \code{tr}, \code{tg}, \code{zx}, \code{zt}, \code{hh}, \code{n}.
}
\keyword{internal}
