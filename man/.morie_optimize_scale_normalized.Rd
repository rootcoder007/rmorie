% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_optimize_scale_normalized}
\alias{.morie_optimize_scale_normalized}
\title{.morie_optimize_scale_normalized}
\usage{
.morie_optimize_scale_normalized(fn, d, n_restarts = 8L, seed = 0L, x0 = NULL)
}
\arguments{
\item{fn}{Accepted by the signature and not used anywhere in the body.}
\item{d}{Numeric; combined arithmetically in the body.}
\item{n_restarts}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8L}.}
\item{seed}{Defaults to \code{0L}.}
\item{x0}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the horowitz_native4 implementation. Called by \code{morie_choice_based_max_score}, \code{morie_ordered_max_score}, \code{morie_panel_max_score}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{value}.
}
\keyword{internal}
