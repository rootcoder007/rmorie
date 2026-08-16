% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_golden_section}
\alias{.schab_golden_section}
\title{Golden section rather than R\\'s optimize (Brent): deterministic and}
\usage{
.schab_golden_section(func, lower, upper, tol = 1e-04, max_iter = 200L)
}
\arguments{
\item{func}{See Usage.}
\item{lower}{See Usage.}
\item{upper}{See Usage.}
\item{tol}{Defaults to \code{1e-04}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200L}.}
}
\description{
with no parabolic-interpolation step whose tie-breaking the Python arm would have to match bit for bit.
}
\value{
A list with \code{x}, \code{value}.
}
\keyword{internal}
