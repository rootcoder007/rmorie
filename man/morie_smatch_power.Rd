% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_smatch_power}
\alias{morie_smatch_power}
\title{morie_smatch_power}
\usage{
morie_smatch_power(n_events, log_ri, r, p_exposed, alpha = 0.05)
}
\arguments{
\item{n_events}{See Usage.}
\item{log_ri}{Passed to \code{morie_smatch_sample_size}.}
\item{r}{Passed to \code{morie_smatch_sample_size}.}
\item{p_exposed}{Passed to \code{morie_smatch_sample_size}.}
\item{alpha}{Passed to \code{morie_smatch_sample_size}. Defaults to \code{0.05}.}
}
\description{
A step of the smatch_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{power}, \code{z_power}, \code{n_events}, \code{A}, \code{B}, \code{C}.
}
\keyword{internal}
