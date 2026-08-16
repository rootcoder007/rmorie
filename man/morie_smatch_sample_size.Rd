% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_smatch_sample_size}
\alias{morie_smatch_sample_size}
\title{morie_smatch_sample_size}
\usage{
morie_smatch_sample_size(log_ri, r, p_exposed, alpha = 0.05, power = 0.8)
}
\arguments{
\item{log_ri}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{r}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{p_exposed}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
\item{power}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.8}.}
}
\description{
A step of the smatch_native implementation. Called by \code{morie_smatch_power}. See the file header for the source the module follows.
}
\value{
A list with \code{n_events}, \code{n_events_ceiling}, \code{rho}, \code{A}, \code{B}, \code{C}, \code{z_alpha_2}, \code{z_power}, \code{log_ri}, \code{r}, \code{p_exposed}, \code{assumes}, \code{method}.
}
\keyword{internal}
