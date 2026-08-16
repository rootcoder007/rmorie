% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfbnp_schedule}
\alias{.alfbnp_schedule}
\title{.alfbnp_schedule}
\usage{
.alfbnp_schedule(T, sigma_data, s_max, s_min, rho)
}
\arguments{
\item{T}{A count; the body uses it as \code{seq_len(...)}.}
\item{sigma_data}{Numeric; combined arithmetically in the body.}
\item{s_max}{Numeric; combined arithmetically in the body.}
\item{s_min}{Numeric; combined arithmetically in the body.}
\item{rho}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the alfbnp_native implementation. Called by \code{morie_alfbnp_af3_sample}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
