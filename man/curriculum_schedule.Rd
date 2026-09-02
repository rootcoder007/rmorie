% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{curriculum_schedule}
\alias{curriculum_schedule}
\title{curriculum_schedule}
\usage{
curriculum_schedule(difficulty, n_steps = 5, hard_first = FALSE) 
}
\arguments{
\item{difficulty}{Passed to \code{.prgrl_to_vec}.}
\item{n_steps}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{5}.}
\item{hard_first}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the prgrl_native implementation. Called by \code{prgrl}. See the file header for the source the module follows.
}
\value{
A list with \code{lambdas}, \code{weights}, \code{dists}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
curriculum_schedule(V)
}
