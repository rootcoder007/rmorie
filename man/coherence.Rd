% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{coherence}
\alias{coherence}
\title{coherence}
\usage{
coherence(A, rank = NULL)
}
\arguments{
\item{A}{Passed to \code{.meglt_mat}.}
\item{rank}{Defaults to \code{NULL}.}
}
\description{
A step of the meglt_native implementation. Called by \code{morie_meglt}. See the file header for the source the module follows.
}
\value{
A list with \code{mu_row}, \code{mu_col}, \code{mu}, \code{rank}, \code{note}.
}
\keyword{internal}
