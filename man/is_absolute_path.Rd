% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{is_absolute_path}
\alias{is_absolute_path}
\title{is_absolute_path}
\usage{
is_absolute_path(path)
}
\arguments{
\item{path}{Character; passed to \code{grepl}.}
}
\description{
A step of the paths implementation. Called by \code{morie_audit_public_outputs}. See the file header for the source the module follows.
}
\value{
The value of \code{grepl}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
is_absolute_path(V)
}
