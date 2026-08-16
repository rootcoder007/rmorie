% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_env_true}
\alias{.morie_env_true}
\title{TRUE when an environment variable is set to a truthy value}
\usage{
.morie_env_true(name)
}
\arguments{
\item{name}{See Usage.}
}
\description{
A step of the exec_guard implementation. Called by \code{.morie_exec_disabled}. See the file header for the source the module follows.
}
\value{
The value of \code{\%in\%}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_env_true(V)
}
