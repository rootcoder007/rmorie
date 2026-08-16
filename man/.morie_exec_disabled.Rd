% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_exec_disabled}
\alias{.morie_exec_disabled}
\title{TRUE when dynamic execution is disabled via MORIE_NO_EXEC}
\usage{
.morie_exec_disabled()
}
\description{
A step of the exec_guard implementation. Called by \code{.morie_ensure_exec_allowed}, \code{.morie_safe_readRDS}. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_env_true}.
}
\keyword{internal}
\examples{
.morie_exec_disabled()
}
