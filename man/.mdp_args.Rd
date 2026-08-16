% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mdp_args}
\alias{.mdp_args}
\title{.mdp_args}
\usage{
.mdp_args(P, R)
}
\arguments{
\item{P}{See Usage.}
\item{R}{A matrix; passed to \code{dim}.}
}
\description{
A step of the mdpval_native implementation. Called by \code{morie_ddqn}, \code{morie_mdpval}, \code{morie_qlearn} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{Pm}, \code{R}, \code{S}, \code{A}.
}
\keyword{internal}
