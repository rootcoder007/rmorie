% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rmrl_compile}
\alias{.rmrl_compile}
\title{.rmrl_compile}
\usage{
.rmrl_compile(phi)
}
\arguments{
\item{phi}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
A step of the rmrl_native implementation. Called by \code{morie_rmrl_reward_machine}. See the file header for the source the module follows.
}
\value{
The value of \code{function}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.rmrl_compile(D)
}
