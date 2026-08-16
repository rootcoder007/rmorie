% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.smcsam_rwk}
\alias{.smcsam_rwk}
\title{A Metropolis random walk that, just like the Python arm\\'s}
\usage{
.smcsam_rwk(scale, n_moves)
}
\arguments{
\item{scale}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_moves}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
random_walk_kernel(scale, n_moves), performs n_moves of x\\' = x + Normal(0, scale^2) and returns the average accept rate over the inner moves.
}
\value{
The value of \code{function}.
}
\keyword{internal}
