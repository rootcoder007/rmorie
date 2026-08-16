% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{orthogonal_moment}
\alias{orthogonal_moment}
\title{Solve sum_i alpha_i T~(Y~ - theta T~) = 0. Refuses when the}
\usage{
orthogonal_moment(y_res, t_res, weights)
}
\arguments{
\item{y_res}{See Usage.}
\item{t_res}{See Usage.}
\item{weights}{See Usage.}
}
\description{
residualized treatment has no weighted variation left.
}
\value{
A list with \code{theta}, \code{den}.
}
\keyword{internal}
