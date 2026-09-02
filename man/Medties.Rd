% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Medties}
\alias{Medties}
\title{Ties at the combined median -- Sec. 6.4, p. 247}
\usage{
Medties(x, y) 
}
\description{
Ties at the combined median -- Sec. 6.4, p. 247
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{u_strict}, \code{u_inclusive}, \code{u_split}, \code{t_strict}, \code{t_inclusive}, \code{nties}, \code{median}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Medties(V, V)
}
