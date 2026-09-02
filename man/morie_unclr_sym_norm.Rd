% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_unclr_sym_norm}
\alias{morie_unclr_sym_norm}
\title{morie_unclr_sym_norm}
\usage{
morie_unclr_sym_norm(A, self_loops) 
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
\item{self_loops}{A flag; the body branches on it.}
}
\description{
A step of the unclr implementation. Called by \code{Lgcnprop}, \code{Sgcprop}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
A <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
morie_unclr_sym_norm(A, self_loops = TRUE)
}
