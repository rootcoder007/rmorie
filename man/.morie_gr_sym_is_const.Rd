% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_sym_is_const}
\alias{.morie_gr_sym_is_const}
\title{.morie_gr_sym_is_const}
\usage{
.morie_gr_sym_is_const(e)
}
\arguments{
\item{e}{A vector; its length is taken.}
}
\description{
A step of the geron_train_native implementation. Called by \code{.morie_gr_sym_diff}, \code{.morie_gr_sym_eval}, \code{.morie_gr_sym_simplify} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_sym_is_const(V)
}
