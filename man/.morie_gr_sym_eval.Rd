% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_sym_eval}
\alias{.morie_gr_sym_eval}
\title{.morie_gr_sym_eval}
\usage{
.morie_gr_sym_eval(e, env)
}
\arguments{
\item{e}{A vector; its length is taken and its elements indexed.}
\item{env}{A vector; indexed elementwise.}
}
\description{
A step of the geron_train_native implementation. Called by \code{.morie_gr_sym_simplify}, \code{morie_geron_symbolic_differentiation}. See the file header for the source the module follows.
}
\value{
The value of \code{switch}.
}
\keyword{internal}
