% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_symd_simplify}
\alias{.morie_w4d_symd_simplify}
\title{.morie_w4d_symd_simplify}
\usage{
.morie_w4d_symd_simplify(t)
}
\arguments{
\item{t}{A vector; indexed elementwise.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{morie_geron_symbolic_diff}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\keyword{internal}
\examples{
.morie_w4d_symd_simplify(list("add", list("num", 0), list("var", "x")))
}
