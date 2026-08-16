% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_lcg_w}
\alias{.morie_gr_lcg_w}
\title{.morie_gr_lcg_w}
\usage{
.morie_gr_lcg_w(count, seed, scale = 0.1)
}
\arguments{
\item{count}{Passed to \code{.morie_gr_lcg_u}.}
\item{seed}{Passed to \code{.morie_gr_lcg_u}.}
\item{scale}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{.morie_gr_init}, \code{morie_geron_bert}, \code{morie_geron_roberta}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
