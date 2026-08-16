% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_normal_lcg}
\alias{.propinf_normal_lcg}
\title{.propinf_normal_lcg}
\usage{
.propinf_normal_lcg(e, scale = 1)
}
\arguments{
\item{e}{Passed to \code{.propinf_lcg_draw}.}
\item{scale}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the propinf_native implementation. Called by \code{.propinf_init_net}, \code{.propinf_mlp_init}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
