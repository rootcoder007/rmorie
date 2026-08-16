% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_expit}
\alias{.s4_expit}
\title{.s4_expit}
\usage{
.s4_expit(z)
}
\arguments{
\item{z}{Numeric; passed to \code{exp}.}
}
\description{
A step of the s04core implementation. Called by \code{.s4_glmbin}, \code{.s4_tmle}, \code{.tmlphd_lasso_logit} and 23 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
