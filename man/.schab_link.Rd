% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_link}
\alias{.schab_link}
\title{.schab_link}
\usage{
.schab_link(x, kind, inverse = FALSE)
}
\arguments{
\item{x}{Numeric; passed to \code{exp}.}
\item{kind}{See Usage.}
\item{inverse}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{.schab_conditional_mean}, \code{.schab_fit_pseudo_likelihood}, \code{.schab_naive_marginal_mean} and 3 others in the module. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
