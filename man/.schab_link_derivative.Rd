% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_link_derivative}
\alias{.schab_link_derivative}
\title{.schab_link_derivative}
\usage{
.schab_link_derivative(mu, kind)
}
\arguments{
\item{mu}{A vector; its length is taken.}
\item{kind}{Passed to \code{identical}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{.schab_mu_eta}, \code{.schab_predict_glm}, \code{.schab_pseudo_data} and 1 others in the module. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
