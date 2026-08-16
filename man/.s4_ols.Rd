% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_ols}
\alias{.s4_ols}
\title{# Least squares by the SAME modified Gram-Schmidt in both arms. The}
\usage{
.s4_ols(X, y)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{A matrix; passed to \code{crossprod}.}
}
\description{
# shared tail1 core uses each language own QR (MGS in Python, Householder # in R); on an ill-conditioned design those part company around 1e-8, # above the parity threshold and below anything a user would notice.
}
\value{
A list with \code{beta}, \code{fitted}, \code{resid}, \code{xtxinv}.
}
\keyword{internal}
