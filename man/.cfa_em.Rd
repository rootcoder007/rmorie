% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cfa_em}
\alias{.cfa_em}
\title{Masked EM factor analysis (Rubin & Thayer 1982 E- and M-steps)}
\usage{
.cfa_em(S, mask)
}
\arguments{
\item{S}{See Usage.}
\item{mask}{See Usage.}
}
\description{
mask[i, j] is 1 where item i may load on factor j.  Factors are standardised and orthogonal, so Sigma = Lambda Lambda\\\' + Psi.
}
\value{
A list with \code{lam}, \code{psi}, \code{fml}, \code{resid}, \code{it}.
}
\keyword{internal}
