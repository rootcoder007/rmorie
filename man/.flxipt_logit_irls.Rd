% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.flxipt_logit_irls}
\alias{.flxipt_logit_irls}
\title{Logistic IRLS, internal: returns coef vector --------------------}
\usage{
.flxipt_logit_irls(X, y, ridge = 1e-10, penalty = 0, max_iter = 200, tol = 1e-08)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{ridge}{Defaults to \code{1e-10}.}
\item{penalty}{Defaults to \code{0}.}
\item{max_iter}{Defaults to \code{200}.}
\item{tol}{Defaults to \code{1e-08}.}
}
\description{
Part of the flxipt_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{b}, as built in the body.
}
\keyword{internal}
