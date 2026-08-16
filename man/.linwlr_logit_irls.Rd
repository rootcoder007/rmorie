% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.linwlr_logit_irls}
\alias{.linwlr_logit_irls}
\title{.linwlr_logit_irls}
\usage{
.linwlr_logit_irls(Z, y, max_iter, ridge)
}
\arguments{
\item{Z}{A matrix; passed to \code{nrow}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}.}
\item{ridge}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the linwlr_native implementation. Called by \code{morie_linwlr}. See the file header for the source the module follows.
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
