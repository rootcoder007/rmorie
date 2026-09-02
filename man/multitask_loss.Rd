% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{multitask_loss}
\alias{multitask_loss}
\title{multitask_loss}
\usage{
multitask_loss(l_cls, l_box, l_mask) 
}
\arguments{
\item{l_cls}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{l_box}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{l_mask}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the masrcn_native implementation. Called by \code{morie_masrcn}. See the file header for the source the module follows.
}
\value{
A list with \code{total}, \code{cls}, \code{box}, \code{mask}, \code{note}.
}
\keyword{internal}
\examples{
multitask_loss(l_cls = c(1, 2, 3, 4, 5, 6, 7, 8), l_box = c(1, 2, 3, 4, 5, 6, 7, 8), l_mask = c(1, 2, 3, 4, 5, 6, 7, 8))
}
