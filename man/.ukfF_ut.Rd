% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ukfF_ut}
\alias{.ukfF_ut}
\title{.ukfF_ut}
\usage{
.ukfF_ut(pts, w, fun)
}
\arguments{
\item{pts}{A matrix; indexed by row and column.}
\item{w}{A matrix; passed to \code{crossprod}.}
\item{fun}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the ukfF_mixedcase_native implementation. Called by \code{morie_ukfF}. See the file header for the source the module follows.
}
\value{
A list with \code{ys}, \code{mean}, \code{cov}.
}
\keyword{internal}
