% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnd_mistats}
\alias{.bnd_mistats}
\title{.bnd_mistats}
\usage{
.bnd_mistats(yl, yu)
}
\arguments{
\item{yl}{A vector; its length is taken.}
\item{yu}{Numeric; passed to \code{mean}.}
}
\description{
A step of the helpers_bnd implementation. Called by \code{Bndinf}, \code{Bnsiii}. See the file header for the source the module follows.
}
\value{
A list with \code{n}, \code{mL}, \code{sL}, \code{mU}, \code{sU}.
}
\keyword{internal}
