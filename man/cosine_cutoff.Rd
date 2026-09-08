% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cosine_cutoff}
\alias{cosine_cutoff}
\title{cosine_cutoff}
\usage{
cosine_cutoff(r, cutoff = 5) 
}
\arguments{
\item{r}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{cutoff}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{5}.}
}
\description{
A step of the schN_native implementation. Called by \code{cfconv}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
cosine_cutoff(r = 5L)
}
