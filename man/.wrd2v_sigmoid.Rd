% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wrd2v_sigmoid}
\alias{.wrd2v_sigmoid}
\title{.wrd2v_sigmoid}
\usage{
.wrd2v_sigmoid(z)
}
\arguments{
\item{z}{Numeric; passed to \code{exp}.}
}
\description{
A step of the wrd2v_native implementation. Called by \code{.wrd2v_neg_step}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.wrd2v_sigmoid(z = 5L)
}
