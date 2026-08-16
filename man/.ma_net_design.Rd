% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ma_net_design}
\alias{.ma_net_design}
\title{Contrast design matrix of a treatment network. Treatments are sorted}
\usage{
.ma_net_design(design)
}
\arguments{
\item{design}{A matrix; passed to \code{as.matrix}.}
}
\description{
and the smallest is the reference, fixed at zero; a study comparing t2 against t1 contributes +1 in the column of t2 and -1 in that of t1.
}
\value{
A list with \code{X}, \code{treats}, \code{T}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.ma_net_design(D)
}
