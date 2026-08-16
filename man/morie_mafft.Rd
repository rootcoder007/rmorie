% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mafft}
\alias{morie_mafft}
\title{morie_mafft}
\usage{
morie_mafft(op, ...)
}
\arguments{
\item{op}{A vector; its length is taken.}
\item{...}{Passed through.}
}
\description{
A step of the mafft_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{switch}.
}
\keyword{internal}
\examples{
r <- morie_mafft("guide_tree",
                 sixtuple_distance(c("MKVLATLLPLAA", "MKVLATLLPLAG",
                                     "MRVLATGLPLAA")))
str(r, max.level = 1)
}
