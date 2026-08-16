% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{instruction_prompt}
\alias{instruction_prompt}
\title{instruction_prompt}
\usage{
instruction_prompt(symbolic, kind = "conversation")
}
\arguments{
\item{symbolic}{A list; the body reads \code{$text} from it.}
\item{kind}{Carried through into a list the body builds. Defaults to \code{"conversation"}.}
}
\description{
A step of the llavx_native implementation. Called by \code{morie_llavx}. See the file header for the source the module follows.
}
\value{
A list with \code{prompt}, \code{kind}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
instruction_prompt(D)
}
