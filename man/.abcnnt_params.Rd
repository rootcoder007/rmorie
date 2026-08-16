% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcnnt_params}
\alias{.abcnnt_params}
\title{Walk every (container, index) the gradient touches. A parameter is}
\usage{
.abcnnt_params(flow)
}
\arguments{
\item{flow}{A list; the body reads \code{$layers} from it.}
}
\description{
in the active set iff its mask entry is 1; theta columns are always unmasked, hidden units are masked by deg_in, output by deg_out.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.abcnnt_params(D)
}
