% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfqud_write}
\alias{.alfqud_write}
\title{.alfqud_write}
\usage{
.alfqud_write(st, loc, v)
}
\arguments{
\item{st}{A list; the body reads \code{$mem}, \code{$reg} from it.}
\item{loc}{A vector; indexed elementwise.}
\item{v}{See Usage.}
}
\description{
A step of the alfqud_native implementation. Called by \code{morie_alfqud_step}. See the file header for the source the module follows.
}
\value{
The value of \code{st}, as built in the body.
}
\references{
  Mankowitz, D.J. et al. (2023) "Faster sorting algorithms discovered
    using deep reinforcement learning." Nature 618, 257-263.
    doi:10.1038/s41586-023-06004-9.
  Silver, D. et al. (2017) "Mastering the game of Go without human
    knowledge." Nature 550, 354-359. The PUCT selection rule.
  Rosin, C.D. (2011) "Multi-armed bandits with episode context."
    Annals of Mathematics and Artificial Intelligence 61(3), 203-230.
}
\keyword{internal}
