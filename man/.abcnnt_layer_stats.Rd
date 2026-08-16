% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcnnt_layer_stats}
\alias{.abcnnt_layer_stats}
\title{Returns list(mu, al, h). Tanh hidden activations, clipped log-scale}
\usage{
.abcnnt_layer_stats(layer, x, t)
}
\arguments{
\item{layer}{A list; the body reads \code{$b1}, \code{$ba}, \code{$bm}, \code{$dim_t}, \code{$dim_x}, \code{$hidden}, \code{$M1}, \code{$M2}, \code{$W1}, \code{$Wa}, \code{$Wm} from it.}
\item{x}{Passed to \code{c}.}
\item{t}{Passed to \code{c}.}
}
\description{
A step of the abcnnt_native implementation. Called by \code{flow_forward}. See the file header for the source the module follows.
}
\value{
A list with \code{mu}, \code{al}, \code{h}.
}
\keyword{internal}
