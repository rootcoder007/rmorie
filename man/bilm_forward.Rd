% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bilm_forward}
\alias{bilm_forward}
\title{Run the biLM and return every layer\'s representation}
\usage{
bilm_forward(X, layers)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{layers}{See Usage.}
}
\description{
layers is a list of (Wxf, Whf, bf, Wxb, Whb, bb). The token dimension must equal the hidden dimension, because layer 0 is the token vector duplicated and every layer has to be the same width for eq. (1) to add them. The backward pass reads the sequence in reverse and its output is re-reversed before concatenation, so position k always aligns with token k.
}
\value{
The value of \code{reps}, as built in the body.
}
\keyword{internal}
