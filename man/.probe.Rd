% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.probe}
\alias{.probe}
\title{.probe}
\usage{
.probe(text, pos, ctx, fn)
}
\arguments{
\item{text}{See Usage.}
\item{pos}{See Usage.}
\item{ctx}{A list; the body reads \code{$steps} from it.}
\item{fn}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the prsPEG_native implementation. Called by \code{morie_prsPEG_and_}, \code{morie_prsPEG_choice}, \code{morie_prsPEG_not_} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{fn}.
}
\keyword{internal}
