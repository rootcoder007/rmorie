% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_array_split}
\alias{.morie_gr_array_split}
\title{Numpy array_split(seq_len(m), K): first m \%\% K parts get one extra}
\usage{
.morie_gr_array_split(idx, K)
}
\arguments{
\item{idx}{A vector; its length is taken and its elements indexed.}
\item{K}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{morie_geron_cross_validation_score}, \code{morie_geron_kfold}, \code{morie_geron_kfold_cv} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
