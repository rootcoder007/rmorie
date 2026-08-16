% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sbert_vec}
\alias{.sbert_vec}
\title{.sbert_vec}
\usage{
.sbert_vec(x)
}
\arguments{
\item{x}{A list; the body checks with \code{is.list}.}
}
\description{
A step of the sbert_native implementation. Called by \code{classification_features}, \code{cosine_similarity}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
