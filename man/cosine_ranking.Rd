% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cosine_ranking}
\alias{cosine_ranking}
\title{cosine_ranking}
\usage{
cosine_ranking(q_hat, model, top_k = 5)
}
\arguments{
\item{q_hat}{A vector; indexed elementwise.}
\item{model}{A list; the body reads \code{$D}, \code{$S} from it.}
\item{top_k}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.}
}
\description{
A step of the lsa_native implementation. Called by \code{morie_lsa}. See the file header for the source the module follows.
}
\value{
A list with \code{ranking}, \code{scores}, \code{n_documents}.
}
\keyword{internal}
