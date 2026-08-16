% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sentpc_encode_bpe}
\alias{morie_sentpc_encode_bpe}
\title{Apply a trained BPE model}
\usage{
morie_sentpc_encode_bpe(text, model, add_prefix = TRUE)
}
\arguments{
\item{text}{Character.}
\item{model}{Output of \code{train_bpe}.}
\item{add_prefix}{Logical; escape-and-prefix.}
}
\description{
Apply a trained BPE model
}
\value{
Character vector of pieces.
}
\keyword{internal}
