% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_embedding_classifier}
\alias{morie_alammar_embedding_classifier}
\title{Softmax head on frozen embeddings; SetFit pair generation}
\usage{
morie_alammar_embedding_classifier(embeddings, labels, n_steps = 500, learning_rate = 0.5, l2 = 1e-04)
}
\arguments{
\item{embeddings,labels}{Data (labels 0-based, every class present).}
\item{n_steps,learning_rate,l2}{Training settings.}
}
\description{
Softmax head on frozen embeddings; SetFit pair generation
}
\value{
A list with \code{weights}, \code{bias}, \code{train_accuracy}, \code{cross_entropy}, \code{predictions}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
