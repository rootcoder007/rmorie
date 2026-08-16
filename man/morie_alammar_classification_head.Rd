% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_classification_head}
\alias{morie_alammar_classification_head}
\title{Classification and NER heads (Alammar Ch 4)}
\usage{
morie_alammar_classification_head(h_cls, W_cls, b)
}
\arguments{
\item{h_cls}{Hidden vector.}
\item{W_cls}{Weight matrix.}
\item{b}{Bias.}
}
\description{
Classification and NER heads (Alammar Ch 4)
}
\value{
A list with \code{logits}, \code{probabilities}, \code{predicted_class}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
