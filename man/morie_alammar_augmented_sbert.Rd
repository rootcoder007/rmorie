% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_augmented_sbert}
\alias{morie_alammar_augmented_sbert}
\title{morie_alammar_augmented_sbert}
\usage{
morie_alammar_augmented_sbert(unlabeled_pairs, cross_encoder, gold_pairs = NULL, gold_labels = NULL)
}
\arguments{
\item{unlabeled_pairs}{List of c(a, b).}
\item{cross_encoder}{   function(a, b) -> score.}
\item{gold_pairs,gold_labels}{Optional.}
}
\description{
morie_alammar_augmented_sbert
}
\value{
A list with \code{n_silver}, \code{n_gold}, \code{silver_labels}, \code{cross_encoder_gold_agreement}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
b <- morie_alammar_bag_of_words(c("a", "b", "a", "z"), c("a", 
    "b", "c"))
morie_alammar_augmented_sbert(list(c("x", "x"), c("x", "y")), 
    function(a, b) if (a == b) 1 else 0, gold_pairs = list(c("p", 
        "p"), c("p", "q"), c("r", "r"), c("r", "s")), gold_labels = c(1, 
        0, 1, 0))
}
