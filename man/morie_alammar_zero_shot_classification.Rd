% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_zero_shot_classification}
\alias{morie_alammar_zero_shot_classification}
\title{Zero-shot, T5 and judge orchestration around caller functions}
\usage{
morie_alammar_zero_shot_classification(text, candidate_labels, nli_model, hypothesis_template = "This example is about %s.")
}
\arguments{
\item{text}{Input.}
\item{candidate_labels}{Labels.}
\item{nli_model}{function(premise, hypothesis) -> score.}
\item{hypothesis_template}{Format string with one %s.}
}
\description{
Zero-shot, T5 and judge orchestration around caller functions
}
\value{
A list with \code{probabilities}, \code{predicted_label}, \code{entailment_scores}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
