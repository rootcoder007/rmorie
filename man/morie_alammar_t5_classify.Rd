% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_t5_classify}
\alias{morie_alammar_t5_classify}
\title{morie_alammar_t5_classify}
\usage{
morie_alammar_t5_classify(input_text, label_tokens, model, prefix = "")
}
\arguments{
\item{input_text}{Input.}
\item{label_tokens}{Closed label set.}
\item{model}{function(input, label) -> log-probability.}
\item{prefix}{Optional task prefix.}
}
\description{
morie_alammar_t5_classify
}
\value{
A list with \code{predicted_label}, \code{probabilities}, \code{log_scores}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
