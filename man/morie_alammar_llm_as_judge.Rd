% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_llm_as_judge}
\alias{morie_alammar_llm_as_judge}
\title{morie_alammar_llm_as_judge}
\usage{
morie_alammar_llm_as_judge(responses, rubric, judge_model, n_samples = 1)
}
\arguments{
\item{responses}{Character vector.}
\item{rubric}{Judge rubric.}
\item{judge_model}{function(rubric, response, sample_index) -> score.}
\item{n_samples}{Judge samples per response.}
}
\description{
morie_alammar_llm_as_judge
}
\value{
A list with \code{scores}, \code{judge_sd}, \code{best_response}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
