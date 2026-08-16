% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.esl_var_p}
\alias{.esl_var_p}
\title{Population variance, matching numpy\\'s np.var default (ddof = 0);}
\usage{
.esl_var_p(y)
}
\arguments{
\item{y}{A vector; its length is taken.}
}
\description{
stats::var uses ddof = 1 and would weight the split criterion differently
}
\value{
A numeric value.
}
\keyword{internal}
