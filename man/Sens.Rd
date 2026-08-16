% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Sens}
\alias{Sens}
\title{Eq (10.100): S+ = TP / (subjects with the disease).  Measures the}
\usage{
Sens(tp, fn = NULL)
}
\arguments{
\item{tp}{A matrix; passed to \code{as.matrix}.}
\item{fn}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
capability to DETECT and says nothing about false alarms -- a test calling everyone positive scores 1, which is why the book always reports it beside the specificity.
}
\value{
A list with \code{sensitivity}, \code{tpf}, \code{fnf}, \code{n_diseased}, \code{tp}, \code{fn}, \code{says_nothing_about_false_alarms}, \code{method}.
}
\keyword{internal}
