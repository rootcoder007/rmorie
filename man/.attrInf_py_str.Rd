% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.attrInf_py_str}
\alias{.attrInf_py_str}
\title{Match Python\\\'s str() for the tie-breaking sort. Python\\\'s str(1.0)}
\usage{
.attrInf_py_str(v)
}
\arguments{
\item{v}{See Usage.}
}
\description{
is "1.0" not "1", str(True) is "True" not "TRUE", str(None) is "None"; reproducing those quirks keeps the sort identical to the Python arm.
}
\value{
A character value.
}
\keyword{internal}
