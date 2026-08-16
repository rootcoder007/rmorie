% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dmlqs_act}
\alias{.dmlqs_act}
\title{.dmlqs_act}
\usage{
.dmlqs_act(x, activation)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{activation}{Coerced to character by the body, with \code{as.character}.}
}
\description{
A step of the dmlqs_native implementation. Called by \code{.dmlqs_message_pass}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
