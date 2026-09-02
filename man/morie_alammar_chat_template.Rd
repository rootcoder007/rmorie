% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_chat_template}
\alias{morie_alammar_chat_template}
\title{morie_alammar_chat_template}
\usage{
morie_alammar_chat_template(turns, template_tokens = NULL) 
}
\arguments{
\item{turns}{List of c(role, content).}
\item{template_tokens}{Named   list role -> c(open, close).}
}
\description{
morie_alammar_chat_template
}
\value{
A list with \code{prompt}, \code{n_turns}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_chat_template(list(c("user", "hi")), list(user = c("<u>", 
    "</u>")))
}
