% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_conversation_buffer_memory}
\alias{morie_alammar_conversation_buffer_memory}
\title{morie_alammar_conversation_buffer_memory}
\usage{
morie_alammar_conversation_buffer_memory(conversation, N) 
}
\arguments{
\item{conversation}{List of c(user, assistant) turns.}
\item{N}{Window.}
}
\description{
morie_alammar_conversation_buffer_memory
}
\value{
A list with \code{memory}, \code{turns_forgotten}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_conversation_buffer_memory(conversation = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), N = 5L)
}
