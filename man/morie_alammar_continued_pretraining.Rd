% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_continued_pretraining}
\alias{morie_alammar_continued_pretraining}
\title{morie_alammar_continued_pretraining}
\usage{
morie_alammar_continued_pretraining(domain_corpus, mlm_loss_fn, n_mlm_steps, task_loss_fn = NULL)
}
\arguments{
\item{domain_corpus}{Documents.}
\item{mlm_loss_fn}{function(corpus,   step) -> loss.}
\item{n_mlm_steps}{Steps.}
\item{task_loss_fn}{   Optional function() -> loss.}
}
\description{
morie_alammar_continued_pretraining
}
\value{
A list with \code{mlm_loss_curve}, \code{mlm_improved}, \code{task_loss}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
