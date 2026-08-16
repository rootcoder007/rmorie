% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_one_shot}
\alias{morie_geron_one_shot}
\title{One-shot in-context prompting: assemble prompt, call model (Geron Ch 15, hmosf)}
\usage{
morie_geron_one_shot(model, example, query, verbalizer = NULL)
}
\arguments{
\item{model}{Function `model(prompt) -> prediction`.}
\item{example}{`(x1, y1)` pair or list.}
\item{query}{Input to label.}
\item{verbalizer}{Optional `verbalizer(label) -> str`.}
}
\description{
One-shot in-context prompting: assemble prompt, call model (Geron Ch 15, hmosf)
}
\value{
A list with \code{prediction}, \code{prompt}, \code{prompt_text}, \code{shots}, \code{demo_label}, \code{query}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
