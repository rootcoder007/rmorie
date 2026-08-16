% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_tmpl_result}
\alias{.morie_km2_tmpl_result}
\title{.morie_km2_tmpl_result}
\usage{
.morie_km2_tmpl_result(prompt, filled, eq, template)
}
\arguments{
\item{prompt}{Character; passed to \code{trimws}.}
\item{filled}{Carried through into a list the body builds.}
\item{eq}{See Usage.}
\item{template}{Carried through into a list the body builds.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_ch3_cloze_prompt_template}, \code{morie_kamath_ch3_prefix_prompt_template}, \code{morie_kamath_ch3_translate_prefix_prompt}. See the file header for the source the module follows.
}
\value{
A list with \code{prompt}, \code{slot_filled}, \code{template}, \code{tokens}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
.morie_km2_tmpl_result(prompt = c(1, 2, 3, 4, 5, 6, 7, 8), filled = c(1, 2, 3, 4, 5, 6, 7, 8), eq = c(1, 2, 3, 4, 5, 6, 7, 8), template = c(1, 2, 3, 4, 5, 6, 7, 8))
}
