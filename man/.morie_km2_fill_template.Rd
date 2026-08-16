% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_fill_template}
\alias{.morie_km2_fill_template}
\title{.morie_km2_fill_template}
\usage{
.morie_km2_fill_template(template, x, z)
}
\arguments{
\item{template}{Character; passed to \code{grepl}.}
\item{x}{Character; passed to \code{trimws}.}
\item{z}{Optional; may be \code{NULL}. Character; passed to \code{gsub}.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_ch3_cloze_prompt_template}, \code{morie_kamath_ch3_prefix_prompt_template}, \code{morie_kamath_ch3_translate_prefix_prompt}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\keyword{internal}
