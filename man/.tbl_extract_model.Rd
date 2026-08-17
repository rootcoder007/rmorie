% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tbl_extract_model}
\alias{.tbl_extract_model}
\title{Internal helper: Tbl Extract Model}
\usage{
.tbl_extract_model(m)
}
\description{
Internal helper: Tbl Extract Model
}
\value{
A list with \code{params}, \code{se}, \code{pvalues}, \code{ci}, \code{nobs}, \code{rsquared}, \code{aic}, \code{bic}, \code{llf}.
}
\keyword{internal}
\examples{
set.seed(1)
df <- data.frame(x = rnorm(30))
df$y <- df$x + rnorm(30)
m <- lm(y ~ x, data = df)
anova_table(m, typ = 1L)
.tbl_extract_model(m = m)
}
