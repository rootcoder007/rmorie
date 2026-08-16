% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.val_score}
\alias{.val_score}
\title{Internal helper: Val Score}
\usage{
.val_score(scoring, y_true, y_pred)
}
\description{
Internal helper: Val Score
}
\value{
The value of \code{switch}.
}
\keyword{internal}
\examples{
df <- data.frame(age = c(20, 30, -1))
rules <- list(age = list(type = "numeric", min = 0))
res <- try(validate_schema(df, rules))
if (!inherits(res, "try-error")) str(res, max.level = 1)
.val_score(scoring = df, y_true = df, y_pred = df)
}
