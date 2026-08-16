% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_llm_ollama_default_model}
\alias{.morie_llm_ollama_default_model}
\title{Internal helper: resolve the Ollama model to use.}
\usage{
.morie_llm_ollama_default_model(base = .morie_llm_ollama_base())
}
\description{
No hardcoded model name -- models are pulled per-machine and upstream tags get retired (llama3.2, gemma3), so an assumed default errors on most servers. Resolution order: (1) \code{OLLAMA_MODEL} if the user named one; (2) otherwise ask the server what it actually serves and use the first model. Returns \code{NA_character_} when neither is available so callers can raise a clear "pull a model or set OLLAMA_MODEL" error.
}
\value{
The value of \code{if}.
}
\keyword{internal}
\examples{
.morie_llm_ollama_default_model()
}
