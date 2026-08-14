# Parsing expression grammars: no ambiguity to resolve.
# Sources: Ford, B. (2004) "Parsing Expression Grammars: A
# Recognition-Based Syntactic Foundation", POPL '04, 111-122,
# doi:10.1145/964001.964011; Ford, B. (2002) "Packrat Parsing:
# Simple, Powerful, Lazy, Linear Time", ICFP 2002, 36-47,
# doi:10.1145/581478.581483. Mirroring morie.fn.prsPEG: same
# prioritised choice, same greedy *, + and ?, same & and ! lookahead,
# same plain and packrat (memoised) recognisers.

.EPS <- 1e-12
FAIL <- NA_integer_

.probe <- function(text, pos, ctx, fn) {
  ctx$steps <- ctx$steps + 1L
  fn(text, pos, ctx)
}

morie_prsPEG_lit <- function(s) {
  fn <- function(text, pos, ctx) {
    ctx$steps <- ctx$steps + 1L
    if (substr(text, pos + 1L, pos + nchar(s)) == s) pos + nchar(s) else FAIL
  }
  attr(fn, "tag") <- c("lit", s)
  fn
}

morie_prsPEG_seq <- function(...) {
  es <- list(...)
  fn <- function(text, pos, ctx) {
    ctx$steps <- ctx$steps + 1L
    p <- pos
    for (e in es) {
      p <- .probe(text, p, ctx, e)
      if (is.na(p)) return(FAIL)
    }
    p
  }
  attr(fn, "tag") <- c("seq", length(es))
  fn
}

morie_prsPEG_choice <- function(...) {
  es <- list(...)
  fn <- function(text, pos, ctx) {
    ctx$steps <- ctx$steps + 1L
    for (e in es) {
      p <- .probe(text, pos, ctx, e)
      if (!is.na(p)) return(p)
    }
    FAIL
  }
  attr(fn, "tag") <- c("choice", length(es))
  fn
}

morie_prsPEG_star <- function(e) {
  fn <- function(text, pos, ctx) {
    ctx$steps <- ctx$steps + 1L
    p <- pos
    repeat {
      q <- .probe(text, p, ctx, e)
      if (is.na(q) || q == p) return(p)
      p <- q
    }
  }
  attr(fn, "tag") <- c("star")
  fn
}

morie_prsPEG_plus <- function(e) morie_prsPEG_seq(e, morie_prsPEG_star(e))

morie_prsPEG_opt <- function(e) morie_prsPEG_choice(e, morie_prsPEG_lit(""))

morie_prsPEG_and_ <- function(e) {
  fn <- function(text, pos, ctx) {
    ctx$steps <- ctx$steps + 1L
    if (is.na(.probe(text, pos, ctx, e))) FAIL else pos
  }
  attr(fn, "tag") <- c("and")
  fn
}

morie_prsPEG_not_ <- function(e) {
  fn <- function(text, pos, ctx) {
    ctx$steps <- ctx$steps + 1L
    if (is.na(.probe(text, pos, ctx, e))) pos else FAIL
  }
  attr(fn, "tag") <- c("not")
  fn
}

morie_prsPEG_parse <- function(expr, text, full = TRUE) {
  ctx <- list(steps = 0L, memo = NULL)
  end <- expr(as.character(text), 0L, ctx)
  ok <- !is.na(end) && (!full || end == nchar(text))
  list(estimate = ok, matched = ok, end = end,
       consumed = if (is.na(end)) 0L else end,
       steps = ctx$steps, memoised = FALSE,
       method = "PEG recognition; Ford (2004)",
       note = "prioritised choice, so there is at most ONE parse")
}

morie_prsPEG_packrat_parse <- function(expr, text, full = TRUE) {
  memo <- new.env(parent = emptyenv())
  ctx <- list(steps = 0L, memo = memo)
  eid <- "0"
  attr(expr, "eid") <- eid

  wrap <- function(e) {
    key <- attr(e, "tag")
    eid <- attr(e, "eid")
    fn <- function(t, pos, c) {
      kk <- paste0(eid, ":", pos)
      if (exists(kk, envir = memo, inherits = FALSE))
        return(get(kk, envir = memo, inherits = FALSE))
      r <- e(t, pos, c)
      assign(kk, r, envir = memo)
      r
    }
    attr(fn, "tag") <- key
    attr(fn, "eid") <- eid
    fn
  }

  end <- wrap(expr)(as.character(text), 0L, ctx)
  ok <- !is.na(end) && (!full || end == nchar(text))
  list(estimate = ok, matched = ok, end = end,
       steps = ctx$steps, memo.entries = length(ls(memo)),
       memoised = TRUE,
       method = "packrat parsing; Ford (2002)")
}

# house entry point: the package exports one morie_<module>
morie_prsPEG <- morie_prsPEG_lit
