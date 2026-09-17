# Seeding the RNG for one function call only.
#
# set.seed() replaces the session's random stream. A user who seeded
# their session for reproducibility and then called a function that
# seeds internally would get identical downstream draws whatever seed
# they had chosen, with no warning. .rmorie_local_seed() sets the seed
# for the rest of the calling function and puts the caller's stream
# back (or removes .Random.seed again if there was none) when that
# function exits. morie_det_rng() is the one deliberate exception: its
# contract is to seed the session.

.rmorie_restore_seed <- function(old) {
  if (is.null(old)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  } else {
    assign(".Random.seed", old, envir = globalenv())
  }
  invisible(NULL)
}

.rmorie_local_seed <- function(seed, envir = parent.frame()) {
  if (is.null(seed)) {
    return(invisible(NULL))
  }
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  set.seed(seed)
  # Registered LIFO (after = FALSE) so that when a function seeds more
  # than once, say once per chain, the state saved before the first
  # seed is the one restored last.
  do.call(on.exit,
          list(bquote((.(.rmorie_restore_seed))(.(old))),
               add = TRUE, after = FALSE),
          envir = envir)
  invisible(seed)
}
