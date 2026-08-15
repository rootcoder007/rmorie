It samples k unique indices using the set approach with `int(rng.random() * n)`. In R, I should use the same pattern with `.ghc_rng` and `.ghc_unif`.

Let me check: `.ghc_rng(seed)` - probably creates a generator state, `.ghc_unif(e, n)` returns a uniform random number. I'll assume `.ghc_unif` takes a generator state and returns the next uniform [0, 1).

For reproducibility, I need to use these bit-identical RNGs. The Python uses `np.random.default_rng(seed)` which is the PCG64 generator. The instructions say to use `.ghc_rng(seed)/.ghc_unif(e, n)` for `np.random.default_rng`, but they also mention the glibc LCG pattern. Since numpy's default_rng uses PCG64, not glibc LCG, I should use the ghc_rng/ungc_unif pattern.

Let me write the R code:
