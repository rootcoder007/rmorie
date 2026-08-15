So `_s03core` contains: `vec`, `mat`, `logit_irls`, `sigmoid`, `logistic_fluctuation`, `qnorm`, `sd`, `design`, `matvec`.

In R, the helpers would be named differently. Looking at R conventions, I'd guess:
- `k.vec` → `s03_vec` or something like that
- `k.mat` → `s03_mat`
- etc.

But I don't have access to the R code. I need to implement these in R. Let me write them as private helpers with the prefix `.<module>_` per the spec.

Actually, the spec says "Private helpers must be prefixed .<module>_". So my private helpers in this module would be `.tmlcic_*`. But the k.* functions aren't in this module, they're in `_s03core`.

Hmm, let me think about this differently. The instruction says to translate the Python module into native R. The Python module uses `k.*` and `np.*` from other modules. In R, I would need to either:
1. Implement the needed helpers as private functions in this R file
2. Use the R equivalents from the R/.s03core.R file

Since I don't know the exact R function names in the s03core module, and the spec says "no library() calls" and to implement the algorithm natively, I'll implement the needed helpers as private functions in this R file with the `.tmlcic_` prefix.

Wait, the spec also says: "Private helpers must be prefixed .<module>_". So I need to prefix with `.tmlcic_` for this module.

But if I implement all the k.* functions in this file, that might be too much. Let me check if there's a standard naming pattern. Given the instruction "all of R/ shares one environment and duplicate names silently collide", I should be careful.

Let me think about what's the cleanest approach. The R module needs to be self-contained. So I'll implement the necessary helpers. Let me list them:
- vec - convert to vector
- mat - convert to matrix
- logit_irls - logistic IRLS
- sigmoid
- logistic_fluctuation
- qnorm
- sd
- design
- matvec
- RichResult

Actually, looking at this more carefully, the .s03core helpers are likely already defined in R/.s03core.R. The Python import `from . import _s03core as k` becomes in R something like accessing those functions directly. But I don't know the exact names.

Let me assume the R/.s03core.R file has functions like:
- `s03_vec`
- `s03_mat`
- `s03_logit_irls`
- `s03_sigmoid`
- `s03_logistic_fluctuation`
- `s03_qnorm`
- `s03_sd`
- `s03_design`
- `s03_matvec`
- `RichResult` (or just a named list)

But I don't have access to that. Let me just implement them as private functions in this R file. The spec says "Private helpers must be prefixed .<module>_", so I'll use `.tmlcic_*` for module-private helpers.

But wait, the helpers `k.vec`, `k.mat` etc. are not really private to this module - they're in `_s03core`. In R, they would be in `R/.s03core.R` and would have whatever names that file uses.

Since I don't know the exact names, the safest approach is to implement minimal versions of the needed helpers in this file. But that would make the file very long.

Actually, let me re-read the spec one more time...

"Real implementation of the same algorithm: same formulas, same return keys (an R named list mirrors the RichResult payload), same argument names and defaults, same entry point named morie_<module>."

OK so the entry point is `morie_tmlcic` (since module is `tmlcic`).

"Native R only: no library() calls."

OK no library calls.

"If the Python uses np.random.default_rng, use .ghc_rng(seed)/.ghc_unif(e, n) which exist already and are bit-identical."

So `.ghc_rng` and `.ghc_unif` exist. But the Python code doesn't use `np.random.default_rng` - it uses deterministic computations. So I don't need those.

"For the glibc LCG pattern (1103515245*st+12345) %% 2^31 use .ghc_lcg31(st) which is exact."

The Python code doesn't use this pattern either.

"Private helpers must be prefixed .<module>_ because all of R/ shares one environment and duplicate names silently collide."

So my private helpers need `.tmlcic_` prefix.

"R traps to avoid: %% binds tighter than * (parenthesise products before %% and %/%); 1:0 is c(1,0) so use seq_len/seq_along; (i+1):n counts DOWN at i=n so guard it; & is logical not bitwise, use bitwAnd; R is 1-based where Python is 0-based; R sum() is fine for ordinary use."

OK these are important.

"Do NOT write a stub. The function must compute the real thing; its output must change when the inputs change."

So I need a real implementation.

OK so my plan:
1. Implement all the k.* helpers as private functions in this R file with `.tmlcic_` prefix (or use whatever the R convention is).

Wait, actually, looking at the instruction again: "same entry point named morie_<module>". This is a single function. So `morie_tmlcic` is the entry point. And it should mirror `tmle_cluster_ic` from Python.

Let me look at the Python module's structure:
- `tmle_cluster_ic` - main function
- `tmle_hierarchical` - another main function
- `tmleclusteric` - alias

So there are multiple main functions. But the spec says "same entry point named morie_<module>". So the module entry point is `morie_tmlcic`. This should be the main function, which is `tmle_cluster_ic` in Python.

OK let me just implement the whole thing. I'll put private helpers with `.tmlcic_` prefix. For the k.* functions, I'll implement them as needed.

Actually, looking at this more carefully, I realize that the k.* functions are complex (like logit_irls which is iterative). Implementing them all in R would make this file very long. Let me think about whether I can assume they exist.

The instruction says "R/ shares one environment". This suggests that there are R files in an R/ directory that all share a global environment. So functions defined in R/.s03core.R would be accessible.

But the naming convention isn't clear. Let me just implement the helpers in this file to be safe. I'll use the `.tmlcic_` prefix.

Wait, actually, if the k.* functions already exist in the R environment (e.g., as `s03_vec`, `s03_mat`, etc.), then I should use them. But if they don't exist, my code won't work.

Given the ambiguity, let me implement them as private helpers in this file. This is the safest approach.

Let me start writing:
