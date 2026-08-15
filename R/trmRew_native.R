So a variable is `(VAR, name)` - a 2-tuple. An application has `t[1]` as symbol and `t[2]` as arguments (which is a tuple/list).

Let me write this in R. In R, I'll represent:
- Variables: `list(type = "var", name = "x")` or use a special structure
- Apps: `list(type = "app", sym = "f", args = list(...))`

Actually, to mirror the Python more closely and make equality work, let me use lists consistently. R lists compare by element values, so two lists with same contents should be `==` in some cases, but actually `==` on lists in R does element-wise comparison. Let me think...

Actually in R, `list(1,2) == list(1,2)` gives `c(TRUE, TRUE)`, not a single TRUE. I need `identical()` for structural equality.

Let me design term representation in R:
- Variable: `list("var", "x")` - 2-element list
- Constant: `list("app", "a", list())` - 3-element list, empty args
- Application: `list("app", "f", list(arg1, arg2))` - 3-element list

For Python:
- Variable: `(VAR, "x")` - 2-tuple
- Constant: depends on `app` function
- Application: similar

Actually let me think about what `is_var(t)` does. From the renaming function:
