/-
  Researchproofs/P15Decomposition.lean

  P15 (new): sentencing disparity decompositions — what the "unexplained"
  part is and is not (Oaxaca 1973; Blinder 1973; Oaxaca & Ransom 1999;
  Fortin, Lemieux & Firpo 2011).

  Two groups A and B with mean covariate vectors X̄_A, X̄_B (including a
  constant) and fitted coefficient vectors β_A, β_B. Because least squares
  with an intercept passes through the means, the mean outcomes are
  ȳ_g = X̄_g · β_g, and the gap decomposes exactly:

    * twofold_B: ȳ_A − ȳ_B = (X̄_A − X̄_B)·β_B + X̄_A·(β_A − β_B)
                            (explained at B's prices, unexplained at A's means)
    * twofold_A: ȳ_A − ȳ_B = (X̄_A − X̄_B)·β_A + X̄_B·(β_A − β_B)
    * threefold: ȳ_A − ȳ_B = (X̄_A − X̄_B)·β_B + X̄_B·(β_A − β_B) + (X̄_A − X̄_B)·(β_A − β_B)
                            (endowments, coefficients, interaction)
    * reference_dependence: the two "explained" parts differ by exactly the
      interaction term; they agree iff (X̄_A − X̄_B)·(β_A − β_B) = 0
      (explained_eq_iff). The index-number problem is not a rounding issue.
    * attribution_shift: adding a constant c to covariate j for everyone
      leaves the total unexplained part unchanged but moves c (β_A − β_B)_j
      of it from the intercept's line to covariate j's line — the per-variable
      attribution of the unexplained part is not identified (Oaxaca & Ransom
      1999; the omitted-category problem for dummies).

  Reading: a sentencing-gap study that labels the unexplained part
  "discrimination" has chosen a reference group and a coding of its
  covariates; both choices move the number. Report both twofold versions
  and the interaction, and never attribute the unexplained part to a
  categorical covariate by line.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P15

variable {p : ℕ}

def dot (x b : Fin p → ℝ) : ℝ := ∑ j, x j * b j

theorem dot_sub_left (x y b : Fin p → ℝ) : dot (x - y) b = dot x b - dot y b := by
  unfold dot; rw [← sum_sub_distrib]; apply sum_congr rfl; intro j _; simp [sub_mul]

theorem dot_sub_right (x b c : Fin p → ℝ) : dot x (b - c) = dot x b - dot x c := by
  unfold dot; rw [← sum_sub_distrib]; apply sum_congr rfl; intro j _; simp [mul_sub]

/-- Group means and coefficients; `ybar_eq` records that least squares with a constant passes through the means. -/
structure Groups (p : ℕ) where
  (xA xB bA bB : Fin p → ℝ)
  (yA yB : ℝ)
  (yA_eq : yA = dot xA bA)
  (yB_eq : yB = dot xB bB)

variable (G : Groups p)

def explainedB : ℝ := dot (G.xA - G.xB) G.bB
def unexplainedA : ℝ := dot G.xA (G.bA - G.bB)
def explainedA : ℝ := dot (G.xA - G.xB) G.bA
def unexplainedB : ℝ := dot G.xB (G.bA - G.bB)
def interaction : ℝ := dot (G.xA - G.xB) (G.bA - G.bB)

theorem twofold_B : G.yA - G.yB = explainedB G + unexplainedA G := by
  unfold explainedB unexplainedA; rw [G.yA_eq, G.yB_eq, dot_sub_left, dot_sub_right]; ring

theorem twofold_A : G.yA - G.yB = explainedA G + unexplainedB G := by
  unfold explainedA unexplainedB; rw [G.yA_eq, G.yB_eq, dot_sub_left, dot_sub_right]; ring

theorem threefold : G.yA - G.yB = explainedB G + unexplainedB G + interaction G := by
  unfold explainedB unexplainedB interaction
  rw [G.yA_eq, G.yB_eq, dot_sub_left, dot_sub_right, dot_sub_left, dot_sub_right, dot_sub_right]; ring

/-- The two explained parts differ by exactly the interaction term. -/
theorem reference_dependence : explainedA G - explainedB G = interaction G := by
  unfold explainedA explainedB interaction; rw [dot_sub_right]

theorem explained_eq_iff : explainedA G = explainedB G ↔ interaction G = 0 := by
  rw [← reference_dependence]; constructor <;> intro h <;> linarith

/-- Shifting covariate `j` by `c` for everyone (both groups, same coefficients) keeps the
    unexplained total but moves `c (β_A − β_B)_j` between lines of the attribution. -/
theorem attribution_shift (c : ℝ) (j : Fin p) :
    let xA' := Function.update G.xA j (G.xA j + c)
    dot xA' (G.bA - G.bB) = unexplainedA G + c * (G.bA j - G.bB j) ∧
      xA' j * (G.bA j - G.bB j) = G.xA j * (G.bA j - G.bB j) + c * (G.bA j - G.bB j) := by
  intro xA'
  constructor
  · unfold unexplainedA dot
    have : ∀ k, xA' k * (G.bA - G.bB) k = G.xA k * (G.bA - G.bB) k + (if k = j then c * (G.bA j - G.bB j) else 0) := by
      intro k
      by_cases hk : k = j
      · subst hk; simp [xA', Function.update_self]; ring
      · simp [xA', Function.update_of_ne hk, hk]
    simp_rw [this]; rw [sum_add_distrib, sum_ite_eq' univ j]; simp
  · simp [xA', Function.update_self]; ring

end Research.P15
