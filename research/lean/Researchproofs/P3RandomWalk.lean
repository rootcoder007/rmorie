/-
  Researchproofs/P3RandomWalk.lean

  P3, continued: random-walk exposure on a graph is proportional to degree
  (library scan C15; Grimmett & Stirzaker Exercises 6.4.6, 6.5.9).

  On a finite undirected graph without isolated vertices the simple random
  walk (uniform over neighbours) has the stationary law π_v = d_v/(2|E|)
  (`stationary`, `stationary_sum_one`): a patrol that moves to a uniformly
  random neighbouring segment spends time on a segment in proportion to its
  degree, whatever the crime there. On a directed network the out-degree law
  is not stationary (`directed_fails`: a three-node digraph).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Finset SimpleGraph

noncomputable section

namespace Research.P3.RandomWalk

variable {V : Type*} [Fintype V] (G : SimpleGraph V) [DecidableRel G.Adj]

/-- Transition kernel of the simple random walk. -/
def P (u v : V) : ℝ := if G.Adj u v then 1 / (G.degree u : ℝ) else 0
/-- The degree law. -/
def π (v : V) : ℝ := (G.degree v : ℝ) / (2 * (G.edgeFinset.card : ℝ))

theorem stationary (hdeg : ∀ v, 0 < G.degree v) (v : V) : ∑ u, π G u * P G u v = π G v := by
  unfold π P
  have h : ∀ u, (G.degree u : ℝ) / (2 * (G.edgeFinset.card : ℝ)) * (if G.Adj u v then 1 / (G.degree u : ℝ) else 0)
      = if G.Adj v u then 1 / (2 * (G.edgeFinset.card : ℝ)) else 0 := by
    intro u
    by_cases hadj : G.Adj u v
    · rw [if_pos hadj, if_pos hadj.symm]
      have : (G.degree u : ℝ) ≠ 0 := by exact_mod_cast (hdeg u).ne'
      field_simp
    · rw [if_neg hadj, if_neg (fun h => hadj h.symm)]; ring
  simp_rw [h]
  rw [← sum_filter, sum_const, nsmul_eq_mul]
  rw [← neighborFinset_eq_filter, card_neighborFinset_eq_degree]
  ring

theorem stationary_sum_one (hE : 0 < G.edgeFinset.card) : ∑ v, π G v = 1 := by
  unfold π
  rw [← sum_div]
  have : (∑ v, (G.degree v : ℝ)) = 2 * G.edgeFinset.card := by
    exact_mod_cast G.sum_degrees_eq_twice_card_edges
  rw [this]
  have : (2 * G.edgeFinset.card : ℝ) ≠ 0 := by positivity
  field_simp

/-- A directed three-node network: 0 → 1, 1 → 2, 2 → 0, 0 → 2. Out-degrees (2, 1, 1); the
out-degree law (1/2, 1/4, 1/4) is not stationary: mass flowing into node 0 is 1/4, not 1/2. -/
theorem directed_fails :
    let πd : Fin 3 → ℝ := ![1 / 2, 1 / 4, 1 / 4]
    let Pd : Fin 3 → Fin 3 → ℝ := ![![0, 1 / 2, 1 / 2], ![0, 0, 1], ![1, 0, 0]]
    ∑ u, πd u * Pd u 0 ≠ πd 0 := by
  norm_num [Fin.sum_univ_three]

end Research.P3.RandomWalk
