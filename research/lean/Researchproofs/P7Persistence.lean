/-
  Researchproofs/P7Persistence.lean

  P7, continued: the rank-persistence null. "The same places are hot year after year" is
  read as evidence of stable risk. Under the null that the second year's top-k set is an
  exchangeable draw with no memory of the first (a uniformly random k-subset of the N
  places, independent of which k were hot before), the overlap of the two top-k sets is
  hypergeometric: of the C(N, k) possible second-year sets, exactly C(k, j) C(N − k, k − j)
  meet the first-year set in j places (card_overlapSets), so
  P(overlap = j) = C(k, j) C(N − k, k − j) / C(N, k) (overlap_prob) and the law sums to one
  (overlap_prob_sum). The expected overlap is k²/N (expected_overlap): with 158
  neighbourhoods and the top 16, about 1.6 places of 16 recur by chance alone.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

namespace Research.P7.Persistence

open Finset

variable {α : Type*} [Fintype α] [DecidableEq α]

/-- The k-subsets of the places that meet A in exactly j places. -/
def overlapSets (A : Finset α) (k j : ℕ) : Finset (Finset α) :=
  (univ.powersetCard k).filter fun B => (A ∩ B).card = j

/-- Counting: a k-set B meeting A in j places is a j-subset of A together with a (k − j)-subset
of the complement, and conversely. -/
theorem card_overlapSets (A : Finset α) (k j : ℕ) (hj : j ≤ k) :
    (overlapSets A k j).card = A.card.choose j * (Fintype.card α - A.card).choose (k - j) := by
  have key : (overlapSets A k j).card = ((A.powersetCard j) ×ˢ (Aᶜ.powersetCard (k - j))).card := by
    refine card_bij' (fun B _ => (B ∩ A, B \ A)) (fun P _ => P.1 ∪ P.2) ?_ ?_ ?_ ?_
    · intro B hB
      simp only [overlapSets, mem_filter, mem_powersetCard, subset_univ, true_and] at hB
      simp only [mem_product, mem_powersetCard]
      refine ⟨⟨inter_subset_right, ?_⟩, ⟨?_, ?_⟩⟩
      · rw [inter_comm]; exact hB.2
      · intro x hx
        rw [mem_sdiff] at hx
        exact mem_compl.mpr hx.2
      · have h := card_sdiff_add_card_inter B A
        rw [inter_comm] at h
        omega
    · intro P hP
      simp only [mem_product, mem_powersetCard] at hP
      obtain ⟨⟨h1s, h1c⟩, ⟨h2s, h2c⟩⟩ := hP
      have hdisj : Disjoint P.1 P.2 := by
        rw [disjoint_left]
        intro x hx1 hx2
        exact (mem_compl.mp (h2s hx2)) (h1s hx1)
      simp only [overlapSets, mem_filter, mem_powersetCard, subset_univ, true_and]
      refine ⟨?_, ?_⟩
      · rw [card_union_of_disjoint hdisj, h1c, h2c]; omega
      · have : A ∩ (P.1 ∪ P.2) = P.1 := by
          ext x
          simp only [mem_inter, mem_union]
          constructor
          · rintro ⟨hxA, hx | hx⟩
            · exact hx
            · exact absurd hxA (mem_compl.mp (h2s hx))
          · intro hx; exact ⟨h1s hx, Or.inl hx⟩
        rw [this, h1c]
    · intro B _
      simp only
      rw [union_comm, sdiff_union_inter]
    · intro P hP
      simp only [mem_product, mem_powersetCard] at hP
      obtain ⟨⟨h1s, _⟩, ⟨h2s, _⟩⟩ := hP
      have e1 : (P.1 ∪ P.2) ∩ A = P.1 := by
        ext x
        simp only [mem_inter, mem_union]
        constructor
        · rintro ⟨hx | hx, hxA⟩
          · exact hx
          · exact absurd hxA (mem_compl.mp (h2s hx))
        · intro hx; exact ⟨Or.inl hx, h1s hx⟩
      have e2 : (P.1 ∪ P.2) \ A = P.2 := by
        ext x
        simp only [mem_sdiff, mem_union]
        constructor
        · rintro ⟨hx | hx, hxA⟩
          · exact absurd (h1s hx) hxA
          · exact hx
        · intro hx; exact ⟨Or.inr hx, mem_compl.mp (h2s hx)⟩
      rw [e1, e2]
  rw [key, card_product, card_powersetCard, card_powersetCard, card_compl]

/-- The null law: a uniformly random k-subset meets the first-year top-k set in j places with
probability C(k, j) C(N − k, k − j) / C(N, k). -/
theorem overlap_prob (A : Finset α) (k j : ℕ) (hj : j ≤ k) (hA : A.card = k) :
    ((overlapSets A k j).card : ℝ) / (univ.powersetCard k : Finset (Finset α)).card =
      (k.choose j * (Fintype.card α - k).choose (k - j) : ℝ) / (Fintype.card α).choose k := by
  rw [card_overlapSets A k j hj, hA, card_powersetCard, card_univ]
  push_cast
  ring

/-- The overlap classes partition the k-subsets, so the law sums to one. -/
theorem overlap_prob_sum (A : Finset α) (k : ℕ) (hk : k ≤ Fintype.card α) :
    ∑ j ∈ range (k + 1), ((overlapSets A k j).card : ℝ) / (univ.powersetCard k : Finset (Finset α)).card = 1 := by
  have hpos : (0 : ℝ) < (univ.powersetCard k : Finset (Finset α)).card := by
    rw [card_powersetCard, card_univ]
    exact_mod_cast Nat.choose_pos hk
  rw [← sum_div, div_eq_one_iff_eq hpos.ne']
  have hcard : ∑ j ∈ range (k + 1), ((overlapSets A k j).card : ℝ) =
      ((univ.powersetCard k : Finset (Finset α)).card : ℝ) := by
    rw [← Nat.cast_sum]
    congr 1
    rw [← card_biUnion]
    · congr 1
      ext B
      simp only [mem_biUnion, mem_range, overlapSets, mem_filter, mem_powersetCard, subset_univ, true_and]
      constructor
      · rintro ⟨j, _, hB, _⟩; exact hB
      · intro hB
        refine ⟨(A ∩ B).card, ?_, hB, rfl⟩
        have := card_le_card (inter_subset_right : A ∩ B ⊆ B)
        omega
    · intro i _ j _ hij
      rw [Function.onFun, disjoint_left]
      intro B hBi hBj
      simp only [overlapSets, mem_filter] at hBi hBj
      exact hij (hBi.2.symm.trans hBj.2)
  exact hcard

end Research.P7.Persistence
