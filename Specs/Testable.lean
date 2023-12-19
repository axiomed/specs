/-! Defines a `Testable` Type class that is used to things that we can test through decidable equality -/

class inductive Testable (α : Prop) where
  | isTrue (x: α)
  | isFalse (h: ¬α) (msg: String := "failed")

instance {x y : ρ} [Repr ρ] [d: Decidable (x = y)] : Testable (x = y) :=
  match d with
  | isTrue h  => Testable.isTrue h
  | isFalse h => Testable.isFalse h (msg := s!"{repr x} ≠ {repr y}")

instance {x y : ρ} [LT ρ] [Repr ρ] [d: Decidable (x < y)] : Testable (x < y) :=
  match d with
  | isTrue h  => Testable.isTrue h
  | isFalse h => Testable.isFalse h (msg := s!"{repr x} >= {repr y}")

instance {x y : ρ} [LE ρ] [Repr ρ] [d: Decidable (x ≤ y)] : Testable (x ≤ y) :=
  match d with
  | isTrue h  => Testable.isTrue h
  | isFalse h => Testable.isFalse h (msg := s!"{repr x} > {repr y}")

instance [d: Decidable ρ] : Testable ρ :=
  match d with
  | isTrue h  => Testable.isTrue h
  | isFalse h => Testable.isFalse h
