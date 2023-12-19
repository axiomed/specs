/-
Specs.lean uses `matchers` to test and describe values in different ways. This module defines
a bunch of then
-/

import Specs.Core

open Specs.Core
open Cats.Trans

namespace Specs.Matchers

/-- `describe` creates a block that groups a bunch of tests that are related. -/
def describe (label: String) (x: SpecsM α β) : SpecsM α β :=
  let withEnv := SpecsM.withEnv (λ env => env.push label) x
  SpecsM.mapTree (Array.singleton ∘ specGroup label) withEnv

/-- `it` defines a single test with a label -/
def it (label: String) (action: α) : SpecsM α Unit :=
  let item : Item α := { requirement := label, action }
  WriterT.tell #[Tree.leaf item]

/-- `assert` checks that two values are equal. -/
def assert [Repr α] [BEq α] (message: String) (actual: α) (expected: α) : Test := do
  if actual != expected then
    throw { message, reason := Reason.equality (repr actual) (repr expected) }

/-- `skip` is used to skip a test. -/
def skip : Test := pure ()

/-- `fail` is used to fail a test inconditionally. -/
def fail (message: String) : Test :=
  throw { message, reason := Reason.failure message }

/-- `shouldBe` is used to check that two values are equal. -/
def shouldBe [Repr α] [BEq α] (actual: α) (expected: α) : Test :=
  assert "should be" actual expected

/-- checks if a value is the default value for the `Inhabited` type class -/
def isEmpty [Repr α] [BEq α] [Inhabited α] (actual: α) : Test :=
  assert "should be empty" actual Inhabited.default

/-- checks if a value is greater than another value -/
def isGreater [Repr α] [BEq α] [Ord α] (actual: α) (expected: α) : Test := do
  match Ord.compare actual expected with
  | Ordering.gt => pure ()
  | _ => throw { message := "should be greater", reason := Reason.comparison (repr actual) (repr expected) }

/-- checks if a value is less than another value -/
def isLess [Repr α] [BEq α] [Ord α] (actual: α) (expected: α) : Test := do
  match Ord.compare actual expected with
  | Ordering.lt => pure ()
  | _ => throw { message := "should be less", reason := Reason.comparison (repr actual) (repr expected) }

/-- checks if a value is greater or equal than another value -/
def isGreaterOrEqual [Repr α] [BEq α] [Ord α] (actual: α) (expected: α) : Test := do
  match Ord.compare actual expected with
  | Ordering.gt => pure ()
  | Ordering.eq => pure ()
  | _ => throw { message := "should be greater or equal", reason := Reason.comparison (repr actual) (repr expected) }

/-- checks if a value is less or equal than another value -/
def isLessOrEqual [Repr α] [BEq α] [Ord α] (actual: α) (expected: α) : Test := do
  match Ord.compare actual expected with
  | Ordering.lt | Ordering.eq => pure ()
  | _ => throw { message := "should be less or equal", reason := Reason.comparison (repr actual) (repr expected) }

/-- checks if a value is true -/
def isTrue (actual: Bool) : Test := do
  if actual then pure () else throw { message := "should be true", reason := Reason.failure "should be true" }

/-- checks if a value is false -/
def isEqual [Repr α] [BEq α] (actual: α) (expected: α) : Test :=
  assert "should be equal" actual expected

end Specs.Matchers
