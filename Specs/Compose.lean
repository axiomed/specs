import Specs.Core

open Specs.Core
open Cats.Trans

def describe (label: String) (x: SpecsM α β) : SpecsM α β :=
  let withEnv := SpecsM.withEnv (λ env => env.push label) x
  SpecsM.mapTree (Array.singleton ∘ specGroup label) withEnv

def it (label: String) (action: α) : SpecsM α Unit :=
  let item : Item α := { requirement := label, action }
  WriterT.tell #[Tree.leaf item]

def assert [Repr α] [BEq α] (message: String) (actual: α) (expected: α) : EIO Failure Unit :=
  if actual == expected then
    pure ()
  else
    throw { message, reason := Reason.equality (repr actual) (repr expected) }

def skip : EIO Failure Unit := pure ()

def fail (message: String) : EIO Failure Unit :=
  throw { message, reason := Reason.failure message }

def shouldBe [Repr α] [BEq α] (actual: α) (expected: α) : EIO Failure Unit :=
  assert "should be" actual expected
