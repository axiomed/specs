/-
Core definitions for the Specs test framework
-/

import Cats.Trans

open Cats.Trans

namespace Specs.Core

inductive Reason
  | equality (actual: Std.Format) (expected: Std.Format)
  | comparison (actual: Std.Format) (expected: Std.Format)
  | property (actual: Std.Format)
  | failure (message: String)

structure Failure where
  message : String
  reason : Reason

abbrev Test := EIO Failure Unit

structure Item (α: Type) where
  requirement : String
  action      : α
  deriving Repr

inductive Tree (α: Type)
  | node (name: String) (leaves: Array (Tree α))
  | leaf (data: Item α)

structure Env where
  labels : List String
  deriving Repr

def Env.push (env: Env) (label: String) : Env :=
  { env with labels := label :: env.labels }

def SpecsM (α: Type) (r: Type) : Type := WriterT (Array (Tree α)) (ReaderT Env Id) r

def Specs : Type := SpecsM (EIO Failure Unit) Unit

def Specs.run (x: Specs) : Array (Tree (EIO Failure Unit)) :=
  let ⟨res, ()⟩ := x #[] { labels := [] }
  res

instance : Monad (SpecsM α) where
  pure x := λ p => pure ⟨p, x⟩
  bind x f := λ p => do
    let ⟨p, x⟩ ← x p
    f x p

instance : MonadReader Env (SpecsM α) where
  read := λ p x => pure ⟨p, x⟩

def SpecsM.withEnv (f: Env -> Env) (x: SpecsM α β) : SpecsM α β :=
  λ p n => x p (f n)

def SpecsM.withLabel (name: String) (x: SpecsM α β) : SpecsM α β :=
  SpecsM.withEnv (λ env => env.push name) x

def SpecsM.mapTree (f: Array (Tree α) -> Array (Tree α)) (x: SpecsM α β) : SpecsM α β :=
  λ p n => do
    let ⟨p', x⟩ ← x #[] n
    pure ⟨Array.append p (f p'), x⟩

def specGroup (name: String) (leaves: Array (Tree α)) : Tree α :=
  match name with
  | "" => Tree.node "(no description)" leaves
  | _  => Tree.node name leaves

end Specs.Core
