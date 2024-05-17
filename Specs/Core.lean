/-!
Core definitions for the Specs test framework
-/

namespace Specs.Core

inductive Reason
  | equality (actual: Std.Format) (expected: Std.Format)
  | comparison (actual: Std.Format) (expected: Std.Format)
  | property (actual: Std.Format)
  | failure (message: String)

structure Failure where
  message : String
  reason : Reason

instance : ToString Failure where
  toString failure :=
    match failure.reason with
    | Reason.equality actual expected => s!"{failure.message}: expected {expected}, got {actual}"
    | Reason.comparison actual expected => s!"{failure.message}: comparison between {actual} and {expected} failed"
    | Reason.property actual => s!"{failure.message}: got {actual}"
    | Reason.failure message => message

abbrev Test := ExceptT Failure Id Unit

structure Item (α: Type) where
  requirement : String
  action      : α
  shouldFail  : Bool := false
  deriving Repr

inductive Tree (α: Type)
  | node (name: String) (leaves: Array (Tree α)) (parallel: Bool)
  | leaf (data: Item α)

structure Env where
  labels : List String
  deriving Repr

def Env.push (env: Env) (label: String) : Env :=
  { env with labels := label :: env.labels }

def SpecsM (α: Type) (r: Type) : Type := StateT (Array (Tree α)) (ReaderT Env Id) r

def Specs : Type := SpecsM Test Unit

def Specs.run (x: Specs) : Array (Tree Test) :=
  let ⟨(), t⟩ := x #[] { labels := [] }
  t

instance : Monad (SpecsM α) where
  pure x := λ p => pure ⟨x, p⟩
  bind x f := λ p => do
    let ⟨p, x⟩ ← x p
    f p x

instance : MonadReader Env (SpecsM α) where
  read := λ p x => pure ⟨x, p⟩

def SpecsM.withEnv (f: Env -> Env) (x: SpecsM α β) : SpecsM α β :=
  λ p n => x p (f n)

def SpecsM.withLabel (name: String) (x: SpecsM α β) : SpecsM α β :=
  SpecsM.withEnv (λ env => env.push name) x

def SpecsM.mapTree (f: Array (Tree α) -> Array (Tree α)) (x: SpecsM α β) : SpecsM α β :=
  λ p n => do
    let ⟨x, p'⟩ ← x #[] n
    pure ⟨x, Array.append p (f p')⟩

def specGroup (name: String) (parallel: Bool) (leaves: Array (Tree α)) : Tree α :=
  match name with
  | "" => Tree.node "(no description)" leaves parallel
  | _  => Tree.node name leaves parallel

end Specs.Core
