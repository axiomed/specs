/-
Module for running `Specs` tests.
-/

import Specs.Core

open Specs.Core

namespace Specs.Runner

structure Config where
  verbose : Bool := false
  bail    : Bool := false

instance : ToString Failure where
  toString failure :=
    match failure.reason with
    | Reason.equality actual expected => s!"{failure.message}: expected {expected}, got {actual}"
    | Reason.comparison actual expected => s!"{failure.message}: comparison between {actual} and {expected} failed"
    | Reason.property actual => s!"{failure.message}: got {actual}"
    | Reason.failure message => message

private def executeTest (ident: String) (message: String) (test: Test) : IO UInt32 := do
  let result ← EIO.catchExceptions (Except.ok <$> test) (pure ∘ Except.error)

  match result with
  | Except.ok () => do
    IO.println s!"{ident}🗸 {message}"
    return 0
  | Except.error failure => do
    IO.println s!"{ident}🗙 {message}"
    IO.println s!"{ident}    {failure}"
    return 1

private partial def executeTree (config: Config) (ident: String) (tree: Tree Test) : IO UInt32 := do
  match tree with
  | Tree.node name tests => do
    IO.println s!"{ident}{name}"
    let mut result := 0
    for tree in tests do
      result ← executeTree config (ident ++ "  ") tree
      if config.bail && result != 0 then
        return result
    return result
  | Tree.leaf data => executeTest ident data.requirement data.action

def execute (_config: Config) (specs: Specs) : IO UInt32 := do
  IO.println "\nRunning tests...\n"
  let tests := specs.run
  let mut result := 0
  for tree in tests do
    result ← executeTree _config "" tree
    if result != 0 && _config.bail then
      IO.println ""
      return result
  IO.println ""
  return result

end Specs.Runner
