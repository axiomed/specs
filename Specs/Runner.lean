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

private def executeTest (ident: String) (message: String) (shouldFail: Bool) (test: Test) : IO UInt32 := do
  let result := ExceptT.run test

  let ⟨failed, errMessage⟩ :=
    match result with
    | Except.ok () => (false, none)
    | Except.error failure => (true, some (ToString.toString failure))

  let mark := if shouldFail == failed then "🗸" else "🗙"
  IO.println s!"{ident}{mark} {message}"

  match errMessage with
  | some message => do if !shouldFail then IO.println s!"{ident}    {message}"
  | _ => pure ()

  return if shouldFail == failed then 0 else 1

private partial def executeTree (config: Config) (ident: String) (tree: Tree Test) : IO UInt32 := do
  match tree with
  | Tree.node name tests _ => do
    IO.println s!"{ident}{name}"
    let mut result := 0
    for tree in tests do
      result := result + (← executeTree config (ident ++ "  ") tree)
      if config.bail && result > 0 then return result
    return result
  | Tree.leaf data => executeTest ident data.requirement data.shouldFail data.action

def execute (_config: Config) (specs: Specs) : IO UInt32 := do
  IO.println "\nRunning tests...\n"
  let tests := specs.run

  let mut result := 0
  for tree in tests do
    result := result + (← executeTree _config "" tree)
    if result > 0 && _config.bail then break

  IO.println ""

  if result == 0 then
    IO.println "All tests passed!\n"
  else
    IO.println s!"{result} test(s) failed.\n"

  return if result == 0 then 0 else 1

end Specs.Runner
