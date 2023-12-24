import Specs.Core
import Specs.Config
import Specs.Display

import Lean

/-!
Module for running `Specs` tests. This module is responsible for executing the tests and printing
the results.
-/

open Specs Specs.Core Specs.Display

namespace Specs.Runner

private def executeTest (config : Config) (item: Item Test) : ConfigurableTestTree :=
  let result := ExceptT.run item.action

  let ⟨failed, errMessage⟩ :=
    match result with
    | Except.ok () => (false, none)
    | Except.error failure => (true, some (ToString.toString failure))

  let succeded := item.shouldFail == failed

  let testTree := TestTree.test succeded item.requirement errMessage

  ConfigurableTestTree.mk testTree config

partial def executeTree (config: Config) (tree: Tree Test) : ConfigurableTestTree :=
  match tree with
  | Tree.leaf item => executeTest config item
  | Tree.node name tests _ => Id.run do
    let mut arr := Array.empty
    for tree in tests do
      let res := executeTree config tree
      arr := Array.push arr res
      if res.testTree.failed && config.bail then break
    let new_arr := arr.map (λ x => x.testTree)
    return ConfigurableTestTree.mk (TestTree.group name new_arr) config

def executePure (config: Config) (specs: Specs) : Array ConfigurableTestTree := Id.run do
  let tests := specs.run

  let mut arr := Array.empty

  for tree in tests do
    let res := executeTree config tree
    arr := Array.push arr res
    if res.testTree.failed && config.bail then break

  return arr

/-- Execute the given `Specs` and print the results. -/
def executeIO (config: Config) (specs: Specs) : IO UInt32 := do
  IO.println "\nRunning tests...\n"
  let tests := specs.run

  -- let mut result := 0
  let mut arr := Array.empty

  for tree in tests do
    let res := executeTree config tree
    arr := Array.push arr res
    if res.testTree.failed && config.bail then break

  IO.println s!"{Specs.Display.displayMultiple arr}"

  let result := arr.foldl (λ acc test => acc + test.testTree.countFailures) 0

  if result == 0 then
    IO.println "All tests passed!\n"
  else
    IO.println s!"{result} test(s) failed.\n"

  return if result == 0 then 0 else 1

end Specs.Runner
