import Specs.Config
import Specs.Pretty

/-! Data structure for representing the execution of a test suite. -/

namespace Specs.Display

inductive TestTree where
  | group (config : Config) (name: String) (tests: Array TestTree)
  | test (config : Config) (succeded: Bool) (message: String) (explanation: Option String)
  deriving Inhabited

partial def TestTree.reprTree (ident: String) : TestTree → String
  | TestTree.test config succeded message explanation =>
    let mark := if succeded
                  then prettifier "[PASS]" (Colorized.color Color.Green) config
                  else prettifier "[FAIL]" (Colorized.color Color.Red) config
    let explanation := match explanation with
      | some explanation => if succeded then "" else s!"\n{ident}  {explanation}"
      | _                => ""
    s!"{ident}{mark} {message}{explanation}\n"
  | TestTree.group _ name tests =>
    let tests := tests.map (TestTree.reprTree (ident ++ "  "))
    s!"{ident}{name}\n{String.join $ Array.toList tests}"

partial def TestTree.countFailures : TestTree → Nat
  | TestTree.test _ succeded _ _ => if succeded then 0 else 1
  | TestTree.group _ _ tests     => tests.foldl (λ acc test => acc + test.countFailures) 0

partial def TestTree.failed : TestTree → Bool
  | TestTree.test _ succeded _ _ => !succeded
  | TestTree.group _ _ tests     => tests.any TestTree.failed

instance : ToString TestTree where
  toString tree := tree.reprTree ""

def displayMultiple (tree: Array TestTree) : String := tree
  |> Array.toList
  |> List.map ToString.toString
  |> String.join

end Specs.Display
