import Specs.Config
import Specs.Pretty

namespace Specs.Display

/-! Data structure for representing the execution of a test suite. -/

inductive TestTree where
  | group (name: String) (tests: Array TestTree)
  | test (succeded: Bool) (message: String) (explanation: Option String)
  deriving Inhabited

structure ConfigurableTestTree where
  testTree : TestTree
  config : Config
  deriving Inhabited

partial def ConfigurableTestTree.reprTree (ident: String) (configurableTestTree : ConfigurableTestTree) : String :=
  match configurableTestTree.testTree with
  | TestTree.test succeded message explanation =>
      let mark := if succeded
        then prettifier "[PASS]" (Colorized.color Color.Green) configurableTestTree.config
        else prettifier "[FAIL]" (Colorized.color Color.Red) configurableTestTree.config
    let explanation := match explanation with
      | some explanation => if succeded then "" else s!"\n{ident}  {explanation}"
      | _                => ""
    s!"{ident}{mark} {message}{explanation}\n"
  | TestTree.group name tests =>
    let tests := tests.map (λ x => ConfigurableTestTree.reprTree (ident ++ "  ") (ConfigurableTestTree.mk x configurableTestTree.config))
    s!"{ident}{name}\n{String.join $ Array.toList tests}"

partial def TestTree.countFailures : TestTree → Nat
  | TestTree.test succeded _ _ => if succeded then 0 else 1
  | TestTree.group _ tests     => tests.foldl (λ acc test => acc + test.countFailures) 0

partial def TestTree.failed : TestTree → Bool
  | TestTree.test succeded _ _ => !succeded
  | TestTree.group _ tests     => tests.any TestTree.failed

instance : ToString ConfigurableTestTree where
  toString tree := tree.reprTree ""

def displayMultiple (tree: Array ConfigurableTestTree) : String := tree
  |> Array.toList
  |> List.map ToString.toString
  |> String.join

end Specs.Display
