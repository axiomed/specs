/-! Data structure for representing the execution of a test suite. -/

namespace Specs.Display

inductive TestTree where
  | group (name: String) (tests: Array TestTree)
  | test (succeded: Bool) (message: String) (explanation: Option String)
  deriving Inhabited

partial def TestTree.reprTree (ident: String) : TestTree → String
  | TestTree.test succeded message explanation =>
    let mark := if succeded then "🗸" else "🗙"
    let explanation := match explanation with
      | some explanation => if succeded then "" else s!"\n{ident}  {explanation}"
      | _                => ""
    s!"{ident}{mark} {message}{explanation}\n"
  | TestTree.group name tests =>
    let tests := tests.map (TestTree.reprTree (ident ++ "  "))
    s!"{ident}{name}\n{String.join $ Array.toList tests}"

partial def TestTree.countFailures : TestTree → Nat
  | TestTree.test succeded _ _ => if succeded then 0 else 1
  | TestTree.group _ tests     => tests.foldl (λ acc test => acc + test.countFailures) 0

partial def TestTree.failed : TestTree → Bool
  | TestTree.test succeded _ _ => !succeded
  | TestTree.group _ tests     => tests.any TestTree.failed

instance : ToString TestTree where
  toString tree := tree.reprTree ""

def displayMultiple (tree: Array TestTree) : String := tree
  |> Array.toList
  |> List.map ToString.toString
  |> String.join

end Specs.Display
