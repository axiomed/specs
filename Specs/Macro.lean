import Lean

import Specs.Runner
import Specs.Config
import Specs.Core
import Specs.Display

/-! Module for macros for execution of tests -/

namespace Specs.Macro

def executeTest (term : Specs.Core.Specs) : Lean.Elab.TermElabM Unit :=
  let result := Specs.Runner.executePure default term
  let string := Specs.Display.displayMultiple result
  if Array.any result (·.testTree.failed) then
    throwError string
  else
    Lean.logInfo string

macro "#test " term:term : command => `(#eval Specs.Macro.executeTest $term)

end Specs.Macro
