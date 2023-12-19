import Specs.Core
import Specs.Runner
import Specs.Compose
import Cli

open Specs.Core Specs.Runner Cats.Trans Cli

namespace Specs

/-- Parses and validates the arguments of the command. -/
private def runCmd (specs: Specs) (parsed : Parsed) : IO UInt32 := do
  let config := Config.mk
      (verbose := parsed.hasFlag "verbose")
      (bail    := parsed.hasFlag "bail")
  execute config specs

/-- The command to run the tests of the project. -/
private def specsCmd (specs: Specs) : Cmd := `[Cli|
  testsCmd VIA runCmd specs; ["0.0.1"]
  "A utility to run tests for a project"

  FLAGS:
    verbose; "Prints more information about each test"
    bail; "Stops the test suite after the first failure"
]

/-- Runs a test suite using the given arguments. -/
def runCli (suite: Specs) (args : List String) : IO UInt32 :=
  (specsCmd suite).validate args

end Specs
