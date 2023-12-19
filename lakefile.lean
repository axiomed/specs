import Lake
open Lake DSL

package «Specs» where
  -- add package configuration options here

lean_lib «Specs» where
  -- add library configuration options here

lean_exe «specstest» where
  root := `Tests
  supportInterpreter := true

require Cats from git "https://github.com/axiomed/Cats.lean.git"
require Cli from git "https://github.com/leanprover/lean4-cli.git"
