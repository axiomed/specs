import Lake
open Lake DSL

package «Specs» where
  -- add package configuration options here

lean_lib «Specs» where
  -- add library configuration options here

lean_exe «tests» where
  root := `Tests
  supportInterpreter := true
