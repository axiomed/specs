namespace Specs

/-! Module for `Runner` configuration -/

structure Config where
  verbose  : Bool := false
  bail     : Bool := false
  noColors : Bool := true

instance : Inhabited Config := ⟨{
  verbose  := false
  bail     := false
  noColors := true
}⟩

end Specs
