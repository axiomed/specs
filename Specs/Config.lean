/-! Module for `Runner` configuration -/

namespace Specs

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
