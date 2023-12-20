/-! Module for `Runner` configuration -/

namespace Specs

structure Config where
  verbose  : Bool := false
  bail     : Bool := false
  noColors : Bool := true
  deriving Inhabited

end Specs
