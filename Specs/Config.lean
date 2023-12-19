/- Module for `Runner` configuration -/

namespace Specs

structure Config where
  verbose : Bool := false
  bail    : Bool := false
  deriving Inhabited

end Specs
