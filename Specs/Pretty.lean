import Specs.Config
import Colorized.Console.Colorized

open Specs

def prettifier (label : String) (colorized : String → String) (config : Config) :=
  if config.noColors
    then label
    else colorized label
