import Specs

open Specs
open Specs.Matchers

def main := runCli do
  describe "check if numbers are different" do
    it "numbers greater than 0 but less than 10" do
      isEqual 1 1
      isGreater 3 1
    it "numbers greater than 10" do
      isGreater 20 15
    failing "numbers less than 0" do
      isGreater 3 0
