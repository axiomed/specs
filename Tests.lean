import Specs
import Specs.Macro

open Specs
open Specs.Matchers
open Specs.Macro

/- Testing just using the `run` function -/
#test do
  it "1 equals 1" do
    prop "is equal" (1 = 1)
  failing "1 diff 2" do
    prop "is equal" (1 = 2)

def main := runCli do
  describe "check if numbers are different" do
    it "numbers greater than 0 but less than 10" do
      isEqual 1 1
      isGreater 3 1
    it "numbers greater than 10" do
      isGreater 20 23
    failing "numbers less than 0" do
      isGreater (-1) (-2)
    failing "random tests that should fail" do
      isEqual 1 2
      assert "this should fail" false true
      assert "this should fail" true false
      isGreater 1 2
      isLess 2 1
    describe "specific numbers" do
      it "1 and 1" do
        isEqual 1 2
      failing "1 and 2" do
        isEqual 1 2
  describe "empty values" do
    it "empty string" do
      isEmpty ""
    failing "empty list" do
      isEmpty [1]
    it "empty option" do
      isEmpty (none : Option Int)
    it "empty list" do
      isEmpty ([] : List Int)
    failing "empty list" do
      isEmpty [1]
    failing "empty option" do
      isEmpty (some 1)
  describe "matchers" do
    it "skipper" do
      skip
    failing "fails" do
      fail "it fails"
  describe "props" do
    it "1 equals 1" do
      prop "1 equals 1" (1 = 1)
    failing "1 equals 2" do
      prop "1 equals 2" (1 = 2)
