# Http.lean

[![CI](https://github.com/axiomed/Specs.lean/actions/workflows/push.yml/badge.svg)](https://github.com/axiomed/Specs.lean/actions/workflows/push.yml)

A WIP implementation of a Test Framework for [Lean 4](https://github.com/leanprover/lean4).

## The Goal

The goal of this project is to provide Lean community a test framework inspired on 
[Mocha](https://mochajs.org/) and [HSpecs](https://hspec.github.io/).

The broader goal of Axiomed initiative is to build an ecosystem of tools in Lean 4 that software engineers need in their everyday tasks, such as HTTP libs, async frameworks, DB connectors, parsers etc.

If you think Lean 4 can shine not only as a theorem prover, but as a general purpose language - you're welcome to join the organization!

## Usage

Add Http to your `lakefile.lean`:

```lean
require Specs from git "https://github.com/axiomed/Specs.lean.git"
```

This is a simple test suite for numbers:

```lean
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
```