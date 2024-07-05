import Lake
open Lake DSL

package Http where

module_data alloy.c.o.export : BuildJob FilePath
module_data alloy.c.o.noexport : BuildJob FilePath

open Lean
lean_lib Http where
  precompileModules := true
  nativeFacets := fun shouldExport =>
    if shouldExport then
      #[Module.oExportFacet, `alloy.c.o.export]
    else
      #[Module.oNoExportFacet, `alloy.c.o.noexport]

require CaseInsensitive from git "https://github.com/axiomed/CaseInsensitive.lean.git"
require alloy from git "https://github.com/tydeu/lean4-alloy.git"
require LibUV from git "https://github.com/algebraic-sofia/lean-libuv.git" @ "socket-fix"
require Parse from git "https://github.com/axiomed/Parse.lean.git"
require Time from git "https://github.com/axiomed/Time.lean.git"

meta if get_config? env = some "dev" then
  require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"

open Lean

def mkArrayLit (lvl : Level) (type : Expr) (l : List Expr) : Expr :=
  let empty := Expr.app (Expr.const ``Array.empty [lvl]) type
  let push r h := mkAppN (Expr.const ``Array.push [lvl]) #[type, r, h]
  l.foldl push empty

def elabRunPkgConfig (stx : Syntax) (args : Array String) : Elab.TermElabM Expr := do
  Lean.withRef stx do
    match ← (IO.Process.output { cmd := "pkg-config", args }).toBaseIO with
    | .ok out =>
      if out.exitCode != 0 then
        throwErrorAt stx "pkg-config failed: {out.exitCode}"
      let libParts := out.stdout.splitOn
      let stringType := Expr.const ``String []
      libParts
          |>.map (mkStrLit ·.trimRight)
          |> mkArrayLit .zero stringType
          |> pure
    | .error _ =>
        throwErrorAt stx "Could not run pkg-config"

syntax:lead (name := libuvLibsElab) "libuvLibs" : term

@[term_elab libuvLibsElab]
def elabLibUVLibs : Lean.Elab.Term.TermElab := fun stx _expectedType? =>
  elabRunPkgConfig stx #["--libs", "libuv"]

@[default_target]
lean_exe exampl where
  root := `Example
  moreLinkArgs := libuvLibs
