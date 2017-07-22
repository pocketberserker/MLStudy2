module MLStudy2.TypeProviders.Loader

open System
open System.IO
open System.Reflection
open FSharp.Reflection
open Microsoft.FSharp.Core.CompilerServices

let publicTypes (asm: Assembly) =
  asm.GetTypes()
  |> Seq.filter (fun typ -> typ.IsPublic)

let publicNestedTypes (typ: Type) =
  typ.GetNestedTypes()
  |> Seq.filter (fun typ -> typ.IsNestedPublic)

let collectModules (asm: Assembly) =
  seq {
    let tops = publicTypes asm
    yield! tops
    yield! tops |> Seq.collect publicNestedTypes
  }
  |> Seq.filter FSharpType.IsModule
