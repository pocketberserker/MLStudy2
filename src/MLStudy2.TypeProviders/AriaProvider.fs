namespace MLStudy2.TypeProviders

open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

[<TypeProvider>]
type AriaProvider(cfg: TypeProviderConfig) as this =
  inherit TypeProviderForNamespaces()

  let asm = Assembly.GetExecutingAssembly()
  let ns = "MLStudy2.TypeProviders"
  let tempAsm = ProvidedAssembly (Path.ChangeExtension (Path.GetTempFileName(), ".dll"))
  let typ = ProvidedTypeDefinition(asm, ns, "AriaProvider", None, IsErased = false)
  do
    let parameters = [
      ProvidedStaticParameter("filePath", typeof<string>)
    ]
    typ.DefineStaticParameters(parameters, fun typeName (args: obj[]) ->

      let filePath = unbox<string> args.[0]
      if not <| File.Exists(filePath) then failwithf "File not found: %s" filePath
      let lines = File.ReadAllLines(filePath)

      let typ = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, HideObjectMethods = true, IsErased = false)
      let ctor = ProvidedConstructor(parameters = [], InvokeCode = fun _ -> <@@ () @@>)
      typ.AddMember(ctor)

      if Array.isEmpty lines then failwithf "Empty Text: %s" filePath
      //let ts =
      //  lines
      //  |> Array.mapi (fun n l ->
      //    let name = sprintf "Line%d" n
      //    let numType = ProvidedTypeDefinition(name, Some typeof<obj>, IsErased = false)
      //    let ctor = ProvidedConstructor(parameters = [], InvokeCode = fun _ -> <@@ () @@>)
      //    numType.AddMember(ctor)
      //    let newObj _ = Expr.NewObject(ctor, [])
      //    let prop =
      //      ProvidedProperty(
      //        propertyName = name,
      //        IsStatic = true,
      //        propertyType = numType,
      //        GetterCode = newObj
      //      )
      //    typ.AddMembers(
      //      [
      //        numType :> MemberInfo
      //        prop :> MemberInfo
      //      ]
      //    )
      //    (numType, prop, l)
      //  )
      //let head = Array.head ts
      //let (last, _, _) = Array.last ts

      //builderType.AddMemberDelayed(fun () -> Generator.genYield ())
      //builderType.AddMemberDelayed(fun () -> head |||> Generator.genLine0)
      typ.AddMemberDelayed(fun () -> Generator.genYield ())
      typ.AddMemberDelayed(fun () -> Array.head lines |> Generator.genLine0)

      //ts
      //|> Array.pairwise
      //|> Array.iter (fun t ->
      //  builderType.AddMemberDelayed(fun () -> t ||> Generator.genLine)
      //)
      lines
      |> Array.tail
      |> Array.iter (fun l ->
        typ.AddMemberDelayed(fun () -> Generator.genLine l)
      )

      //builderType.AddMemberDelayed(fun () -> last |> Generator.genDelay)
      //builderType.AddMemberDelayed(fun () -> last |> Generator.genRun)
      typ.AddMemberDelayed(fun () -> Generator.genDelay())
      typ.AddMemberDelayed(fun () -> Generator.genRun())

      tempAsm.AddTypes [typ]

      typ
    )
    tempAsm.AddTypes [typ]
    this.AddNamespace(ns, [typ])

[<assembly:TypeProviderAssembly>]
do ()
