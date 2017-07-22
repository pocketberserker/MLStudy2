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
  let typ = ProvidedTypeDefinition(asm, ns, "AriaProvider", Some typeof<obj>)
  do
    let parameters = [
      ProvidedStaticParameter("filePath", typeof<string>)
    ]
    typ.DefineStaticParameters(parameters, fun typeName (args: obj[]) ->

      let filePath = unbox<string> args.[0]
      if not <| File.Exists(filePath) then failwithf "File not found: %s" filePath
      let lines = File.ReadAllLines(filePath)

      let typ = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
      let builderType = ProvidedTypeDefinition("Builder", Some typeof<obj>)
      let ctor = ProvidedConstructor(parameters = [], InvokeCode = fun _ -> <@@ () @@>)
      builderType.AddMember(ctor)
      typ.AddMember(builderType)

      if Array.isEmpty lines then failwithf "Empty Text: %s" filePath
      let ts =
        lines
        |> Array.mapi (fun n l ->
          let name = sprintf "Line%d" n
          let numType = ProvidedTypeDefinition(name, Some typeof<obj>)
          let ctor = ProvidedConstructor(parameters = [], InvokeCode = fun _ -> <@@ () @@>)
          numType.AddMember(ctor)
          let newObj _ = Expr.NewObject(ctor, [])
          let prop =
            ProvidedProperty(
              propertyName = name,
              IsStatic = true,
              propertyType = numType,
              GetterCode = newObj
            )
          typ.AddMembers(
            [
              numType :> MemberInfo
              prop :> MemberInfo
            ]
          )
          (numType, prop, l)
        )
      let head = Array.head ts
      let (last, _, _) = Array.last ts

      builderType.AddMemberDelayed(fun () -> Generator.genYield ())
      builderType.AddMemberDelayed(fun () -> head |||> Generator.genLine0)

      ts
      |> Array.pairwise
      |> Array.iter (fun t ->
        builderType.AddMemberDelayed(fun () -> t ||> Generator.genLine)
      )

      builderType.AddMemberDelayed(fun () -> last |> Generator.genDelay)
      builderType.AddMemberDelayed(fun () -> last |> Generator.genRun)

      typ
    )
    this.AddNamespace(ns, [typ])

[<assembly:TypeProviderAssembly>]
do ()
