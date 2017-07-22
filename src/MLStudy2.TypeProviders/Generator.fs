module MLStudy2.TypeProviders.Generator

open System
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes

let private (|MethodCall|) = function
| x::xs -> (x, xs)
| _ -> failwith "parameters mismatch"

let private (|Single|Pair|) = function
| [x] -> Single x
| [x; y] -> Pair(x, y)
| _ -> failwith "parameters mismatch"

//let private printName line (args: Expr list) =
let private printName (args: _ list) =
  let f =
    <@@
      let name =
        (MethodBase.GetCurrentMethod().GetCustomAttribute(typeof<CustomOperationAttribute>) :?> CustomOperationAttribute).Name
      async {
        do! ((%% args.[1]): Async<unit>)
        do! Async.Sleep 1000
        printfn "%s" name
      }
  @@>
  //Expr.NewTuple([Expr.PropertyGet(line); f])
  f

let private makeCustomOperationAttribute name =
  let t = typeof<CustomOperationAttribute>
  { new CustomAttributeData() with
    member __.Constructor = t.GetConstructor([| typeof<string> |])
    member __.ConstructorArguments = [| CustomAttributeTypedArgument(typeof<CustomOperationAttribute>, name) |] :> IList<_>
    member __.NamedArguments = [||] :> IList<_>
  }

let private makeCustomOperator name parameter retType code keyword =
  let p = ProvidedMethod(name, [parameter], retType, IsStaticMethod = false, InvokeCode = code)
  p.AddMethodAttrs(MethodAttributes.HideBySig)
  p.AddCustomAttribute(makeCustomOperationAttribute keyword)
  p

//let genLine0 (t: ProvidedTypeDefinition) (p: ProvidedProperty) name =
let genLine0 name =
  let parameter = ProvidedParameter("f", typeof<unit>)
  //let retType = FSharpType.MakeTupleType([| t; typeof<Async<unit>> |])
  let retType = typeof<Async<unit>>
  let inner _ =
    let f =
      <@@
        let name =
          (MethodBase.GetCurrentMethod().GetCustomAttribute(typeof<CustomOperationAttribute>) :?> CustomOperationAttribute).Name
        async {
          do! Async.Sleep 1000
          printfn "%s" name
        }
      @@>
    //Expr.NewTuple([Expr.PropertyGet(p); f])
    f
  //makeCustomOperator t.Name parameter retType inner name
  makeCustomOperator name parameter retType inner name

//let genLine ((t1, _, _): ProvidedTypeDefinition * _ * _) ((t2, p2, name):  ProvidedTypeDefinition * ProvidedProperty * string) =
let genLine name =
  //let parameter = ProvidedParameter("f", FSharpType.MakeTupleType([| t1; typeof<Async<unit>> |]))
  let parameter = ProvidedParameter("f", typeof<Async<unit>>)
  //let retType = FSharpType.MakeTupleType([| t2; typeof<Async<unit>> |])
  let retType = typeof<Async<unit>>
  //makeCustomOperator t2.Name parameter retType (printName p2) name
  makeCustomOperator name parameter retType printName name

let genYield () =
  let parameter = ProvidedParameter("f", typeof<unit>)
  let inner _ = <@@ () @@>
  ProvidedMethod("Yield", [parameter], typeof<unit>, IsStaticMethod = false, InvokeCode = inner)

//let genDelay (lineType: ProvidedTypeDefinition) =
let genDelay () =

  let parameter =
    //let retType = FSharpType.MakeTupleType([| lineType; typeof<Async<unit>> |])
    let retType = typeof<Async<unit>>
    let t = FSharpType.MakeFunctionType(typeof<unit>, retType)
    ProvidedParameter("f", t)

  let inner = function
  | MethodCall(_, Single f) -> f
  | exprs -> failwithf "oops!: %A" exprs

  ProvidedMethod("Delay", [parameter], parameter.ParameterType, IsStaticMethod = false, InvokeCode = inner)

//let genRun (lineType: ProvidedTypeDefinition) =
let genRun () =

 //let retType = FSharpType.MakeTupleType([| lineType; typeof<Async<unit>> |])
 let retType = typeof<Async<unit>>
 let parameter =
   let t = FSharpType.MakeFunctionType(typeof<unit>, retType)
   ProvidedParameter("f", t)

 let inner = function
 | MethodCall(_, Single f) ->
   //let a = Expr.TupleGet(Expr.Application(f, <@@ () @@>), 1)
   let a =Expr.Application(f, <@@ () @@>)
   <@@ ((%% a): Async<unit>) |> Async.RunSynchronously @@>
 | exprs -> failwithf "oops!: %A" exprs

 ProvidedMethod("Run", [parameter], typeof<unit>, IsStaticMethod = false, InvokeCode = inner)
