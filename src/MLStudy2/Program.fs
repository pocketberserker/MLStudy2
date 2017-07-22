module MLStudy2.Program

open System.Reflection

type Line0 = Line0
type Line1 = Line1
type Line2 = Line2
type Line3 = Line3
type Line4 = Line4
type Line5 = Line5
type Line6 = Line6

let inline operatorName () =
  (MethodBase.GetCurrentMethod().GetCustomAttribute(typeof<CustomOperationAttribute>) :?> CustomOperationAttribute).Name

let inline printName line before =
  let name = operatorName ()
  let print = async {
    do! before
    do! Async.Sleep 1000
    printfn "%s" name
  }
  (line, print)

type MonadAriaBuilder() =
  member __.Delay(f: unit -> _) = f
  member __.Run(f: unit -> Line6 * Async<_>) =
    f ()
    |> snd
    |> Async.RunSynchronously
  member __.Yield(()) = ()
  [<CustomOperation("モナドは")>]
  member __.Line0(()) =
    printName Line0 (async { () })
  [<CustomOperation("単なる")>]
  member __.Line1((Line0, x)) =
    printName Line1 x
  [<CustomOperation("自己関手の")>]
  member __.Line2((Line1, x)) =
    printName Line2 x
  [<CustomOperation("圏における")>]
  member __.Line3((Line2, x)) =
    printName Line3 x
  [<CustomOperation("モノイド対象だよ")>]
  member __.Line4((Line3, x)) =
    printName Line4 x
  [<CustomOperation("何か問題でも")>]
  member __.Line5((Line4, x)) =
    printName Line5 x
  [<CustomOperation("?")>]
  member __.Line6((Line5, x)) =
    printName Line6 x

let 詠唱 = MonadAriaBuilder()

let example1 () = 詠唱 {
  モナドは
  単なる
  自己関手の
  圏における
  モノイド対象だよ
  何か問題でも
  ``?``
}

type MonadAriaBuilder with
  member __.Zero() =
    let a =
      typeof<MonadAriaBuilder>.GetMethods()
      |> Array.choose (fun x ->
        let attr = x.GetCustomAttribute<CustomOperationAttribute>()
        if isNull (box attr) then None
        else Some attr.Name
      )
      |> Array.fold (fun a name -> async {
        do! a
        do! Async.Sleep 1000
        printfn "%s" name
      }
      ) (async.Zero())
    (Line6, a)

let example2 () = 詠唱 { () }

[<Literal>]
let monadPath = __SOURCE_DIRECTORY__ + @"\Monad.txt"

open MLStudy2.TypeProviders

type MonadAriaBuilder2 = AriaProvider<monadPath>

let 詠唱2 = MonadAriaBuilder2()

type MonadAriaBuilder2 with
  [<CustomOperation("quick")>]
  member __.ExtraLine(()) =
    printName MonadAria.Line6 (async { return () })

// TypeProviderで生成した場合はカスタムオペレーターと認識されない…？
let example3 () = 詠唱2 {
  quick
  //モナドは
  //単なる
  //自己関手の
  //圏における
  //モノイド対象だよ
  //何か問題でも
  //``?``
}

[<EntryPoint>]
let main _ =

  let f (x, y) =
    printfn "%A" x
    printfn "%A" y

  let a = typeof<MonadAriaBuilder>.GetMethods()
  typeof<MonadAriaBuilder2>.GetMethods()
  |> Array.choose (fun x ->
    let a1 = x.GetCustomAttribute<CustomOperationAttribute>()
    a |> Array.tryFind (fun y ->
      let a2 = y.GetCustomAttribute<CustomOperationAttribute>()
      if isNull (box a1) || isNull (box a2) then false
      else a1.Name = a2.Name
    )
    |> Option.map (fun y -> (y, x))
  )
  |> Array.iter (f)

  //example1 ()
  //example2 ()
  //example3 ()

  0
