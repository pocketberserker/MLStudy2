module MLStudy2.Program

open System.Reflection

type Line1 = Line1
type Line2 = Line2
type Line3 = Line3
type Line4 = Line4
type Line5 = Line5
type Line6 = Line6
type Line7 = Line7

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
  member __.Run(f: unit -> Line7 * Async<_>) =
    f ()
    |> snd
    |> Async.RunSynchronously
  member __.Yield(x) = x
  [<CustomOperation("モナドは")>]
  member __.Line1(()) =
    printName Line1 (async { () })
  [<CustomOperation("単なる")>]
  member __.Line2((Line1, x)) =
    printName Line2 x
  [<CustomOperation("自己関手の")>]
  member __.Line3((Line2, x)) =
    printName Line3 x
  [<CustomOperation("圏における")>]
  member __.Line4((Line3, x)) =
    printName Line4 x
  [<CustomOperation("モノイド対象だよ")>]
  member __.Line5((Line4, x)) =
    printName Line5 x
  [<CustomOperation("何か問題でも")>]
  member __.Line6((Line5, x)) =
    printName Line6 x
  [<CustomOperation("?")>]
  member __.Line7((Line6, x)) =
    printName Line7 x

let 詠唱 = MonadAriaBuilder()

[<EntryPoint>]
let main _ =
  詠唱 {
    モナドは
    単なる
    自己関手の
    圏における
    モノイド対象だよ
    何か問題でも
    ``?``
  }
  0
