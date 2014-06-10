module HundredDoorsTests

open NUnit.Framework
open FsUnit
open HundredDoors

[<Test>]
let ``All doors are initially closed`` () = 
    makeClosedDoors(3) |> should equal [Closed; Closed; Closed]

[<Test>]
let ``Toggle opens closed door`` () =
    toggleDoor Closed |> should equal Open

[<Test>]
let ``Toggle  closes open door`` () =
    toggleDoor Open |> should equal Closed

[<Test>]
let ``toggle every second door opens the correct doors`` () =
    let toggleFunction = toggleDoors 2
    toggleFunction [Closed; Closed; Closed; Closed]
        |> should equal [Closed; Open; Closed; Open] 

[<Test>]
let ``toggle every third door closes the correct doors`` () =
    let toggleFunction = toggleDoors 3
    toggleFunction [Open; Open; Open; Open; Open; Open]
        |> should equal [Open; Open; Closed; Open; Open; Closed;]
        

[<Test>]
let ``Full walk of 4 doors produces correct result`` () =
    let doors = makeClosedDoors 4
    let manualDoorWalker = 
        (toggleDoors 1) >> (toggleDoors 2) >> (toggleDoors 3) >>  (toggleDoors 4)
    let expectedResult = doors |> manualDoorWalker
    let actualResult = doors |> walkDoors
    actualResult |> should equal expectedResult