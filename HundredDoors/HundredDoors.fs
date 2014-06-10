module HundredDoors

type Door = 
    |Closed 
    |Open

let makeClosedDoors = function 
    |numDoors -> List.init numDoors (fun n -> Closed)
    

let toggleDoor = function 
    |Closed -> Open
    |Open -> Closed

let toggleDoors interval doors = 
    
    let toggleNthDoor i door = 
        if (i+1) % interval = 0
        then toggleDoor door
        else door

    doors |> List.mapi toggleNthDoor 

let walkDoors doors = 
    let size = doors |> List.length
    
    let rec walkHelper currentIteration doors = 
        let currentToggler = toggleDoors currentIteration
        match currentIteration with 
        |i when i = size -> currentToggler doors
        |_ -> 
            let nextInput = currentToggler doors
            walkHelper (currentIteration + 1) nextInput

    walkHelper 1 doors

let finder n door = 
    match door with 
    |Open -> n
    |_ -> 