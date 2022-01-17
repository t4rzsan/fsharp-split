#load @"Split.fsx"

let printer title l =
    printfn "%s: %A" title l

[ 1; 2; 3; 10; 354; 234; 23; 45 ]
|> Split.create (fun x -> x >= 10)
|> Split.outputExcluded (printer "Less than 10")
|> Split.recreate (fun x -> (x % 2) = 0)
|> Split.outputExcluded (printer "Odd")
|> Split.swap
|> Split.outputExcluded (printer "Even")
