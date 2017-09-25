#load @"Split.fsx"

// Demo
{
    Split.Included = [ 1; 2; 3; 10; 354; 234; 23; 45 ];
    Split.Excluded = Seq.empty<int>;
}
|> Split.split (fun i -> (i % 2) = 0)
|> Split.iter (fun i -> printfn "%d" i)
|> Split.clearAndMap (fun i -> i * i)
|> Split.iter (fun i -> printfn "%d" i)
