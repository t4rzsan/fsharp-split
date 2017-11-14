type FilterResult<'a> = {
    Included: 'a;
    Excluded: 'a;
}

type FilterResultBuilder() =
    member this.Bind(m, f) = 
        m.Included |> f

    member this.Return(x) =
        x

let filter = FilterResultBuilder()

let split predicate x =
    {
        Included = x |> Seq.filter predicate;
        Excluded = x |> Seq.filter (not << predicate)
    }

let map mapping x =
    {
        Included = x |> Seq.map mapping;
        Excluded = x |> Seq.map mapping;
    }

let testFilter x =
    filter
        {
            let! a = x |> split(fun n -> (n % 2) = 0)
            let! b = a |> map (sprintf "Nummer %d")
            return b
        }    

let evens =
    seq { 1 .. 7 }
    |> testFilter

evens
|> Seq.iter(fun n -> printfn "%A" n)


