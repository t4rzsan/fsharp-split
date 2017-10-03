type FilterResult<'a> = {
    Name: string;
    Included: 'a;
    Excluded: 'a;
}

type FilterResultBuilder(printer) =
    member this.Printer = printer

    member this.Bind(m, f) = 
        m.Included |> f

    member this.Return(x) =
        x

let filter = FilterResultBuilder()

let filterEven x =
    let f n = (n % 2) = 0;

    {
        Name= "even";
        Included = x |> Seq.filter f;
        Excluded = x |> Seq.filter (not << f)
    }

let map mapping x =
    {
        Name = "map";
        Included = x |> Seq.map mapping;
        Excluded = x |> Seq.map mapping;
    }

let testFilter x =
    filter
        {
            let! a = x |> filterEven
            let! b = a |> map (sprintf "Nummer %d")
            return b
        }    

let evens =
    seq { 1 .. 7 }
    |> filterEven

evens.Included
|> Seq.iter(fun n -> printfn "%d" n)


