module Split

type FilterResult<'a> = {
    Included: 'a seq;
    Excluded: 'a seq;
    }

type FileName = FileName of string

let split filter previousResults =
    let negatedFilter = not << filter
    
    {
        Included = previousResults.Included |> Seq.filter filter;
        Excluded = previousResults.Included |> Seq.filter negatedFilter;
    }

let map mapper previousResults =
    {
        Included = previousResults.Included |> Seq.map mapper;
        Excluded = previousResults.Excluded |> Seq.map mapper;
    }

let iter action previousResults =
    previousResults.Included |> Seq.iter action
    previousResults.Excluded |> Seq.iter action

    previousResults

let clear previousResults =
    {
        previousResults with Excluded = Seq.empty
    }    

let iterAndClear fileName = (iter fileName) >> clear

let clearAndMap mapper = clear >> (map mapper)
