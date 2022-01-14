module Split =
    type Split<'a> =
        { Included: 'a list
          Excluded: 'a list }

    /// Split a list into two lists, one containing the included items, and the other containing the excluded items.
    let create predicate (l: 'a list) =
        let rec doSplit (split: Split<'a>) (l: 'a list) =
            match l with
            | [] -> split
            | x :: xs ->
                if x |> predicate then
                    doSplit { split with Included = [x] |> List.append split.Included } xs
                else
                    doSplit { split with Excluded = [x] |> List.append split.Excluded } xs
        
        doSplit { Included = []; Excluded = [] } l

    let ofList list =
        { Included = list
          Excluded = [] }    

    let recreate predicate split =
        split.Included |> create predicate

    let decompose split =
        split.Included

    let switch split =
        { Included = split.Excluded
          Excluded = split.Included }

    let merge split =
        { Included = split.Excluded |> List.append split.Included
          Excluded = [] }

    let clear split =
        { split with Excluded = [] }

    let map mapping split =
        { Included = split.Included |> List.map mapping
          Excluded = split.Excluded |> List.map mapping }

    let choose chooser split =
        { Included = split.Included |> List.choose chooser
          Excluded = split.Excluded |> List.choose chooser }

    let filter predicate split =
        { Included = split.Included |> List.filter predicate
          Excluded = split.Excluded |> List.filter predicate }

    let sortBy projection split =
        { Included = split.Included |> List.sortBy projection
          Excluded = split.Excluded |> List.sortBy projection }

    let sortByDescending projection split =
        { Included = split.Included |> List.sortByDescending projection
          Excluded = split.Excluded |> List.sortByDescending projection }

    let collect mapping split =
        { Included = split.Included |> List.collect mapping
          Excluded = split.Excluded |> List.collect mapping }

    let outputIncluded printer split =
        split.Included
        |> printer

        split

    let outputExcluded printer split =
        split.Excluded
        |> printer

        split

    let outputIncludedAndClear printer =
        (outputIncluded printer) >> clear
