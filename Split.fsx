type Split<'a> =
    { Included: 'a list
      Excluded: 'a list }

/// Split a list into two lists, one containing the included items, and the other containing the excluded items.
let create predicate (l: 'a list) =
    let (included, excluded) = List.partition predicate l
    { Included = included
      Excluded = excluded }

// Create a new split from a list and create a split with Included and Excluded
// appended from the existing split and the new split.
let append predicate split source =
    let newSplit = create predicate source
    { Included = newSplit.Included |> List.append split.Included
      Excluded = newSplit.Excluded |> List.append split.Excluded }

let choose chooser split =
    { Included = split.Included |> List.choose chooser
      Excluded = split.Excluded |> List.choose chooser }

/// Create a Split with the Excluded list cleared.
let clear split =
    { split with Excluded = [] }

let collect mapping split =
    { Included = split.Included |> List.collect mapping
      Excluded = split.Excluded |> List.collect mapping }

/// Return the Included list of a Split.
let decompose split =
    split.Included

let distinct split =
    { Included = split.Included |> List.distinct
      Excluded = split.Excluded |> List.distinct }

let distinctBy projection split =
    { Included = split.Included |> List.distinctBy projection
      Excluded = split.Excluded |> List.distinctBy projection }

let filter predicate split =
    { Included = split.Included |> List.filter predicate
      Excluded = split.Excluded |> List.filter predicate }

let groupBy projection split =
    { Included = split.Included |> List.groupBy projection
      Excluded = split.Excluded |> List.groupBy projection }

let indexed split =
    { Included = split.Included |> List.indexed
      Excluded = split.Excluded |> List.indexed }

let map mapping split =
    { Included = split.Included |> List.map mapping
      Excluded = split.Excluded |> List.map mapping }

/// Create a split with the Excluded list appended to Included list.
/// The Exluded list will be empty.
let merge split =
    { Included = split.Excluded |> List.append split.Included
      Excluded = [] }

/// Create a Split from a list.  The Included property will contain the items in the input list.
let ofList source =
    { Included = source
      Excluded = [] }

/// Create a Split from a sequence.  The Included property will contain the items in the input sequence.
let ofSeq source =
    { Included = Seq.toList source
      Excluded = [] }

let outputExcluded printer split =
    split.Excluded
    |> printer

    split

/// Do a new split from the Included list of an existing Split.
let outputIncluded printer split =
    split.Included
    |> printer

    split

let outputIncludedAndClear printer =
    (outputIncluded printer) >> clear

let recreate predicate split =
    split.Included |> create predicate

let rev split =
    { Included = split.Included |> List.rev
      Excluded = split.Excluded |> List.rev }

let sort split =
    { Included = split.Included |> List.sort
      Excluded = split.Excluded |> List.sort }

let sortBy projection split =
    { Included = split.Included |> List.sortBy projection
      Excluded = split.Excluded |> List.sortBy projection }

let sortByDescending projection split =
    { Included = split.Included |> List.sortByDescending projection
      Excluded = split.Excluded |> List.sortByDescending projection }

let sortDescending split =
    { Included = split.Included |> List.sortDescending
      Excluded = split.Excluded |> List.sortDescending }

let splitAndOutputExcluded predicate printer =
    create predicate
    >> outputExcluded printer

let splitAndOutputAndClearExcluded predicate printer =
    create predicate
    >> outputExcluded printer
    >> clear

/// Create a Split with Included and Excluded swapped.
let swap split =
    { Included = split.Excluded
      Excluded = split.Included }

