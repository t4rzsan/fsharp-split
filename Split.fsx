module Split

type FilterResult<'a> = {
    Included: 'a seq;
    Excluded: 'a seq;
    }

let create elements =
    {
        Included = elements;
        Excluded = Seq.empty;
    }

/// Returns the Included sequence.
let decompose previousResults = 
    previousResults.Included

/// Splits input by the filter function.  Included will contain all elements in
/// Included of the input that pass the filter.  Excluded will be set to a sequence of 
/// all elements that do not pass the filter.  Excluded elements in the input
/// will be lost.
let split filter previousResults =
    let negatedFilter = not << filter
    {
        Included = previousResults.Included |> Seq.filter filter;
        Excluded = previousResults.Included |> Seq.filter negatedFilter;
    }

/// Appends the Excluded sequence to the included and clears Excluded.
let merge previousResults =
    {
        Included = previousResults.Excluded |> Seq.append previousResults.Included;
        Excluded = Seq.empty
    }

let map projection previousResults =
    {
        Included = previousResults.Included |> Seq.map projection;
        Excluded = previousResults.Excluded |> Seq.map projection;
    }

let sortBy projection previousResults =
    {
        Included = previousResults.Included |> Seq.sortBy projection;
        Excluded = previousResults.Excluded |> Seq.sortBy projection;
    }

let groupBy projection previousResults =
    {
        Included = previousResults.Included |> Seq.groupBy projection;
        Excluded = previousResults.Excluded |> Seq.groupBy projection;
    }

let collect mapping previousResults =
    {
        Included = previousResults.Included |> Seq.collect mapping;
        Excluded = previousResults.Excluded |> Seq.collect mapping;
    }

let iter action previousResults =
    previousResults.Included |> Seq.iter action
    previousResults.Excluded |> Seq.iter action

    previousResults

let output outputIncluded outputExcluded previousResults =
    previousResults.Included |> outputIncluded
    previousResults.Excluded |> outputExcluded

    previousResults

/// Clears the Excluded sequence.
let clear previousResults =
    {
        previousResults with Excluded = Seq.empty
    }    

let iterAndClear fileName = (iter fileName) >> clear

let splitAndClear filter = (split filter) >> clear

let outputAndClear outputIncluded outputExcluded = (output outputIncluded outputExcluded) >> clear

/// Clear Excluded and perform map on Included.
let clearAndMap projection = clear >> (map projection)

let clearAndGroupBy projection = clear >> (groupBy projection)

