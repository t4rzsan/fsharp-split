# F# Split
Split is an F# module for filtering lists but without losing the data that you filter await.  If you use F#'s built in List.filter function, you have no way of keeping track of the items in the list that you filter away.  With Split you can keep track of both the items that are included in the filter and the items that are excluded.  

Take a look at Demo.fsx for an example on how to use Split.  Also, check out this write-up on dev.to: [https://dev.to/t4rzsan/filtering-lists-in-f-without-throwing-data-away-a7o](https://dev.to/t4rzsan/filtering-lists-in-f-without-throwing-data-away-a7o).
