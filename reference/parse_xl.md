# Parse a excel config into config + selections

Shared by the app's Excel import and [`apparate()`](apparate.md).
Requires Bookmark and Table columns (case-insensitive); recognises
optional Parameters, Timelines, ExcludedColumns, ExcludedRows and
ExcludedHeaderRows columns, all semicolon-separated. Any .rtf extension
on Table values is stripped.

## Usage

``` r
parse_xl(xl)
```

## Arguments

- xl:

  A data.frame (e.g. from readxl) with the columns above

## Value

list(config = data.frame(Bookmark, Table), selections = list keyed by
row index as character, matching the app's table_selections format)
