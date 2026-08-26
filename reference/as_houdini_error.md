# Return `e` unchanged if it is already a houdini_error, otherwise wrap it

Errors raised deep in the pipeline (or by base R) carry no hint and are
often meaningless to the user e.g. "subscript out of bounds" tells them
nothing about which file or bookmark is at fault. Wrapping preserves the
orginal error whilst adding an actionable message and a hint. existing
houdini_errors are just passed as they are.

## Usage

``` r
as_houdini_error(e, constructor, context)
```

## Arguments

- e:

  A condition object

- constructor:

  A houdini_error construcor taking (context, cause)

- context:

  First argument for the constructor (e.g. a bookmark name)
