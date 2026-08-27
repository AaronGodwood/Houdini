# Preview a single RTF table or figure

Renders one RTF file to a standalone HTML page and opens it, without
starting the Shiny app. Useful for checking how a table parses,
confirming that a filter selects what you expect, or inspecting a file
that failed during a run.

## Usage

``` r
houdini_preview(
  path,
  parameters = NULL,
  timelines = NULL,
  levels = NULL,
  excluded_cols = NULL,
  excluded_rows = NULL,
  hide_data = FALSE,
  browse = interactive(),
  file = tempfile(fileext = ".html")
)
```

## Arguments

- path:

  Path to the .rtf file

- parameters:

  Character vector of parameter values to keep (NULL = all)

- timelines:

  Character vector of timepoint labels to keep (NULL = all)

- levels:

  Character vector of indent level titles to keep (NULL = all)

- excluded_cols:

  Integer vector of 1-based column indices to drop

- excluded_rows:

  Integer vector of data-row IDs to drop

- hide_data:

  Replace values with XX, for sharing a layout without data

- browse:

  Open the page in a browser or the RStudio viewer. When FALSE the file
  is written but not opened.

- file:

  Where to write the HTML. Defaults to a temporary file.

## Value

Invisibly, the path to the HTML file

## Details

Filters accept the same values as
[`apparate()`](https://aarongodwood.github.io/Houdini/reference/apparate.md)'s
config columns, so a selection can be tried here before being committed
to a workbook.

## Examples

``` r
if (FALSE) { # \dontrun{
houdini_preview("tables/t_14_1_1.rtf")
houdini_preview("tables/t_14_1_1.rtf", parameters = "Cholesterol")
} # }
```
