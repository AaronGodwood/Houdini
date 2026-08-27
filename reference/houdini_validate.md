# Check a configuration before generating a document

Runs every check the app shows in its validation panel, without opening
the app and without writing anything. Use it in a script or CI step to
fail early, with the whole list of problems at once, rather than
discovering them one at a time during a run.

## Usage

``` r
houdini_validate(
  input_doc,
  input_sheet,
  file_location,
  figure_location = NULL,
  quiet = FALSE
)
```

## Arguments

- input_doc:

  Path to the template (or previously generated) .docx

- input_sheet:

  Path to a config .xlsx (see
  [`read_xlsx()`](https://aarongodwood.github.io/Houdini/reference/read_xlsx.md))
  or a data.frame with Bookmark/Table columns plus optional filter
  columns

- file_location:

  Folder containing the table .rtf files named in the config

- figure_location:

  Folder containing the figure .rtf files named in the config (defaults
  to table location)

- quiet:

  Suppress the printed summary; the findings are always returned

## Value

Invisibly, a data.frame of findings with one row per problem and columns
`row`, `severity`, `bookmark`, `table`, `message` and `hint`. A clean
config returns a zero-row data.frame.

## Details

The same rules drive the app's warnings panel, so a config that
validates cleanly here will validate cleanly there.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fail a CI step on any error
problems <- houdini_validate("report.docx", "config.xlsx", "tables/")
if (any(problems$severity == "error")) stop("config is not ready")
} # }
```
