# Run the full houdini injection pipeline

Injects every configured RTF table/image into the Word document at its
bookmark. Suitable for scripts and CI. Re-running on an
already-generated document replaces the previously injected content
instead of duplicating it.

## Usage

``` r
apparate(
  input_doc,
  input_sheet,
  file_location,
  figure_location = NULL,
  hide_data = FALSE,
  rtf = FALSE,
  quiet = FALSE
)
```

## Arguments

- input_doc:

  Path to the template (or previously generated) .docx

- input_sheet:

  Path to a config .xlsx (see [`read_xlsx()`](read_xlsx.md)) or a
  data.frame with Bookmark/Table columns plus optional filter columns

- file_location:

  Folder containing the table .rtf files named in the config

- figure_location:

  Folder containing the figure .rtf files named in the config (defaults
  to table location)

- hide_data:

  Option to replace all data with XX on insertion

- rtf:

  legacy option (kept for existing programs)

- quiet:

  Suppress progress and summary messages

## Value

Invisibly, the per-row status list: NULL for success or a houdini_error
condition per failed row, keyed by config row
