# Watch an RTF folder and regenerate the document on every change

Polls the RTF folder (and the config workbook, when given as a path) and
calls
[`apparate()`](https://aarongodwood.github.io/Houdini/reference/apparate.md)
whenever a file appears, disappears, or changes. Blocks until
interrupted (Escape / Ctrl+C).

## Usage

``` r
houdini_watch(input_doc, input_sheet, file_location, interval = 5)
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

- interval:

  Seconds between polls
