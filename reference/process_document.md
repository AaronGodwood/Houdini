# Process a Word document: inject all configured RTF content at bookmarks

Auto-detects whether each RTF file contains an image (\pngblip) or a
table and calls the appropriate inject function.

## Usage

``` r
process_document(
  word_path,
  config,
  rtf_paths,
  selections,
  output_path,
  progress_cb = NULL,
  hide_data = FALSE
)
```

## Arguments

- word_path:

  Path to the source .docx file

- config:

  data.frame with columns: bookmark, tablename

- rtf_paths:

  Named list: table_name -\> rtf file path

- selections:

  Named list: row_index (as character) -\> list(cols, row_start,
  row_end, parameters, timelines)

- output_path:

  Path to write the final .docx

- progress_cb:

  Optional callback `function(i, n, msg)` invoked once per config row
  for progress reporting; `NULL` disables reporting.

- hide_data:

  toggle to replace all data in tables with XX
