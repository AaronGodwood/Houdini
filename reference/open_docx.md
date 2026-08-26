# Open a Word document for editing

Unzips the .docx, parses word/document.xml and its relationships file
into memory, detects text width, and builds a bookmark jump table. Call
inject_table() / inject_image() any number of times, then close_docx().

## Usage

``` r
open_docx(docx_path)
```

## Arguments

- docx_path:

  Path to the source .docx file

## Value

A docx session environment
