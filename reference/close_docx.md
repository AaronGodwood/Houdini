# Finalise and save a Word document session

Serialises document.xml and the updated relationships file, rezips the
.docx, and cleans up the temp directory.

## Usage

``` r
close_docx(session, output_path)
```

## Arguments

- session:

  A docx session returned by open_docx()

- output_path:

  Path to write the finished .docx
