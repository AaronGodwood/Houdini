# Houdini

Programmatically insert data tables and figures into report Word documents.

`Houdini` is an R package for injecting RTF tables and figures
into a Word (`.docx`) template at named bookmarks. It parses RTF output (the
kind produced by SAS), lets you interactively filter and trim each table,
and writes a fully-formatted Word document with native OOXML tables.

## What it does

Given:

- A **Word template** (`.docx`) containing named **bookmarks** where content
  should go
- An **Excel Document** (`.xlsx`) or config data frame that contains bookmark to table mappings and filtering instructions
- A **folder of RTF files**, one per table or figure,

Houdini maps each bookmark to an RTF file and on generate, injects the
corresponding table (as a real Word table) or figure (png) after the bookmarked
paragraph. Tables and figures are scaled to the document's text width automatically.

Along the way you can, per table:

- Filter pages by **parameter** (e.g. `Parameter: Cholesterol`).
- Filter data rows by **timeline** label (e.g. `Week 1`, `Week 4`).
- Filter data by **indent level** (e.g. `prefered term`)
- Interactively **exclude columns and rows** by clicking them in the preview or specifying in the config document.

## Installation

Houdini contains a C fast-path, so installation needs a working toolchain
(Rtools on Windows, or the standard build tools on macOS/Linux).

Note: If C is not available, alternative R functions exist to take their place.

```r
# install.packages("devtools")
devtools::install_local("path/to/Houdini")
```

## Quick start

# Using the GUI

```r
library(Houdini)
run_app()
```

This opens the Shiny app in your browser. Then:

1. **Word Document** - upload the `.docx` template. Houdini lists the bookmarks
   it finds.
2. **RTF Source** - choose the folder containing your RTF files (inside RStudio
   a native folder picker is offered; elsewhere paste the folder path).
3. **Table Configuration** - map each bookmark to an RTF table. Click a row to
   preview it and configure filters/exclusions.
4. **Generate** - download the finished Word document. A generation log is
   available alongside it.
   
# Using the command line interface

```r
library(Houdini)
apparate( input_doc = "path/to/word/document.docx",
          input_sheet = "path/to/excel/document.xlsx",
          file_location = "path/to/rtf/folder/")
```

## Configuration via Excel

For large/reproducible jobs or when using the command line interface  you can define the mapping in a spreadsheet and import it, rather than filling the grid by hand. The recognised columns are
(case-insensitive):

| Column        | Required | Meaning                                                                          |
|---------------|----------|----------------------------------------------------------------------------------|
| `Bookmark`    | yes      | Bookmark name in the Word document.                                              |
| `Dataset`     | yes      | RTF file name (the `.rtf` extension is optional and stripped).                   |
| `Parameters`  | no       | Semicolon-separated parameter values to keep, e.g. `Cholesterol; Triglycerides`. |
| `Timepoints`  | no       | Semicolon-separated timepoint labels to keep, e.g. `Week 1; Week 4`.             |
| `Levels`      | no       | Semicolon-separated indent level labels to keep e.g. `System Organ Class; Prefered Term`|
| `ExcludedCols`| no       | Semicolon-separated column numbers or headers to exclude e.g. `1; 4` or `Placebo; Treatment`|

Use **Export Excel** to save the current configuration back out in this same
format (a convenient way to produce a template to edit).

## How it works

The pipeline is deliberately dependency-light - no external RTF or Word
libraries:

1. **Parse** the RTF into a structured page/row/cell representation
   (`R/parsing.R`, with a C fast-path in `src/` and a pure-R fallback).
2. **Filter and combine** pages by parameter/timeline and apply exclusions.
3. **Render** to minimal OOXML (`R/xml.R`) or HTML preview (`R/html.R`).
4. **Inject** the OOXML into the unzipped `.docx`, then rezip (`R/docx.R`).

See the *Getting started with Houdini* vignette for a worked end-to-end example.

## License

MIT. See [LICENSE.md](LICENSE.md).
