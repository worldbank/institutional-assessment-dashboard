--- 
title: "Data processing"
author: "Luiza Andrade, Serena Cocciolo, Eric Braian Arias, Gabriel Vaz de Melo"
site: bookdown::bookdown_site
output: bookdown::gitbook
config:
    toc:
      collapse: subsection
documentclass: book
link-citations: yes
---



# About

This notebook processes the data used in G-BID.

## Render book

You can render the HTML version of this example book without changing anything:

1. Find the **Build** pane in the RStudio IDE, and

1. Click on **Build Book**, then select your output format, or select "All formats" if you'd like to use multiple formats from the same book source files.

Or build the book from the R console:


```r
bookdown::render_book()
```

To render this example to PDF as a `bookdown::pdf_book`, you'll need to install XeLaTeX. You are recommended to install TinyTeX (which includes XeLaTeX): <https://yihui.org/tinytex/>.
