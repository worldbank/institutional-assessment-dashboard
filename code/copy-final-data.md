# Move final data to app folder


```r
packages <- 
  c(
    "here"
  )

pacman::p_load(packages,
               character.only = TRUE)
```


```r
file.copy(
  list.files(
    here(
      "..",
      "data",
      "final"
    ),
    full.names = TRUE
  ),
  here(
    "..",
    "app",
    "data"
  ),
  recursive = TRUE
)
```

```
## [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
file.copy(
  "vars-control.R",
  here(
    "..",
    "app",
    "auxiliary"
  )
)
```

```
## [1] FALSE
```
