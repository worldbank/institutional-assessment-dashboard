---
title: "Data Quality Control: Preliminary Analysis"
output: word_document
date: "2023-03-21"
---

# Quality checks: Closeness to Frontier

- Inputs:
  - `data/final/db_variables.rds`
  - `data/final/db_variables.rds`
      
- Outputs:
  - Missingness visualizations
 

## Load packages


```r
packages <- 
  c(
    "tidyverse",
    "here",
    "testthat",
    "naniar",
    "tidytext"
  )

pacman::p_load(
  packages, 
  character.only = TRUE
)

theme_set(theme_minimal())

knitr::opts_chunk$set(
  echo = FALSE, warning = FALSE, message = FALSE, 
  fig.width = 10, fig.height = 6, dpi = 300
)
```

## Read-in Data



## Data Quality Control

We test the data for the following metrics:

  1. Uniqueness of country-year 


```
## Test passed 🥳
```

  2. Missingness of indicators.
  
![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

  3. Missingness by year.


```
## [[1]]
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```
## 
## [[2]]
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png)

```
## 
## [[3]]
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-3.png)

```
## 
## [[4]]
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-4.png)

```
## 
## [[5]]
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-5.png)

```
## 
## [[6]]
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-6.png)

```
## 
## [[7]]
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-7.png)

```
## 
## [[8]]
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-8.png)

```
## 
## [[9]]
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-9.png)

```
## 
## [[10]]
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-10.png)

  4. Missingness by country.

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
