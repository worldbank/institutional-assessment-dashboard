# Create spatial data

- Inputs:
  - `data/final/closeness_to_frontier.rds`
  - `data/final/compiled_indicators.rds`
  - `data/raw/WB_countries_Admin0_lowres.geojson`
  - `data/raw/WB_disputed_areas_Admin0_10m.shp`
  
- Output:
  - `data/final/indicators_map.rds`
  
## Packages 


```r
packages <-
  c(
    "tidyverse",
    "rmapshaper",
    "sf",
    "here"
  )

pacman::p_load(
  packages,
  character.only = TRUE
)
```

## Input data 


```r
ctf <-
  read_rds(
    here(
      "..",
      "data",
      "final",
      "closeness_to_frontier.rds"
    )
  )

raw_indicators <-
  read_rds(
    here(
      "..",
      "data",
      "final",
      "compiled_indicators.rds"
    )
  )
```

## Official WB maps 


```r
world_map <-
  read_sf(
    here(
      "..",
      "data",
      "raw",
      "WB_countries_Admin0_lowres.geojson"
    )
  )

disputed_areas <-
  read_sf(
    here(
      "data",
      "raw",
      "disputed_areas",
      "WB_disputed_areas_Admin0_10m.shp"
    )
  )
```

```
## Error: Cannot open "C:/Users/wb501238/Documents/GitHub/institutional-assessment-dashboard/notebook/data/raw/disputed_areas/WB_disputed_areas_Admin0_10m.shp"; The file doesn't seem to exist.
```

# Clean maps 


```r
disputed_areas <-
  disputed_areas %>%
  select(WB_A3) %>%
  filter(!is.na(WB_A3))
```

```
## Error in select(., WB_A3): object 'disputed_areas' not found
```

```r
world_map <-
  world_map %>%
  select(WB_A3) 

world_map <-
  world_map %>%
  bind_rows(
    disputed_areas
  ) %>%
  rename(
    country_code = WB_A3
  ) %>%
  ms_simplify(
    keep = 0.04,
    keep_shapes = T
  )
```

```
## Error in list2(...): object 'disputed_areas' not found
```

```r
st_crs(world_map) <- "WGS84"

world_map <-
  world_map %>%
  st_transform("+proj=robin")
```


# Combine maps and data

## Closeness to frontier


```r
ctf <-
  ctf %>%
  pivot_longer(
    cols = complaint_mechan:log_gdp,
    values_to = "ctf"
  ) %>%
  mutate(
    bin = case_when(
      ctf < .2 ~ "0.0 - 0.2",
      ctf < .4 ~ "0.2 - 0.4",
      ctf < .5 ~ "0.4 - 0.6",
      ctf < .8 ~ "0.6 - 0.8",
      ctf <= 1 ~ "0.8 - 1.0" 
    )
  ) %>%
  pivot_wider(
    id_cols = starts_with("country_"),
    names_from = name,
    values_from = c(bin, ctf)
  )
```

## Raw data


```r
raw <-
  raw_indicators %>%
  pivot_longer(
    cols = 4:ncol(.)
  ) %>%
  filter(!is.na(value)) %>%
  group_by(country_name, country_code, name) %>%
  filter(year == max(year)) %>%
  ungroup %>%
  pivot_wider(
    values_from = c(value, year),
    names_from = name
  )

map <-
  world_map %>%
  left_join(
    raw
  ) %>%
  left_join(
    ctf
  )
```

```
## Error in `sf_column %in% names(g)`:
## ! `by` must be supplied when `x` and `y` have no
##   common variables.
## i use by = character()` to perform a cross-join.
```


# Save datasets


```r
map %>%
  write_rds(
    here(
      "data",
      "final",
      "indicators_map.rds"
    )
  )
```

```
## Warning in saveRDS(x, con, version = version, refhook
## = refhook, ascii = text): cannot open file 'C:/Users/
## wb501238/Documents/GitHub/institutional-assessment-
## dashboard/notebook/data/final/indicators_map.rds': No
## such file or directory
```

```
## Error in saveRDS(x, con, version = version, refhook = refhook, ascii = text): cannot open the connection
```
