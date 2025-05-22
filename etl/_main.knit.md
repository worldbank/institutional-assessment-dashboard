--- 
title: "Data processing"

author: "Eric Braian Arias, Galileu Kim, Kannan Venkataramanan, Ileana Marroquin Martinez, Alex Khater.\n **Former Members:** Luiza Andrade, Serena Cocciolo, Gabriel Vaz de Melo, Shelmith Kariuki, Abigail Paterson."

site: "bookdown::bookdown_site"
output: 
  bookdown::gitbook
config:
    toc:
      collapse: subsection
    download: ["pdf"] 
documentclass: book
link-citations: yes
---

# Overview

The objective of this document is to provide you with an overview of the data infrastructure for CLIAR, describing entities, their relationships, as well as a description on the data pipeline, including (1) data extraction, (2) data quality controls, (3) data transformation and (4) data loading. As such, the document provides an overview of the data ETL process for CLIAR. We also present an overview of how data from CLIAR is used in the CLIAR Dashboard, providing an end-to-end documentation of how data is applied for analytics.

## Data Model:

This section provides you with an overview of the data model, in particular an Entity Relationship Diagram (ERD) of all entities contained in CLIAR. Note that this diagram describes the finalized data infrastructure, once all the data has been ingested, quality controlled and ingested. Given that the current data model is still evolving, we first provide a Conceptual Model of the CLIAR data infrastructure which will evolve in granularity as the requirements for CLIAR mature.

![Figure 1. Conceptual Model for CLIAR](./_book/assets/cliar_data_model.png)

This Conceptual Model for CLIAR provides us with a mapping of both the entities contained in CLIAR’s data infrastructure, as well as the structure of their relationship. Note that the Indicator entity is at the center of our model. The unit of analysis for the Indicator entity is country-year. The value of the institutional indicator varies across countries and years, and belong to a particular institutional family (e.g., Public HRM). Additionally, to apply the Closeness-to-Frontier (CTF) methodology, we require a set of country comparators, alongside the time window, to calculate the CTF score.

As the requirements for institutional analysis in CLIAR evolve, additional entities will be added to this data model. It is important to both explicitly define these entities, as well as fully mapping their relationship to pre-existing entities. This will ensure that there is full transparency in modifications to the data model. This updating will also help identify data dependencies and potential adaptations needed to ensure the model is robust.

## Data Pipeline:
This section describes the different stages of data processing in CLIAR. We describe these different stages using the convention nomenclature Extraction-Transformation-Loading (ETL). We focus on these three stages:

1.	Data Extraction: this section describes the different data sources extracted by the CLIAR data pipeline and how they are accessed. Currently, the data is being imported from two sources:
    a.	Prosperity Data: Prosperity Data is an initiative of the World Bank’s Governance Equity, Finance and Institutions (EFI) Vice-Presidency. It contains more than 4,700 governance-related indicators on state capacity, efficiency, openness, inclusiveness, accountability, integrity, and trust in government. The site gathers information from 35 data sources, including other World Bank sources. Prosperity 360 is a powerful R Package that facilitates the retrieval and analysis of data related to various indicators.  The process of extracting a subset of indicators from the larger pool of 4700 indicators is guided by the project's specific goals and objectives. The subset selection focuses on indicators that are directly relevant to the CLIAR project's aims. By narrowing down the indicators to those that align with the project's focus, the subset becomes more targeted and meaningful for the CLIAR project's analysis and decision-making processes.
    b. Original Data: The team extracts indicators from an original table of indicators manually produced by the legacy team (merged_for_residuals.rds). This dataset should be updated, and ensure that the indicators are extracted from the original sources instead of a legacy file.
    c. Central Bank Independence Data : The database contains information on the CBIE index in (Romelli, 2022), as well as the most used indices of central bank independence for 154 countries between 1972 and 2017. Using the dta file provided, we extract the variable LVAU and use that for representing the central bank independence. 

2.	Data Quality Controls: this section describes the different protocols used to assess the quality of the data once it is ingested. In particular, we highlight (1) the quantitative review of data imports, as well as (2) inclusion criteria for CLIAR.
    a.	Note: Currently, the data quality controls are not being applied to the Data Extraction. That is because in the legacy system, no quality controls were applied. This will be modified in the next iteration of CLIAR. Our assessment is that the quality controls should be applied at the Data Extraction step. 

3. Data Transformation: this section describes how data is transformed once it has been both ingested and vetted for inclusion within CLIAR. This includes transformation of indicators such as normalization, as well as the application of the Closeness-to-Frontier methodology.
    a.	Currently, there are plenty of transformations applied during the process of data preparation. A few are listed below:
        i.	For the governence data pertaining to Oil and Gas, we combine them and take the average of the indicators
        ii.	GCI indicators are supposed to take values from 1 to 7. However, each indicator has one country with a score above 7 and replace any values above 7 with NA.
        iii.	For computation of CTF, we filter data post 2013 and we rescale indicators so a higher number is always a better performance.
        iv.	Later, we calculate the country-level average for each indicator, identify worst and best performance for each indicator and finally compute closeness to frontier at indicator level by using the min –max scaling.
    b.	Note: Currently the transformation of data is occurring at different stages of the pipeline, and it is not well-documented. This ad hoc approach is brittle and should be transitioned into a systematic, functional programming approach. The team suggests that country name standardizations occurs through the use of the countrycodes package, and any transformation of data is defined at the data source level, not individual indicators.

4.	Data Loading: this section describes how data is exported for use by the CLIAR dashboard. Note that individual TTLs may request data to be exported to them for customized analysis. As such, this section should also accommodate these use-cases.
    a.	We have two stream of codes. One for data preparation and other for the application. Once the data preparation and processing has been completed, the processed data is copied to the data folder inside the application (app) folder. This powers the functioning of the application locally. 
    b.	Once we host the application on Posit, the app folder (which includes data folder) is completely copied to the R server from where the application runs.
    c.	Note: The loading of the data is being handled in the backend by Posit Connect. This means that all of the data contained in the GitHub repository is being converted into a back-end sqlite file, which is then accessed by the Shiny App. There is no circumventing it, but what the team should aim for is to directly load a sqlite file into the RShiny repository, so that the database itself is loaded into PositConnect. Additionally, the sqlite file should be stored in a secure folder in the World Bank’s OneDrive, instead of living locally in each one of the team member’s computers.


<!--chapter:end:index.Rmd-->

# Load R packages


``` r
# generate output data folder
if(!dir.exists(here("data", "output"))) dir.create(here("data", "output"))

library(tidyverse)
library(readxl)
library(here)
library(labelled)
library(haven)
library(knitr)
library(assertthat)
library(tibble)
library(sf)
library(janitor)
library(stringr)
library(scales)
library(naniar)
library(countrycode)
library(purrr)
library(testthat)
```

<!--chapter:end:00-setup.Rmd-->


# Data Sources update (optional)

-   Input: `data/input/**`
-   Output: `data/output/2024/clean-2024version-*.csv.gz`

READ ME (new data versions)

-   This script is considered "optional" because its primary purpose is
    to extract new data versions. Therefore, each chunk of this script
    is currently set to `eval=FALSE` (not running).

-   To effectively execute this code and run the chunks, remove the
    `eval=FALSE` tags from the chunk names.

-   Latest Data Version Update: September 5, 2024.

## Import Inputs

The data, initially imported manually, will be filtered (relevant
indicators only) and cleaned either calling .do files or in R. It uses
the 'RStata' package for initial cleaning of manually imported data,
streamlining the process and preparing key indicators. This step enables
automation for future data update. Once cleaned, standardized CSV files
will be exported to be further processed.


``` r
# Setting path based on the user
{
  
  user <- Sys.getenv("USERNAME")
  
  # # NEED TO CHANGE THIS DIRECTORY
  # if (user == "wb622595") { 
  #   print("Ileana has been selected")
  #   directory <- "C:/Users/wb622595/gitHub-repositories/institutional-assessment-dashboard/code"
  # } else {
  #   stop("Unknown user. Please set the correct directory.")
  # }
  #ALEX ADDITION TO MAKE THIS RUN
  
  directory<-here()
  # NEED TO CHANGE STATA (CHANGE DIRECTORY) 
  options("RStata.StataVersion" = 18.0)
  options("RStata.StataPath" = "/ProgramData/Microsoft/Windows/Start Menu/Programs/StataMP 18")
}

# Set working directories
{
  # Data directories
  input_data <- file.path(directory, "data/input")
  output_data <- file.path(directory, "data/output")
  
  # Stata do files to prepare inputs
  do_doc <- file.path(directory, "data/input/0.rtsata_inputs/")
}


# Clean the datasets bash 2024
{
# stata(file.path(do_doc, "clean-2024-heritage.do"))
# stata(file.path(do_doc, "clean-2024-wbl.do"))

}
```


``` r
wdi <- WDI(
  country = "all",
  indicator = c(
    # Existing indicators
    "GC.DOD.TOTL.GD.ZS", "EN.ATM.CO2E.PP.GD.KD", "BN.CAB.XOKA.GD.ZS",
    "SH.XPD.GHED.GD.ZS", "GC.XPN.TOTL.GD.ZS", "NE.EXP.GNFS.ZS",
    "NE.CON.TOTL.ZS", "BX.KLT.DINV.WD.GD.ZS", "BM.KLT.DINV.WD.GD.ZS",
    "NY.GDP.MKTP.KD", "NY.GDP.DEFL.ZS", "NY.GDP.DEFL.ZS.AD",
    "NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.KD", "NY.GDP.PCAP.KD.ZG",
    "NY.GDP.PCAP.PP.KD", "NY.GDP.MKTP.PP.KD", "NE.CON.GOVT.ZS",
    "SE.XPD.TOTL.GD.ZS", "SE.XPD.PRIM.PC.ZS", "SE.XPD.SECO.PC.ZS",
    "SE.XPD.TERT.PC.ZS", "NE.GDI.TOTL.ZS", "NY.GDS.TOTL.ZS",
    "NE.GDI.FTOT.ZS", "NE.DAB.TOTL.ZS", "NY.GNS.ICTR.ZS",
    "NE.IMP.GNFS.ZS", "NY.GDP.DEFL.KD.ZG", "NY.GDP.DEFL.KD.ZG.AD",
    "NV.IND.MANF.ZS", "MS.MIL.XPND.GD.ZS", "NY.GDP.MINR.RT.ZS",
    "NY.GDP.NGAS.RT.ZS", "GC.NLD.TOTL.GD.ZS", "NY.GDP.PETR.RT.ZS",
    "BX.TRF.PWKR.DT.GD.ZS", "GC.REV.XGRT.GD.ZS", "NV.SRV.TOTL.ZS",
    "GC.TAX.TOTL.GD.ZS", "NY.GDP.TOTL.RT.ZS", "NE.TRD.GNFS.ZS",
    "NY.GNP.MKTP.KD", "DT.DOD.DECT.GN.ZS", "NY.GNP.MKTP.KD.ZG",
    "NY.GNP.PCAP.KD", "NY.GNP.PCAP.KD.ZG", "NY.GNP.PCAP.PP.KD",
    "NY.GNP.MKTP.PP.KD", "DT.ODA.ODAT.GN.ZS", "DC.ODA.TOTL.GN.ZS",
    "DT.TDS.DPPG.GN.ZS", "DT.TDS.DECT.GN.ZS", "DT.DOD.PVLX.GN.ZS",
    "DT.DOD.DSTC.ZS", "DT.DOD.DSTC.IR.ZS", "DT.TDS.DECT.EX.ZS",
    "FI.RES.TOTL.DT.ZS", "DT.DOD.DSTC.XP.ZS", "DT.TDS.MLAT.PG.ZS",
    "IQ.CPA.HRES.XQ", "IQ.CPA.BREG.XQ", "IQ.CPA.DEBT.XQ",
    "IQ.CPA.ECON.XQ", "IQ.CPA.REVN.XQ", "IQ.CPA.PRES.XQ",
    "IQ.CPA.FISP.XQ", "IQ.CPA.FINS.XQ", "IQ.CPA.GNDR.XQ",
    "IQ.CPA.MACR.XQ", "IQ.CPA.SOCI.XQ", "IQ.CPA.ENVR.XQ",
    "IQ.CPA.PROP.XQ", "IQ.CPA.PUBS.XQ", "IQ.CPA.FINQ.XQ",
    "IQ.CPA.PADM.XQ", "IQ.CPA.PROT.XQ", "IQ.CPA.STRC.XQ",
    "IQ.CPA.TRAD.XQ", "IQ.CPA.TRAN.XQ",
    # Updated code indicators according the Metadata Glossary 
    # https://databank.worldbank.org/metadataglossary/all/series
    # adjust db_bariables
    "SE.ADT.LITR.FE.ZS",  # Literacy rate, adult female
    "SE.ADT.LITR.MA.ZS",  # Literacy rate, adult male
    "SE.ADT.LITR.ZS",     # Literacy rate, adult total
    "SH.DYN.MORT",        # Mortality rate, under-5
    "SH.STA.ANVC.ZS",     # Pregnant women receiving prenatal care "SH.STA.PRSC.ZS"
    "SH.STA.MMRT",       # Maternal mortality ratio "SH.MMR.RATIO",
    "SH.DYN.MORT.FE",     # Mortality rate, adult female sp.dyn.amrt.fe
    "SH.DYN.MORT.MA",      # Mortality rate, adult male 
    "GC.REV.SOCL.ZS", "MS.MIL.TOTL.TF.ZS", "SL.UEM.TOTL.NE.ZS",
    "SL.UEM.TOTL.ZS", "SE.COM.DURS", "SE.XPD.CPRM.ZS",
    "SE.XPD.CSEC.ZS", "SE.XPD.CTER.ZS", "SE.XPD.CTOT.ZS",
    "SE.XPD.TOTL.GB.ZS", "SE.SEC.TCAQ.LO.ZS", "SE.PRE.TCAQ.ZS",
    "SE.PRM.TCAQ.ZS", "SE.SEC.TCAQ.ZS", "SE.SEC.TCAQ.UP.ZS",
    "SE.PRM.TENR", "SH.STA.BRTC.ZS", "SH.MED.CMHW.P3",
    "SH.XPD.CHEX.GD.ZS", "SH.XPD.OOPC.CH.ZS", "SH.XPD.OOPC.PP.CD",
    "SI.POV.MDIM", "SI.POV.MDIM.XQ", "SI.POV.GAPS",
    "SI.POV.LMIC.GP", "SI.POV.UMIC.GP", "SI.POV.DDAY",
    "SI.POV.LMIC", "SI.POV.UMIC", "SI.POV.NAHC",
    "SE.PRM.ENRL.TC.ZS", "SE.SEC.ENRL.TC.ZS", "SE.TER.ENRL.TC.ZS",
    "SH.MED.BEDS.ZS", "SH.MED.PHYS.ZS", "SP.REG.BRTH.ZS",
    "SP.REG.BRTH.RU.ZS", "SP.REG.BRTH.UR.ZS"
  ),
  start = 1990,
  end = 2023,
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en"
)
```

``` r
# vdem <- vdemdata::vdem 
```

## Clean Inputs


``` r
wdi_clean <- wdi %>%
  clean_names() %>%
  rename(
    country_name = country,
    country_code = iso3c
  )

# Function to convert mixed ISO2 and ISO3 codes to ISO3
convert_to_iso3 <- function(codes) {
  # Convert ISO2 codes to ISO3
  iso3_from_iso2 <- countrycode(
    sourcevar = codes,
    origin = "iso2c",
    destination = "iso3c",
    warn = FALSE
  )
  
  iso3_from_iso3 <- countrycode(
    sourcevar = codes,
    origin = "iso3c",
    destination = "iso3c",
    warn = FALSE
  )
  
  # Return the ISO3 codes from either conversion
  iso3_from_iso2 %>% ifelse(is.na(.), iso3_from_iso3, .)
}

wdi_clean <- wdi_clean %>%
  mutate(
    country_code = convert_to_iso3(iso2c)
  ) %>% 
  select(-iso2c)

# # Check unmatched codes
# unmatched_codes <- wdi_clean$iso2c[is.na(wdi_clean$country_code)]
# print(unmatched_codes)

# Remove underscores from column names and replace prefixes with `wdi_` convention
wdi_clean <- wdi_clean|>
  rename_with(~ str_replace_all(.x, "_", ""), .cols = everything()) %>% 
  rename_with(~ str_replace_all(.x, "^(.+)", "wdi_\\1"), .cols = !starts_with("country") & !starts_with("year"))


vdem_clean <- vdem |>
  filter(year >= 1990) |>
  rename(
  country_code = country_text_id 
  ) |> 
  clean_names() |>  # Clean column names to lower case with underscores
  distinct(country_code, year, .keep_all = TRUE)|>  # Remove duplicates
  select(
    country_code,
    year,
    v2x_corr,
    v2exbribe,
    v2xcl_prpty,
    v2xcl_acjst,
    v2juhcind,
    v2juncind,
    v2juaccnt,
    v2pepwrgen,
    v2pepwrsoc,
    v2pepwrses,
    v2xlg_legcon,
    v2x_gender,
    v2stcritrecadm,
    v2clrspct,
    v2cseeorgs,
    v2dlengage,
    v2clacfree,
    v2csreprss,
    v2x_civlib,
    v2x_cspart,
    v2clstown, ### Former name v2cl_acjst State ownership of economy
    v2clacjstm,
    v2clacjstw,
    v2lgqugen,
    v2lgfemleg,
    v2cldiscm,
    v2cldiscw,
    v2caassemb,
    v2cacamps,
    v2peapsecon,
    v2peasjsoecon,
    v2peapsgen,
    v2peasjgen,
    v2peapspol,
    v2peasjpol,
    v2x_pubcorr,
    v2x_execorr,
    v2lgcrrpt,
    v2dlencmps,
    v2clstown,
    v2peedueq,
    v2pehealth,
    v2peasbepol,
    v2cafres,
    v2cafexch,
    v2x_rule,
    v2xed_ed_cent,
    v2xed_ed_ctag,
    v2xed_ed_con,
    v2edteautonomy,
    v2edteunionindp
  ) |>
 rename_with(
    # replace prefixses with efi conventions
    ~ paste0("vdem_core_", .), 
    .cols = starts_with("v2")
  )
```

## Export Inputs


``` r
write_dta(
  vdem_clean,
  here(
    "data",
    "input",
    "vdem",
    "2024",
    "vdem_19902023.dta"
  )
)

write_dta(
  wdi_clean,
  here(
    "data",
    "input",
    "wdi",
    "2024",
    "wdi_19902023.dta"
  )
)
```

<!--chapter:end:01-update-input-data.Rmd-->

# Process selected indicators

- Input: `data/db_variables.xlsx`
- Outputs: 
  - `data/final/db_variables.xlsx`
  - `data/final/definitions.rds`

## Load list of selected indicators

This list is filled by hand in Excel.


``` r
db_variables <-
  read_excel(
    here(
      "data",
      "input",
      "cliar",
      "db_variables.xlsx"
    )
  ) |> 
  clean_names() |> 
  mutate(
    variable = make_clean_names(variable),
    var_name = str_to_sentence(var_name, locale = "en") # To Sentence
  ) 
```


## Save list of selected indicators in R format


``` r
write_rds(
  db_variables,
  here(

    "data",
    "output",
    "db_variables.rds"
  )
)
```

## Save variable definitions by family


``` r
description <- 
  function(x) {
    assign(
      x,
      db_variables %>%
        filter(family_name == x) %>%
        select(
          Indicator = var_name,
          Description = description,
          Source = source
        )
    )
  }

description <-
  lapply(
    unique(db_variables$family_name),
    description
  )

names(description) <- 
  unique(db_variables$family_name)

write_rds(
  description,
  here(
    "data",
    "output",
    "definitions.rds"
  )
)
```




<!--chapter:end:02-select-indicators.Rmd-->

# Process data

- Input: `data/input/**/*.csv`
- Output: 
    - `data/output/cliar_compiled_indicators.rds`
    - `data/output/diagnostics_compiled_indicators.rds`
    - `data/output/coverage_report_input.rds`
    - `data/output/coverage_ctf_for_analysis.rds`
    - `data/output/coverage_report_full_for_analysis.rds`
    - `data/output/grouping_country_income_and_region_updated.rds`

This script generates the consolidated indicators for the CLIAR dashboard. It imports, processes and consolidates a diverse range of datasets, including from EFI360 and others. An exhaustive list of datasets is provided below:



## Import data

The data was imported from (a) the EFI360 shared data and (b) manual imports.
This section first reads in each of the individual manual-input files (Note that the updated version count with '2024' as folder and the column id names are cleaned with the clean_names() function. After this is done, a list of of standardized WB country names and codes is read in and mutated to be better compatible with the CLIAR data


``` r
efi <- read_dta(
  here("data", "input", "efi", "2024", "GTMI_Fixed_Update_EFI360_1990_2024.dta") 
) ### Latest API extraction and FTMI patch implemented due to a bug [Github issue #326]

# Data availablke here: https://www.pefa.org/assessments/batch-downloads
# Methodology 2016 and national download
pefa <- read_csv(
        here("data", "input", "efi", "2024", "assessments_1730149268.csv")
) |>
  clean_names()
```

```
## Rows: 102 Columns: 132
## ── Column specification ────────
## Delimiter: ","
## chr (130): Country, PI-01, P...
## dbl   (2): Framework, Year
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
romelli <- read_dta(
  here("data", "input", "romelli", "CBIData_Romelli2022.dta")
) |>
  clean_names()

debt_transparency <- read_dta(
  here("data", "input", "debt_transparency", "debt_transparency_2021-2022.dta")
)

fraser <- read_xlsx(
  here("data", "input", "fraser", "2024", "efotw-2023-master-index-data-for-researchers-iso.xlsx")
) |> 
  clean_names() |> 
  slice(-c(1:2)) 
```

```
## New names:
## • `` -> `...1`
## • `` -> `...2`
## • `` -> `...3`
## • `` -> `...4`
## • `` -> `...5`
## • `` -> `...6`
## • `` -> `...7`
## • `` -> `...8`
## • `` -> `...9`
## • `` -> `...10`
## • `` -> `...11`
## • `` -> `...12`
## • `` -> `...13`
## • `` -> `...14`
## • `` -> `...15`
## • `` -> `...16`
## • `` -> `...17`
## • `` -> `...18`
## • `` -> `...19`
## • `` -> `...20`
## • `` -> `...21`
## • `` -> `...22`
## • `` -> `...23`
## • `` -> `...24`
## • `` -> `...25`
## • `` -> `...26`
## • `` -> `...27`
## • `` -> `...28`
## • `` -> `...29`
## • `` -> `...30`
## • `` -> `...31`
## • `` -> `...32`
## • `` -> `...33`
## • `` -> `...34`
## • `` -> `...35`
## • `` -> `...36`
## • `` -> `...37`
## • `` -> `...38`
## • `` -> `...39`
## • `` -> `...40`
## • `` -> `...41`
## • `` -> `...42`
## • `` -> `...44`
## • `` -> `...45`
## • `` -> `...46`
## • `` -> `...47`
## • `` -> `...48`
## • `` -> `...49`
## • `` -> `...50`
## • `` -> `...51`
## • `` -> `...52`
## • `` -> `...53`
## • `` -> `...54`
## • `` -> `...55`
## • `` -> `...56`
## • `` -> `...57`
## • `` -> `...58`
## • `` -> `...59`
## • `` -> `...60`
## • `` -> `...62`
## • `` -> `...63`
## • `` -> `...64`
## • `` -> `...65`
## • `` -> `...66`
## • `` -> `...67`
## • `` -> `...68`
## • `` -> `...69`
## • `` -> `...70`
## • `` -> `...71`
## • `` -> `...72`
## • `` -> `...73`
## • `` -> `...74`
## • `` -> `...75`
## • `` -> `...76`
## • `` -> `...77`
## • `` -> `...78`
## • `` -> `...79`
## • `` -> `...80`
## • `` -> `...81`
## • `` -> `...82`
## • `` -> `...83`
## • `` -> `...84`
## • `` -> `...85`
```

``` r
gfdb <- read_dta(
  here("data", "input", "gfdb", "GFDB_19902021.dta")
)

oecd_epl_regular <- read_csv(
  here("data", "input", "oecd", "epl_regular.csv")
)
```

```
## Rows: 2654 Columns: 15
## ── Column specification ────────
## Delimiter: ","
## chr (7): COUNTRY, Country, S...
## dbl (4): TIME, Time, PowerCo...
## lgl (4): Reference Period Co...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
oecd_epl_temporary <- read_csv(
  here("data", "input", "oecd", "epl_temporary.csv")
)
```

```
## Rows: 1805 Columns: 15
## ── Column specification ────────
## Delimiter: ","
## chr (7): COUNTRY, Country, S...
## dbl (4): TIME, Time, PowerCo...
## lgl (4): Reference Period Co...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
oecd_pmr <- read_dta(
  here("data", "input", "pmr", "PMR_2018.dta")
)

spi <- read_csv(
  here("data", "input", "spi", "2024", "SPI_index_labelled.csv")
) |> 
  clean_names() 
```

```
## Rows: 4141 Columns: 79
## ── Column specification ────────
## Delimiter: ","
## chr (76): country, iso3c, SP...
## dbl  (3): date, weights, pop...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
aspire <- read_dta(
  #Testing new ASPIRE pulled from Pros360 API
  here("data", "input", "aspire", "API_ASPIRE.dta")
  #VV Original Dataset
  #here("data", "input", "aspire", "ASPIRE performance indicators.dta")
)

rise <- read_dta(
  here("data", "input", "rise", "RISE_20102021.dta")
)


wdi <- read_dta(
  here("data", "input", "wdi", "2024", "wdi_19902023.dta")
)

vdem <- read_dta(
  here("data", "input", "vdem", "2024", "vdem_19902023.dta")
)


heritage <- read_dta(
  here("data", "input", "heritage", "2024", "heritage20122024.dta")
)

open_budget <- list.files(
    here("data", "input", "ibp"),
    full.names = TRUE
  ) |> 
    map_dfr(
      read_csv,
      col_select = c(ISO, year, obi)
    )
```

```
## Rows: 441 Columns: 3
## ── Column specification ────────
## Delimiter: ","
## chr (1): ISO
## dbl (2): year, obi
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 115 Columns: 3
## ── Column specification ────────
## Delimiter: ","
## chr (1): ISO
## dbl (2): year, obi
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 117 Columns: 3
## ── Column specification ────────
## Delimiter: ","
## chr (1): ISO
## dbl (2): year, obi
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 120 Columns: 3
## ── Column specification ────────
## Delimiter: ","
## chr (1): ISO
## dbl (2): year, obi
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
# read in world bank standard country codes and mutate them to be compatible
# with the other files
wb_country_list <- read_xlsx(
      here(
        "data",
        "input", 
        "wb",
        "CLASS.xlsx"
      ),
      sheet = "compositions"
    ) %>%
    transmute(
      country_code = WB_Country_Code,
      country_name = WB_Country_Name,
      group = WB_Group_Name,
      group_code = WB_Group_Code
    ) |>
  # exclude non-WB member countries
  filter(
    country_code != "CUB" & 
      country_code != "PRK"
  )

# country income group and region
country_income_and_region <- read_xlsx(
      here(
        "data",
        "input", 
        "wb",
        "CLASS.xlsx"
      ),
      sheet = "List of economies",
      n_max = 219
    ) |>
    transmute(
      country_code = Code,
      region = Region,
      income_group = `Income group`
    ) |>
  # exclude non-WB member countries
  filter(
    country_code != "CUB" & 
      country_code != "PRK"
  )

wblx <- read_dta(
  here("data", "input", "wbl", "WBL_19902022_CLIAR.dta")
)

wbl <- read_dta(
  here("data", "input", "wbl", "WBL_19902023_CLIAR.dta")
)

# note that North America is not included in this list
wb_regions <- c(
  "Africa Eastern and Southern",
  "Africa Western and Central",
  "East Asia & Pacific",
  "Europe & Central Asia",
  "Latin America & Caribbean",
  "Middle East & North Africa",
  "South Asia"
)

country_region_list <- wb_country_list |> 
  # this filter excludes Canada, Bermuda and USA
  filter(group %in% wb_regions) |> 
  select(country_code, region = group)

# Separate SSA into "Africa Eastern and Southern" and "Africa Western and Central",
country_income_and_region_updated <- country_income_and_region %>%
  left_join(country_region_list, by = "country_code", suffix = c("", "_new")) %>%
  mutate(region = if_else(region == "Sub-Saharan Africa", region_new, region)) %>%
  select(-region_new)
```

## Process data

In this section, we take the EFI files, plus the manual import data, and clean them. In order to ensure clean column names and accurate variables, certain datasets are edited to create new variables or to change other factors. More detail given in each subsection.


``` r
# 1. efi
#       For the EFI, first the name of enterprise survey variables is changed to 
#       be more consistent. Then a single PFM indicator is by taking the sum of
#       several GTMI variables. 

efi_clean <- efi |> 
  clean_names() |> 
  # fix enterprise surveys variable name
  rename_with(
    \(x) str_replace(x, "wb_survey", "wb_es_ic_frm"),
    .cols = starts_with("wb_survey")
  ) |> 
  # create PFM MIS indicator by summing over the following and rescaling:
  # GTMI_I-12       Is there an e-Procurement System in place? (foreign and domestic debt)
  # GTMI_I-13       Is there a Debt Management System (DMS) in place? (foreign and domestic debt)
  # GTMI_I-14       Is there a Public Investment Management System (PIMS) in place?
  # GTMI_I-8        Is there a Customs System in place?
  # GTMI_I-7        Is there a Tax Management Information System in place?
  # GTMI_I-6        Is there a TSA supported by FMIS to automate payments and bank reconciliation?
  # GTMI_I-5        Is there an operational FMIS in place to support core PFM functions?
  rowwise() |> 
  mutate(
    wb_gtmi_pfm_mis = sum(
      # wb_gtmi_i_12,
      wb_gtmi_i_13,
      wb_gtmi_i_14,
      wb_gtmi_i_8,
      wb_gtmi_i_7,
      wb_gtmi_i_6,
      wb_gtmi_i_5
    )
  ) |> 
  ungroup() |> 
  mutate(
    wb_gtmi_pfm_mis = scale_values(wb_gtmi_pfm_mis)
  ) |> 
  # edit WJP indicators to: (1) use 2018 data for 2017 in WJP and drop data if year < 2015
  arrange(
    iso3, year
  ) |> 
  mutate(
    across(
      starts_with("wjp_rol"),
      ~ case_when(
        year == 2017 ~ lead(.), # use 2018 data for 2017
        year < 2015 ~ NA, # drop data if year < 2015
        T ~ .
      )
    )
  ) |> 
  select(
    country_code = iso3,
    year,
    everything()
  ) |> 
  select(
    -index,
    -country_name
  )

# PEFA manual update - Defining values, update for 2024
grade_pefa <- c("D" = 1,
                "D* " = 1,
                "D+" = 1.5,
                "C" = 2,
                "C+" = 2.5,
                "B" = 3,
                "A" = 4)

# Transforming the PEFA data
pefa_transformed <- pefa |>
  mutate(across(starts_with("pi_"), 
                 ~ ifelse(. %in% names(grade_pefa), 
                           grade_pefa[.], 
                           NA_real_))) |> 
  rename_with(~ gsub("^pi_", "pi_2016_", .), starts_with("pi_"))

pefa_clean <- pefa_transformed |>
  rename_with(~ paste0("wb_pefa_", .), .cols = starts_with("pi_")) |>
  select(-framework, -uhlg_01, -uhlg_01_1, -uhlg_01_2, -uhlg_01_3)

# Handling country codes, including Kosovo
pefa_country <- pefa_clean %>% 
  mutate(country_code = case_when(
    country == "Kosovo" ~ "XKX",
    TRUE ~ countrycode(country, "country.name", "iso3c", custom_match = c("Kosovo" = "XKX"))
  )) %>% 
  filter(country != "Bosnia and Herzegovina - District Brčko")

# ### Write-out this dataframe for PIGO analysis
# write_rds(
#   pefa_country,
#   here(
#     "data",
#     "output",
#     "pefa_clean_for_analysis.rds"
#   )
# )


# Identify columns that contain "pefa" and are in efi_clean
efi_pefa_subset <- efi_clean  %>% 
  select(country_code, year, contains("pefa")) %>% 
  filter(year <= 2021)

subset_colnames <- colnames(efi_pefa_subset)

# Select only relevant indicators
pefa_subset <- pefa_country %>% 
  select(
    country_code, all_of(subset_colnames)
  ) %>% 
   filter(year >= 2022)

# Merge the dataframes 
efi_pefa_merged <- bind_rows(efi_pefa_subset, pefa_subset)

efi_drop_pefa <- efi_clean %>%
  select(-contains("pefa"))

efi_clean <- left_join(
                efi_drop_pefa,
                efi_pefa_merged,
                by = c("country_code", "year")
)



# 2. fraser - market regulations
      # For the fraser data, the clean dataset is created by selecting only the
      # necessary columns, standardizing the country code variable name, casting
      # the year variable to numeric, and changing variable prefixes to be
      # in line with EFI conventions

      # NOTE 2024:
      # Change variable names https://www.fraserinstitute.org/economic-freedom/approach
fraser <- fraser |> 
  slice(-1) |> 
  setNames(as.character(fraser[1, ])) 

fraser_clean <- fraser |> 
  clean_names() 

# Define the columns to convert to double type
double_columns <- c(
  "year",
  "x5a_credit_market_regulation",
  "x3d_foreign_currency_bank_accounts",
  "x4diii_freedom_of_foreigners_to_visit", 
  "x4dii_capital_controls",
  "x2b_impartial_courts",
  "x2e_legal_integrity",
  "x2a_judicial_independence",
  "x2f_contracts"
)

fraser_clean <- fraser_clean %>% 
  transmute(
    country_code = iso_code_3,
    across(all_of(double_columns), as.double)
  ) |> 
  rename_with(
    ~ str_replace(., "^x[:alnum:]+_", "fraser_efw_")
  )

###
### After using the new fraser file, 4 indicators would need renaming in db_var
### at the time of transforming the variables, Batlesman Institute seems to be the official source
###


  

# 3. romelli - central bank independence
#       The romelli data is cleaned by setting the country code variable name
#       and casting the year variable to numeric
romelli_clean <- romelli |>
  transmute(
    country_code = wb_a3,
    year = as.numeric(year),
    romelli_cbi_central_bank_independence = lvau
  )

# 4. oecd - employment protection laws
#       To clean this data, first the necessary data is taken from both the 
#       regular and temporary employees datasets, as well as standardizing
#       country code and making the year variable numeric. Then these two 
#       categories are merged together.
oecd_epl_regular_clean <- oecd_epl_regular |>
  filter(
    Series == "Version 4 (2013-2019)"
  ) |>
  transmute(
    # the country code used by the OECD is equivalent to the WB
    country_code = COUNTRY,
    year = as.numeric(TIME),
    oecd_epl_regular = Value
  )

oecd_epl_temporary_clean <- oecd_epl_temporary |>
  filter(
    # Series == "Version 1 (1985-2019)"
    Series == "Version 4 (2013-2019)"
  ) |>
  transmute(
    country_code = COUNTRY,
    year = as.numeric(TIME),
    oecd_epl_temporary = Value
  )

# note that there is higher coverage for regular vs. temporary contracts
oecd_epl_clean <- oecd_epl_regular_clean |>
  full_join(
    oecd_epl_temporary_clean
  )
```

```
## Joining with `by =
## join_by(country_code, year)`
```

``` r
# 5. spi - statistical performance indicators
#       First  generate an average index of the census and survey indexes, and
#       then perform the standard cleanings of changing the country code column
#       name and casting the year as numeric

# SPI.DIM4.1.CEN.INDEX	SPI.DIM4.1.SVY.INDEX Dimension 5.2: Standards and Methods
# SPI.DIM5.2.INDEX
spi_clean <- spi %>%
slice(-1)|> 
  mutate(
    spi_dim4_1_cen_index = as.numeric(spi_dim4_1_cen_index),
    spi_dim4_1_svy_index = as.numeric(spi_dim4_1_svy_index),
    spi_dim5_2_index = as.numeric(spi_dim5_2_index),
    date = as.numeric(date) # Relevant cols as numeric
  ) |> 
  rowwise() |> 
  # generate an average index of the census and survey indexes
  mutate(
    spi_census_and_survey_index = mean(
      c(spi_dim4_1_cen_index, spi_dim4_1_svy_index),
      na.rm = TRUE
    )
  ) |> 
  ungroup() |> 
  transmute(
    country_code = iso3c,
    year = as.numeric(date),
    spi_census_and_survey_index,
    spi_std_and_methods = spi_dim5_2_index # standards and methods
  )

# 6. aspire data
#       Filter for the correct indicators, standardize name for country codes 
#       remove non-country codes, and change shape of the data. 
aspire_clean <- aspire |> 
  filter(
    indicator_name %in% c(
      "Adequacy of benefits (%) -All Social Protection and Labor",
      "Coverage (%) -All Social Protection and Labor"
    )
  ) |> 
  transmute(
    # identify non-country codes (aspire includes regions, for example)
    country_code = countrycode(
      Country_Code, 
      origin = "iso3c", destination = "iso3c",
      # create exception for kosovo
      custom_match = c("XKX" = "XKX")
    ),
    year = as.numeric(Year),
    Indicator_Code,
    value = val
  ) |> 
  filter(
    # exclude non-country codes
    !is.na(country_code)
  ) |> 
  pivot_wider(
    id_cols = c(country_code, year),
    values_from = value,
    names_from = Indicator_Code
  ) |> 
  rename(
    wb_aspire_coverage = per_allsp.cov_pop_tot,
    wb_aspire_adequacy_benefits = per_allsp.adq_pop_tot
  )
```

```
## Warning: There was 1 warning in
## `transmute()`.
## ℹ In argument: `country_code =
##   countrycode(...)`.
## Caused by warning:
## ! Some values were not matched unambiguously: AGGREGATE
```

``` r
# 7. rise data
rise_clean <- rise |> 
  clean_names()

# 8. wdi data
# Step 1: Extract variable names and labels
var_labels <- sapply(wdi, function(x) attr(x, "label"))  # Extract labels from the attributes
var_names <- names(var_labels)  # Get the variable names




wdi_clean <- wdi |>
  rename(
    country_name = countryname,
    country_code = countrycode
  ) |>
  rename_with(
    ~ str_to_lower(.) %>% 
      str_replace_all("^wdi_", "wdi_"), 
      starts_with("wdi_")
  )

columns_to_drop <- c(
  "country_name",
  "wdi_dcodatotlgnzs", 
  "wdi_dtdodpvlxgnzs", 
  "wdi_shmedcmhwp3",  
  "wdi_sipovmdim",     
  "wdi_sipovmdimxq"  
)  
  
wdi_clean <- wdi_clean |>
  select(!all_of(columns_to_drop)) |>
  # rename(
  #   wdi_spdynamrtfe = wdi_shdynmortfe,
  #   wdi_spdynamrtma = wdi_shdynmortma
  # ) |>
  filter(!is.na(country_code) & country_code != ""
  ) |>
  distinct(country_code, year, .keep_all = TRUE)


# 9. vdem
vdem_clean <- vdem 

# 10. heritage
heritage_clean <- heritage |> 
  clean_names() |>
  filter(!is.na(country_code) & country_code != "")

# 11. OECD Product Market Regulation
oecd_pmr_clean <- oecd_pmr |> 
  select(
    country_code,
    year,
    PMR_2018_3_3,
    PMR_2018_1_3,
    PMR_2018_6,
    PMR_2018_1_4,
    PMR_2018_1_2,
    PMR_2018_2_1,
    PMR_2018_1_1,
    PMR_2018_2_2
  ) |>
  clean_names() |> 
  rename_with(
    # replace prefixes with efi conventions
    ~ paste0("oecd_", .),
    .cols = starts_with("pmr")
  )

# 12. Open Budget Survey
open_budget_clean <- open_budget |> 
  select(
    country_code = ISO,
    year,
    ibp_obs_obi = obi
  ) |> 
  # fix cambodia ISO3
  mutate(
    country_code = if_else(
      country_code == "KMH",
      "KHM",
      country_code
    )
  ) |> 
  # shift values one year earlier, to reflect year of measurement
  # this holds starting in year = 2015
  mutate(
    year = if_else(
      year >= 2015,  year - 1, year
    )
  )

# 13. Debt Transparency
debt_transparency_clean <- debt_transparency |> 
  clean_names() |> 
  rename_with(
    # replace prefixses with efi conventions
    ~ paste0("wb_", .),
    .cols = starts_with("debt")
  )

# 14. GFDB Bank Concentration
gfdb_clean <- gfdb |> 
  clean_names() |> 
  rename_with(
    # replace prefixses with efi conventions
    ~ paste0("wb_", .),
    .cols = starts_with("gfdb")
  )

# 15. Women, Business and the Law
wbl_clean <- wbl |> 
  clean_names() |> 
  rename_with(
    # replace prefixses with efi conventions
    ~ paste0("wb_", .),
    .cols = starts_with("wbl")
  )
```

## Consolidate data

This section joins all of the datasets together to make the full data. The join is done by country code and year. This section excludes certain codes and alters others so that all of them fit the same standard. Once the country codes are standard, the excluded codes are filtered out and the datasets are joined. Lastly, any year before 1990 is excluded from the final set and the
columns are ordered by country name.


``` r
excluded_country_code <- c(
  "AIA", # anguilla
  "OECD", # OECD
  "SML", # somaliland
  "ZZB", # zanzibar
  "CUB", # cuba
  "PRK", # democratic people's republic of Korea
  "KMH", # unclear, listed in Open Budget Survey
  "PSG" # palestine and gaza (VDEM)
)

cliar_indicators <- list(
  efi_clean,
  fraser_clean,
  romelli_clean,
  oecd_epl_clean,
  spi_clean,
  aspire_clean,
  rise_clean,
  wdi_clean,
  vdem_clean,
  heritage_clean,
  oecd_pmr_clean,
  open_budget_clean,
  debt_transparency_clean,
  gfdb_clean,
  wbl_clean
) |> 
  map(
    # fix country codes for full join
    ~ mutate(
        .,
        country_code = case_when(
          country_code == "ZAR" ~ "COD", # democratic republic of congo
          country_code == "ROM" ~ "ROU", # romania
          T ~ country_code
        )
      ) |> 
      filter(
        !(country_code %in% excluded_country_code)
      )
  ) |> 
  reduce(
    full_join,
    by = c("country_code", "year")
  ) |> 
  filter(
    year >= 1990
  )
  
# order column names
cliar_indicators <- cliar_indicators %>%
  select(
    country_code,
    year,
    sort(colnames(.)),
    -starts_with("country_name")
  ) |> 
  arrange(
    country_code,
    year
  ) |> 
  mutate(
    index = row_number()
  )
```

## Data Quality Control: Indicator Selection

Verify that the indicators are selected correctly. To do this, take the non-removed indicators from the metadata file as the indicators from combined dataset created in step 5 and ensure they have the same contents by using an anti-join in both directions. Re-select indicators to ensure that only the indicators in metadata and v2 id cols are in data.


``` r
# verify that the indicators are selected correctly
db_variables_indicators <- db_variables |> 
  select(
    variable
  )


cliar_indicators <- cliar_indicators %>%
  # add country names
  left_join(
    wb_country_list |>
       distinct(country_code, country_name),
    by = "country_code"
  ) |>
  select(
    country_code,
    country_name,
    year,
    all_of(db_variables_indicators |> pull(variable))
  )

cliar_indicators_id <- cliar_indicators |>
  colnames()|>
  tibble(
    variable = colnames(cliar_indicators)
  )

test_that(
  "All indicators contained in metadata are in the CLIAR dataset",{
    expect_equal(
      nrow(
        db_variables_indicators |> 
          anti_join(cliar_indicators_id, by = "variable") |> 
          as.data.frame()
        ),
      0
    )
  }
)
```

```
## Test passed 🥇
```

## Compute family averages

This section computes family averages, dynamically adapting to the selection of indicators.


``` r
# compute family averages
cliar_indicators_long <-
  cliar_indicators %>%
  pivot_longer(
    any_of(vars_all),
    names_to = "variable"
  ) %>%
  select(-contains("gdp")) %>%
  left_join(
    db_variables %>%
      select(variable, var_name, family_name, family_var),
    by = "variable"
  )

# only calculate family averages for relevant institutional clusters
cliar_family_level_long <- cliar_indicators_long |>
  filter(
    family_var %in% vars_family
  ) |>
  group_by(
    country_code, year, family_var
  ) |>
  summarise(
    value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

cliar_family_level <- cliar_family_level_long |>
  pivot_wider(
    id_cols = c(country_code, year),
    names_from = family_var,
    names_glue = "{family_var}_avg",
    values_from = value
  )

cliar_indicators_clean <- cliar_indicators |>
  left_join(
    cliar_family_level,
    by = c("country_code", "year")
  ) |>
  filter(!country_code %in% c("DDR", "YMD"))
```




``` r
# Create a new row with specified values
# new_row <- tibble(
#   country_code = "CHI",
#   country_name = "Channel Islands",
#   year = 2023
# )
#Commenting out as this is causing issues

# Add NA for all other columns
# new_row <- new_row %>%
#   mutate(
#   add_column(across(everything(), ~ NA))
# )

# # Add the new row to the existing dataframe
# cliar_indicators_clean <- cliar_indicators_clean %>%
#   bind_rows(new_row)
# 
cliar_indicators_clean %>%
  filter(is.na(country_code))
```

```
## # A tibble: 0 × 443
## # ℹ 443 variables:
## #   country_code <chr>,
## #   country_name <chr>,
## #   year <dbl>,
## #   bs_sgi_195 <dbl>,
## #   bs_sgi_196 <dbl>,
## #   bs_bti_q1_2 <dbl>, …
```


## Data Quality Control: verify country_code and country_name consistency

This section ensures that the country_code code and the country name are consistent. Check this by performing an anti-join in both directions on the official WB list of country codes and names and the cliar indicators dataset.


``` r
# there are 218 country codes listed in the WB's official website
# https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups


# a. verify that all the country codes from the official data are included
test_that(
  "Verify that all the country codes from the official data are included",
  expect_equal(
    # number of rows is zero
    cliar_indicators_clean |> 
      distinct(country_code, country_name) |> 
        anti_join(
          wb_country_list |> 
            distinct(country_code, country_name),
          by = c("country_code", "country_name")
        ) |> nrow(),
      0
  )
)
```

```
## Test passed 🥇
```

``` r
# b. verify that cliar has distinct country-year
test_that(
  "Verify that CLIAR has distinct country years",{
    expect_equal(
      nrow(cliar_indicators_clean),
      cliar_indicators_clean |> 
        distinct(country_code, year) |> 
        nrow()
    )
  }
)
```

```
## Test passed 🥳
```

``` r
distinct_years_per_country <- cliar_indicators_clean %>%
  group_by(country_name) %>%
  summarize(distinct_years = n_distinct(year))

# c. verify that all countries have complete year coverage
test_that(
  "Verify that country codes have coverage for all years",{
    expect_equal(
      # calculate number of years covered by country
      cliar_indicators_clean |> 
        count(country_code) |> 
        pull(n) |> 
        unique(),
      ref_year - 1990
    )
  }
)
```

```
## Test passed 🥳
```

## Compute coverage diagnostics

### a. Internal coverage

Use the compute_coverage function from funs.R to create the coverage countries and years it is present for. With that information, percentage coverage, year range, percent of complete records, as well as standard distribution information such as mean and standard deviation are calculated.


``` r
cliar_indicators_diagnostic <- cliar_indicators_clean |>
  select(-country_name) |> 
  compute_coverage(country_code, year, ref_year - 5) |> 
  left_join(
    db_variables |> select(variable, var_name, source, family_name),
    by = c("Indicator" = "variable")
  ) |> 
  select(
    `Indicator`,
    `Indicator Name` = var_name,
    `Institutional Family` = family_name,
    everything(),
    `Data Source` = source
  ) |> 
  arrange(
    `Institutional Family`,
    Indicator
  )
```



### b. External coverage

``` r
### Post transformation
gtmi_mistery <- cliar_indicators_clean %>%
  select(year, starts_with("wb_gtmi_")) %>%
  filter(if_any(starts_with("wb_gtmi_"), ~ !is.na(.))) %>%
  group_by(year) %>%
  summarize(count = n())
```

Building on the 'compute_coverage' function, this subsection makes an assessment on the  available at a country-level information to create a table that overviews at range, latest year, missing years (vector) and missing years share (%) 


``` r
# Pivot long
cliar_indicators_long_diagnosis <- cliar_indicators_clean %>%
    filter(year >= 2000) |>
    pivot_longer(
      cols = -c(country_code, country_name, year),  # Exclude these columns
      names_to = "indicators", 
      values_to = "indicator_value"  
    )

# Apply coverage calculation from funs.R 
cliar_global_coverage <- cliar_indicators_long_diagnosis |>
  compute_global_coverage(country_name, indicators, year, indicator_value)
```

```
## Warning: There were 67224 warnings in
## `summarise()`.
## The first warning was:
## ℹ In argument: `year_range =
##   coverage_range_global(indicator_value,
##   year)`.
## ℹ In group 19: `country_name =
##   "Afghanistan"` `indicators =
##   "bs_sgi_195"`.
## Caused by warning in `min()`:
## ! no non-missing arguments to min; returning Inf
## ℹ Run
##   `dplyr::last_dplyr_warnings()`
##   to see the 67223 remaining
##   warnings.
```

``` r
# Renaming and joining to db_variables classification 
cliar_global_coverage_renamed <- cliar_global_coverage |>
  mutate(
    available_share = case_when(
      !is.na(available_share) ~ as.numeric(str_remove(available_share, "%")),
      TRUE ~ NA_real_) 
    )|>
  left_join( # Join with db_variables
    db_variables |> select(variable, var_name, source, family_name, benchmarked_ctf),
    by = c("indicators" = "variable")
  ) |>
  select( # Select and reorder columns
    everything(),
    indicators,
    var_name,
    family_name,
    source,
    benchmarked_ctf
  ) |>
  arrange(
    family_name,
    indicators
  )
                                     

# Cleaning for naming conventions
cliar_global_coverage_country <- cliar_global_coverage_renamed |>
  mutate(
    country_code = case_when(
      country_name == "Channel Islands" ~ "GGY", 
      country_name == "Kosovo" ~ "XKX", 
      TRUE ~ countrycode(country_name, "country.name", "iso3c") 
    )
  ) %>% 
  filter(!is.na(available_share))
```

```
## Warning: There was 1 warning in
## `mutate()`.
## ℹ In argument: `country_code =
##   case_when(...)`.
## Caused by warning:
## ! Some values were not matched unambiguously: Channel Islands, Kosovo
```

``` r
# Region naming for analysis
cliar_global_coverage_complete <- cliar_global_coverage_country |>
  left_join(
    country_income_and_region_updated,
    by = c("country_code")
  ) %>% 
  mutate(
      region = if_else(region == "North America", "Latin America & Caribbean", region)
             ) %>%
          filter(
           !is.na(region)
              ) %>% 
          mutate(
              region_short = recode(region,
                                 "Europe & Central Asia" = "ECA",
                                 "East Asia & Pacific" = "EAP",
                                 "Latin America & Caribbean" = "LAC",
                                 "Middle East & North Africa" = "MENA",
                                 "South Asia" = "SAR",
                                 "Africa Eastern and Southern" = "AFE",
                                 "Africa Western and Central" = "AFW",
                                 )
                  )
```




### c. CTF by year 

``` r
### CTF coverage
ctf_year_long_diagnosis <- cliar_indicators_clean %>%
                    filter(between(year,2019,2023)) |> 
                     pivot_longer(
                        cols = -c(country_code, country_name, year),  
                        names_to = "indicators", 
                        values_to = "indicator_value"  
                      ) |>
                  left_join( # Join with db_variables
                    db_variables |> 
                      select(variable,
                             var_name,
                             source,
                             family_name,
                             benchmarked_ctf),
                            by = c("indicators" = "variable")
                  ) |>
                  select( # Select and reorder columns
                    everything(),
                    indicators,
                    var_name,
                    family_name,
                    source,
                    benchmarked_ctf
                  ) |>
                  arrange(
                    family_name,
                    indicators
                  )
                                     

# Cleaning for naming conventions
ctf_year_rename <- ctf_year_long_diagnosis |>
  mutate(
    country_code = case_when(
      country_name == "Channel Islands" ~ "GGY", 
      country_name == "Kosovo" ~ "XKX", 
      TRUE ~ countrycode(country_name, "country.name", "iso3c") 
    )
  ) 
```

```
## Warning: There was 1 warning in
## `mutate()`.
## ℹ In argument: `country_code =
##   case_when(...)`.
## Caused by warning:
## ! Some values were not matched unambiguously: Channel Islands, Kosovo
```

``` r
# Region naming for analysis 
ctf_year_coverage_complete <- ctf_year_rename |>
  left_join(
    country_income_and_region_updated,
    by = c("country_code")
  ) %>% 
  mutate(
      region = if_else(region == "North America", "Latin America & Caribbean", region)
             ) %>%
          filter(
           !is.na(region)
              ) %>% 
          mutate(
              region_short = recode(region,
                                 "Europe & Central Asia" = "ECA",
                                 "East Asia & Pacific" = "EAP",
                                 "Latin America & Caribbean" = "LAC",
                                 "Middle East & North Africa" = "MENA",
                                 "South Asia" = "SAR",
                                 "Africa Eastern and Southern" = "AFE",
                                 "Africa Western and Central" = "AFW")
                  )
```

## Incorporate country income and region

We incorporate the country income group and region. Please note that there is no available data on income group for Venezuela (`country_code` == "VEN"). We retroactively classify income groups using 2023 data.


``` r
cliar_indicators_complete <- cliar_indicators_clean |> 
  left_join(
    country_income_and_region_updated,
    by = c("country_code")
  ) |> 
  select(
    country_code, country_name, income_group, region, year, everything()
  )
```

## Save data


``` r
write_rds(
  cliar_indicators_complete,
  here(
    "data",
    "output",
    "compiled_indicators.rds"
  )
)

cliar_indicators_diagnostic |> 
  write_rds(
    here(
      "data",
      "output",
      "diagnostics_compiled_indicators.rds"
    )
  )

#Write the input for the functions and the complete output
write_rds(
  cliar_indicators_long_diagnosis,
  here(
    "data",
    "output",
    "coverage_report_input.rds"
  )
)

write_rds(
  cliar_global_coverage_complete,
  here(
    "data",
    "output",
    "coverage_report_full_for_analysis.rds"
  )
)

# country groupings updated
country_income_and_region_updated |> 
    write_rds(
      here(
        "data",
        "output",
        "grouping_country_income_and_region_updated.rds"
      )
    )
```

<!--chapter:end:03-process-data.Rmd-->

# Clean list of contries

- Inputs:
  - `data/output/compiled_indicators.rds`
  - `data/input/wb/CLASS.xlsx`, obtained from https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups on September 1, 2022
  - `data/input/wb/group_list.csv`, input by the research team to list relevant groups
  - `data/output/grouping_country_income_and_region_updated.rds`, complementary list for groups in the case of splitting SSA into AFW and AFE
      
- Outputs:
  - `data/final/wb_country_list.rds`
  - `data/final/wb_country_groups.rds`

## Inputs


``` r
indicators <-                                                       
      read_rds(
      here(
        "data",
        "output",
        "compiled_indicators.rds"
      )
    )

group_list <-
    read_csv(
      here(
        "data",
        "input",
        "wb",
        "group_list.csv"
      )
    )
```

```
## Rows: 17 Columns: 2
## ── Column specification ────────
## Delimiter: ","
## chr (2): group_name, group_c...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
country_list <- read_xlsx(
      here(
        "data",
        "input", 
        "wb",
        "CLASS.xlsx"
      ),
      sheet = "compositions"
    ) %>%
    transmute(
      country_code = WB_Country_Code,
      country_name = WB_Country_Name,
      group = WB_Group_Name,
      group_code = WB_Group_Code
    ) |>
  # exclude non-WB member countries
  filter(
    country_code != "CUB" & 
      country_code != "PRK"
  )
  
# create a duplicate of the original dataset
region_list <- country_list

### Updated region grouping
  region_list_complement <-
    read_rds(
      here(
        "data",
        "output",
        "grouping_country_income_and_region_updated.rds"
      )
    )
```

## Subset country list

The only relevant countries are those we have some data for


``` r
country_list <-
  indicators %>%
  select(country_code) %>%
  unique %>%
  left_join(country_list)
```

```
## Joining with `by =
## join_by(country_code)`
```

## Subset groups


``` r
country_list <-
  country_list %>%
  filter(
    group %in% group_list$group_name
  ) %>%
  unique

### Define the indexing regions
target_regions <- c("Africa Eastern and Southern", "Africa Western and Central")

country_list_updated <- country_list %>%
  # Add Abbreviations to AFE and AFE regions in one dataframe
  left_join(
    region_list_complement %>% 
      mutate(region_code = recode(region,
                                  "Africa Eastern and Southern" = "AFE",
                                  "Africa Western and Central" = "AFW",
                                  .default = region)) %>% 
      transmute(country_code, detailed_region = region, detailed_region_code = region_code),
    by = "country_code"
  ) %>%
  # Index SSF region to reclassify it into AFE and AFW
  mutate(
    group_name      = if_else(group_code == "SSF" & detailed_region %in% target_regions,
                          detailed_region, group),
    group_code = if_else(group_code == "SSF" & detailed_region %in% target_regions,
                          detailed_region_code, group_code)
  ) %>%
  # Ensure that region_code follows the changes regions
  select(names(region_list)[1:2], group_name, group_code)
```

## Add AFE anhd AFW to Region list


``` r
group_list_updated <- group_list %>%
  # Remove the "Sub-Saharan Africa" row
  filter(group_name != "Sub-Saharan Africa") %>%
  # Append new rows with the desired groups and group_category "Region"
  bind_rows(
    tibble(
      group_name = c("Africa Eastern and Southern", "Africa Western and Central"),
      group_category = "Region"
    )
  ) %>% 
  arrange(group_category, group_name)
```

## Save datasets

Dataset with list of countries in our sample.


``` r
write_rds(
  country_list_updated,
  here(
    "data",
    "output",
    "wb_country_list.rds"
  )
)

write_rds(
  group_list_updated,
  here(
    "data",
    "output",
    "wb_country_groups.rds"
  )
)
```

<!--chapter:end:04-clean-countries.Rmd-->

# Calculate distance to frontier

- Inputs:
  - `data/output/compiled_indicators.rds`
  - `data/outut/country_list.rds`
  - `data/output/db_variables.rds`
      
- Outputs:
 - `data/output/closeness_to_frontier.rds`
 - `data/output/closeness_to_frontier_long.rds`
 - `data/output/closeness_to_frontier_dynamic.rds`
 - `data/output/closeness_to_frontier_dynamic_long.rds`

## Calculate global closeness to frontier

Closeness to frontier (CTF) is global, meaning that we identify the worst and best performance in the full sample (all countries). For each indicator $i$, we compare the last available value of indicator $i$ with the worst and best
performance for indicator $i$ among all countries and in the last $y$ years (2013 - most recent data).^[In the [Doing Business report](https://www.doingbusiness.org/content/dam/doingBusiness/media/Annual-Reports/English/DB17-Chapters/DB17-DTF-and-DBRankings.pdf) they consider the last 5 years, but here for some indicators we have shorter time series.]

## Keep only data from after 2013

Ideally, this will use data for the last 7 years in any given year.


``` r
#load in cleaned indicators from step 3
cliar_indicators <-
  read_rds(
    here(
      "data",
      "output",
      "compiled_indicators.rds"
    )
  ) %>%
  # filter to only years 2013 or later
  filter(
    year >= 2013
  )

#read in country list from step 4
country_list <- read_rds(
  here(
    "data",
    "output",
    "wb_country_list.rds"
  )
)

#read in db_variables
db_variables <- read_rds(
  here(
    "data",
    "output",
    "db_variables.rds"
  )
)

vars_ctf <- db_variables |>
  filter(
    benchmarked_ctf == "Yes"
  ) |>
  pull(variable)

# country income group and region
country_income_and_region <- read_xlsx(
      here(
        "data",
        "input", 
        "wb",
        "CLASS.xlsx"
      ),
      sheet = "List of economies",
      n_max = 219
    ) %>%
    transmute(
      country_code = Code,
      region = Region,
      income_group = `Income group`
    ) |>
  # exclude non-WB member countries
  filter(
    country_code != "CUB" & 
      country_code != "PRK"
  )
```

## Rescale indicators so a higher number denotes stronger institutions

V-DEM: corruption
PRM indicators: Countries are graded between 0 (less control/involvement) and 6 (more control/involvement). 
    In order to rescale these indicators, the years after 2018 are selected, 
    and then each indicator value is subtracted from 6. so that a value that was
    previously 0 for less control, is now 6, indicating a stronger institution.
    
    NOTE:
    Methodological note for PRM indicates that 1998 and 2013 indicators are 
    comparable, but not with 2018 due to change in methodology, so we only retain
    post-2018 data.
    
Enterprise Survey: Percent Of Firms Identifying X As A Major Constraint
    Subtract each indicator from 100, so that a low percentage of firm, for example
    10%, will now have a score of 90, indicating stronger institutions
    
Freedom house: Countries are graded between 1 (most free) and 7 (least free)
    Subtract each indicator from 8 so that a value of 1 for the most free is now 7,
    indicating stronger institutions.
    

``` r
#create new table to hold rescaled values
cliar_indicators_rescaled <- cliar_indicators |> 
  mutate(
    # V-DEM: political and executive corruption: flip direction because “The directionality of the V-Dem corruption index runs from less corrupt to more corrupt unlike the other V-Dem variables that generally run from less democratic to more democratic situation”.
    vdem_core_v2x_pubcorr = -1 * vdem_core_v2x_pubcorr,
    vdem_core_v2x_execorr = -1 * vdem_core_v2x_execorr,
    # V-DEM: political polarization, higher scores denote higher polarization, which can lead to institutional weakness
    # so we decide to flip the indicator by multiplying by -1
    vdem_core_v2cacamps = -1 * vdem_core_v2cacamps,
    # PRM indicators: Countries are graded between 0 (less control/involvement) and 6 (more control/involvement).
    # Methodological note for PRM indicates that 1998 and 2013 indicators are comparable,
    # but not with 2018 due to change in methodology, so we only retain post-2018 data.
    across(
      c(
        starts_with("oecd_pmr")
      ),
      ~ ifelse(year < 2018, NA, 6 - .x)
    ),
    # Enterprise Survey: Percent Of Firms Identifying X As A Major Constraint
    across(
      c(starts_with("wb_enterprisesurveys")),
      ~ 100 - .
    ),
    # Freedom house: Countries are graded between 1 (most free) and 7 (least free)
    across(
      c(starts_with("fh_fiw")),
      ~ (8 - .x)
    ),
    # WDI: ensure that pupil-teacher ratio increases lead to a worse score
    # note that because there is no pre-defined maximum for the ratio so we flip direction multiplying by -1
    across(
      c(wdi_seprmenrltczs, wdi_sesecenrltczs),
      ~ -1 * .x
    ),
    # GFDB:Bank concentration, the higher it is, the less competition. therefore indicator is flipped
    wb_gfdb_oi_01 = 100 - wb_gfdb_oi_01
  )
```

## Calculate country-level average for each indicator

For the static benchmark, we only calculate averages for indicators starting in the year 2019.


``` r
country_average <-
  #filter only data from years we are using
  cliar_indicators_rescaled %>%
  filter(between(year,2019,2023)) |> 
  #this groups countries together so average can be taken 
  group_by(
    country_code
  ) %>%
  summarise(
    across(
      all_of(c(vars_static_ctf)),
      ~ mean(., na.rm = TRUE)
    ),
    #Try switching with 2023 for max(year)
    wdi_nygdppcapppkd = wdi_nygdppcapppkd[year == 2023]
  )

country_last_year <-
  #filter only data from years we are using
  cliar_indicators_rescaled %>%
  filter(between(year,2019,2023)) |>  
  arrange(country_code, year) |> 
  #this groups countries together so average can be taken 
  group_by(
    country_code
  ) |> 
  select(
    all_of(c(vars_static_ctf))
  ) |> 
  # fill missing observations for each indicator with the latest available data
  fill() |> 
  # slice last available data for each country_code
  slice_tail() |> 
  ungroup()
```

```
## Adding missing grouping
## variables: `country_code`
```

## Identify worst and best performance for each indicator

Find this data both on a global time-scale, and for each individual year.

Inspect the entire dataset of rescaled indicator values - so including the indicator
values for every country year after 2013 - and identify the lowes tand highest 
values in each category. 


``` r
# static
min_max <-
  cliar_indicators_rescaled %>%
  filter(between(year,2019,2023)) |> 
  summarise(
    across(
      all_of(vars_static_ctf),
      list(
        min = ~ min(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE)
      ),
      .names = "{.col}-{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)-(.*)"
  )
```

```
## Warning: There were 12 warnings in
## `summarise()`.
## The first warning was:
## ℹ In argument: `across(...)`.
## Caused by warning in `min()`:
## ! no non-missing arguments to min; returning Inf
## ℹ Run
##   `dplyr::last_dplyr_warnings()`
##   to see the 11 remaining
##   warnings.
```

``` r
# dynamic: note that there are quite a few cases of Infinite warnings (due to missingness)
min_max_dynamic <- cliar_indicators_rescaled %>%
  filter(
    between(year,2014,2023)
  ) |> 
  summarise(
    across(
      all_of(vars_dynamic_ctf),
      list(
        min = ~ min(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE)
      ),
      .names = "{.col}-{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)-(.*)"
  ) %>%
  filter(!is.infinite(min) & !is.infinite(max))
```

## Calculate closeness to frontier at indicator level


``` r
ctf <-
  country_average %>%
  pivot_longer(
    all_of(vars_static_ctf),
    names_to = "variable"
  ) %>%
  left_join(
    min_max,
    by = "variable"
  ) %>%
  mutate(
    ctf = (min - value) / (min - max),
    ctf = ifelse(
      ctf == 0,
      0.01,
      ctf
    )
  ) %>%
  pivot_wider(
    id_cols = c("country_code"),
    names_from = "variable",
    values_from = "ctf"
  ) %>%
  select(-starts_with("gdp")) %>%
  left_join(
    country_average %>%
      select(country_code)
  )
```

```
## Joining with `by =
## join_by(country_code)`
```

``` r
# reproduce CTF scores using latest available data for each country-indicator
ctf_static_last_year <- country_last_year %>%
  pivot_longer(
    all_of(vars_static_ctf),
    names_to = "variable"
  ) %>%
  left_join(
    min_max,
    by = "variable"
  ) %>%
  mutate(
    ctf = (min - value) / (min - max),
    ctf = ifelse(
      ctf == 0,
      0.01,
      ctf
    )
  ) %>%
  pivot_wider(
    id_cols = c("country_code"),
    names_from = "variable",
    values_from = "ctf"
  ) %>%
  select(-starts_with("gdp")) %>%
  left_join(
    country_average %>%
      select(country_code)
  )
```

```
## Joining with `by =
## join_by(country_code)`
```

``` r
ctf_dynamic <-
  cliar_indicators_rescaled %>%
  pivot_longer(
    all_of(vars_dynamic_ctf),
    names_to = "variable"
  ) %>%
  left_join(
    min_max_dynamic,
    by = c("variable")
  ) %>%
  mutate(
    ctf_dyn = (min - value) / (min - max),
    ctf_dyn = ifelse(
      ctf_dyn == 0,
      0.01,
      ctf_dyn
    )
  ) %>%
  pivot_wider(
    id_cols = c("country_code", "year"),
    names_from = "variable",
    values_from = "ctf_dyn"
  ) %>%
  left_join(
    cliar_indicators_rescaled %>%
      select(country_code, year)
  )
```

```
## Joining with `by =
## join_by(country_code, year)`
```

## Calculate median per group

Group countries by regional, economic, or income groups and take indicator median
for those groups

``` r
# static
group_ctf <-
  #join country list with ctf
  country_list %>%
  left_join(
    ctf,
    by = "country_code"
  ) %>%
  #group by group code
  group_by(
    group_code, 
    group_name
  ) %>%
  #take median
  summarise(
    across(
      c(all_of(vars_static_ctf)),
      ~ median(., na.rm = TRUE)
    ),
    .groups = "drop"  # optionally ungroup after summarise
  ) %>%
  filter(!is.na(group_name)) %>%  # use group_name instead of group
  rename(
    country_name = group_name,  # rename group_name to country_name
    country_code = group_code
  )

#add group_ctf value to ctf dataset
ctf <- tibble::add_column(ctf, country_group = 0, .after = "country_code")
group_ctf <- tibble::add_column(group_ctf, country_group = 1, .after = "country_code")

# dynamic
group_ctf_dynamic <- country_list %>%
  #join country list with ctf
  left_join(
    ctf_dynamic,
    by = "country_code",
    relationship = "many-to-many"
  ) %>%
  # group by group_code, group_name, and year
  group_by(group_code, group_name, year) %>%
  # take median
  summarise(across(all_of(vars_dynamic_ctf),
                   ~ median(., na.rm = TRUE)),
            .groups = "drop") %>%  # optionally drop grouping
  # filter out rows with missing group_name
  filter(!is.na(group_name)) %>%
  # rename group_name to country_name and group_code to country_code
  rename(country_name = group_name,
         country_code = group_code)

ctf_dynamic <- add_column(ctf_dynamic, country_group = 0, .after = "country_code")
group_ctf_dynamic <- add_column(group_ctf_dynamic, country_group = 1, .after = "country_code")
```

## Clean CTF data and incorporate logged GDP per capita


``` r
# static
ctf <-
  ctf %>%
  # add country codes and names
  left_join(
    country_list |> distinct(country_code, country_name),
    by = c("country_code")
  ) |>
  # add gdp per capita (PPP) data
  # use average value (as in legacy ctf)
  left_join(
    country_average |> select(country_code, wdi_nygdppcapppkd),
    by = c("country_code")
  ) |>
  # rename and transform gdp per capita to log
  mutate(
    log_gdp = log(wdi_nygdppcapppkd)
  ) |>
  bind_rows(group_ctf) %>%
  ungroup() %>%
  arrange(country_name) |>
  select(
    country_code,
    country_name,
    everything()
  )

# dynamic
ctf_dynamic <-
  ctf_dynamic %>%
  # add country codes and names
  left_join(
    country_list %>% distinct(country_code, country_name),
    by = "country_code"
  ) %>%
  # add gdp per capita (PPP) data
  left_join(
    cliar_indicators %>% select(country_code, year, wdi_nygdppcapppkd),
    by = c("country_code", "year")
  ) %>%
  # rename and transform gdp per capita to log
  mutate(
    log_gdp = log(wdi_nygdppcapppkd)
  ) %>%
  bind_rows(group_ctf_dynamic) %>%
  ungroup() %>%
  arrange(country_name) %>%
  select(
    country_code,
    country_name,
    everything()
  )
```

## Convert to long-form

This changes the CTF dataset from wide form to long form and adds some additional 
data such as indicator family.


``` r
# static
ctf_long <-
  ctf %>%
  pivot_longer(
    all_of(vars_static_ctf),
    names_to = "variable"
  ) %>%
  select(-contains("gdp")) %>%
  left_join(
    db_variables %>%
      select(variable, var_name, family_name, family_var),
    by = "variable"
  ) %>%
  left_join(
    country_list %>%
      select(country_code, group_name),
    relationship = "many-to-many",
    by = "country_code",
  )

ctf_long_clean <-
  ctf_long %>%
  group_by(family_name, family_var, country_name, country_code, group_name, country_group) %>%
  summarise(value = median(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    variable = family_var,
    var_name = family_name
  ) %>%
  bind_rows(ctf_long) %>% 
  rename(group = group_name)
```

```
## `summarise()` has grouped
## output by 'family_name',
## 'family_var', 'country_name',
## 'country_code', 'group_name'.
## You can override using the
## `.groups` argument.
```

``` r
# dynamic
ctf_dynamic_long <-
  ctf_dynamic %>%
  pivot_longer(
    all_of(vars_dynamic_ctf),
    names_to = "variable"
  ) %>%
  select(-contains("gdp")) %>%
  left_join(
    db_variables %>%
      select(variable, var_name, family_name, family_var)
  ) %>%
  left_join(
    country_list %>%
      select(country_code, group_name),
    relationship = "many-to-many",
    by = "country_code",
  ) 
```

```
## Joining with `by =
## join_by(variable)`
```

``` r
ctf_dynamic_long_clean <-
  ctf_dynamic_long %>%
  group_by(family_name, family_var, country_name, country_code, country_group, group_name, year) %>%
  summarise(value = median(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    variable = family_var,
    var_name = family_name
  ) %>%
  bind_rows(ctf_dynamic_long) %>% 
  rename(group = group_name)
```

```
## `summarise()` has grouped
## output by 'family_name',
## 'family_var', 'country_name',
## 'country_code',
## 'country_group', 'group_name'.
## You can override using the
## `.groups` argument.
```

## Calculate family level data

Similarly to creating group-level data, this section calculates median CTF for each indicator family 

### Static


``` r
# static
ctf_static_family <- ctf |> 
  compute_family_average(
    vars = vars_static_family_ctf,
    db_variables = db_variables
  )

# dynamic
ctf_dynamic_family <- ctf_dynamic |> 
  # only retain even years because data is updated every two-years
  filter(
    year %% 2 == 0
  ) |> 
  compute_family_average(
    vars = c(vars_dynamic_family_ctf, vars_dynamic_partial_ctf),
    type = "dynamic",
    db_variables = db_variables
  )

# join family averages to ctfs
ctf_clean <- ctf |> 
  left_join(
    ctf_static_family,
    by = "country_code"
  )

ctf_dynamic_clean <- ctf_dynamic |> 
  left_join(
    ctf_dynamic_family,
    by = c("country_code", "year")
  )
```

## Diagnostics on family averages

Take the family averages for dynamic data.


``` r
# select family averages to diagnose
vars_family_static_ctf <- ctf_static_family |> 
  select(ends_with("avg")) |> 
  colnames() |> 
  str_replace(
    "_avg",
    ""
  )

vars_family_dynamic_ctf <- ctf_dynamic_family |> 
  select(ends_with("avg")) |> 
  colnames() |> 
  str_replace(
    "_avg",
    ""
  )

# missingness by institutional family average ### [KEY FOR GRAPH]
# static
ctf_static_family |> 
  select(ends_with("avg")) |> 
  naniar::gg_miss_var(
    show_pct = TRUE
  ) +
  ggtitle(
    "Percentage of Missing Institutional Family Averages: Static CTF",
    subtitle = "Unit of Analysis: Country"
  )
```

<img src="_main_files/figure-html/unnamed-chunk-25-1.png" width="672" />

``` r
# dynamic
ctf_dynamic_family |> 
  select(ends_with("avg")) |> 
  naniar::gg_miss_var(
    show_pct = TRUE
  ) +
  ggtitle(
    "Percentage of Missing Institutional Family Averages: Dynamic CTF",
    subtitle = "Unit of Analysis: Country-Year"
  )
```

<img src="_main_files/figure-html/unnamed-chunk-25-2.png" width="672" />

``` r
# static family averages diagnosis
theme_set(
  theme_minimal()
)


vars_family_static_ctf |> 
  set_names(vars_family_static_ctf) |> 
  map_dfr(
    ~ select(
      ctf,
      all_of(
        intersect(vars_static_ctf, get(.))
        )
      )|> 
        miss_var_summary(),
    .id = "family_var"
  ) |> 
  left_join(db_variables) |> 
  select(-variable) |> 
  rename(variable = var_name) |> 
  ggplot(
    aes(pct_miss, variable)
  ) +
  geom_col() +
  geom_label(
    aes(label = round(pct_miss(variable)))
  ) +
  facet_wrap(
    vars(family_var),
    scales = "free_y",
    ncol = 2
  ) +
  ggtitle(
    "Missingness by Indicator in the Static Benchmarking"
  )
```

```
## Joining with `by =
## join_by(family_var, variable)`
```

<img src="_main_files/figure-html/unnamed-chunk-25-3.png" width="672" />

``` r
# dynamic family averages diagnosis
vars_family_dynamic_ctf |> 
  set_names(vars_family_dynamic_ctf) |> 
  map_dfr(
    ~ select(
      ctf,
      all_of(
        intersect(vars_dynamic_ctf, get(.))
        )
      )|> 
        miss_var_summary(),
    .id = "family_var"
  ) |> 
  left_join(db_variables) |> 
  select(-variable) |> 
  rename(variable = var_name) |> 
  ggplot(
    aes(pct_miss, variable)
  ) +
  geom_col() +
  geom_label(
    aes(label = round(pct_miss(variable)))
  ) +
  facet_wrap(
    vars(family_var),
    scales = "free_y",
    ncol = 2
  ) +
  ggtitle(
    "Missingness by Indicator in the Dynamic Benchmarking"
  )
```

```
## Joining with `by =
## join_by(family_var, variable)`
```

<img src="_main_files/figure-html/unnamed-chunk-25-4.png" width="672" />

## Data Quality Control

Test that all expected indicators and countries are covered

``` r
test_that(
  "All countries are covered",
  {
    expect_setequal(
      ctf_clean |> filter(country_group == 0) |> distinct(country_code) |> pull(),
      country_list |> distinct(country_code) |> pull()
    )
    expect_setequal(
      ctf_dynamic_clean |> filter(country_group == 0) |> distinct(country_code) |> pull(),
      country_list |> distinct(country_code) |> pull()
    )
  }
)
```

```
## Test passed 🌈
```

``` r
test_that(
  "All indicators are covered",
  {
    ## Shel added _avg to the pattern to take care of the new family level indicators (that all have an _avg suffix)
    expect_setequal(
      ctf_clean |> colnames() |> str_subset("year|country|gdp|_avg$", negate = TRUE),
      vars_static_ctf
    )
    expect_setequal(
      ctf_dynamic_clean |> colnames() |> str_subset("year|country|gdp|_avg$", negate = TRUE),
      vars_dynamic_ctf
    )
  }
)
```

```
## Test passed 🥇
```

## Update db_variables to contain the family averages


``` r
db_variables <- db_variables %>% 
  mutate(
    across(where(is.character), str_squish)
  ) |> 
  rename(
    rank_id = indicator_order
  ) |> 
  mutate(
    rank_id = rank_id + 1
  )

# add family level vars
family_level_vars <- db_variables %>% 
  distinct(family_var, family_name) %>% 
  rowwise() %>% 
  mutate(
    variable = paste0(family_var, "_avg"),
    var_name = paste0(family_name, " Average"),
    var_level = "indicator",
    description = "The cluster-level average is an unweighted average of the corresponding and included indicators of this cluster. See Methodological note for details on the inclusion criteria.",
    description_short = "The cluster-level average is an unweighted average of the corresponding and included indicators for this cluster.",
    source = "CLIAR",
    benchmarked_ctf = "Yes",
    rank_id = 1
  )

db_variables <- db_variables %>% 
  bind_rows(family_level_vars) %>%
  arrange(family_var, rank_id)
```

## Add income group and region

In this section, we add income groups and region. Note that the CTF includes groups as observations, meaning that not all rows are expected to have an income group or region.


``` r
ctf_complete <- ctf_clean |> 
  left_join(
    country_income_and_region,
    by = c("country_code")
  ) |> 
  select(
    country_code, country_name, income_group, region, everything()
  )

ctf_dynamic_complete <- ctf_dynamic_clean |> 
  left_join(
    country_income_and_region,
    by = c("country_code")
  ) |> 
  select(
    country_code, country_name, income_group, region, year, everything()
  )
```

## Institutional Family-level Variance

We compute the dispersion of our institutional family-level scores by country. For the static benchmarking, we provide three metrics:

1. Minimum CTF scores for indicators in the same family, for a given country.
2. Maximum CTF scores for indicators in the same family, for a given country.
3. The variance of CTF scores for indicators in the same family, for a given country.

In the case of the dynamic benchmarking we extend our analysis by specifying min-max and standard deviation for a given country and year.


``` r
ctf_static_variance <- ctf_complete |> 
  compute_family_variance(
    vars_static_ctf,
    db_variables = db_variables
  )

ctf_dynamic_variance <- ctf_dynamic_complete |> 
  compute_family_variance(
    vars_dynamic_ctf,
    type = "dymamic", 
    db_variables
  )
```

## Robustness checks: 5-year average vs. Last available data

This section produces a set of robustness checks on the computation of the static CTF, comparing the 5-year average scores with the scores computed using the latest available data. We conduct two tests: (a) a scatterplot, where the unit of analysis is a country-indicator combination and (b) a histogram of the correlation for each indicator. 

For (a), we plot the values of CTF-scores for the 5-year average against the last-year available, using as the join keys the country code and variable. We find that the CTF scores produced using these different approaches are along the 45 degree line, indicating that they are strongly correlated and consistent with one another.

For (b), we summarize the CTF scores presented in (a) at the indicator level, computing the correlation between the CTF scores using 5-year averages and last-year values for each indicator. For example, for the indicator "Separation of powers", we calculate the correlation of its two types of CTF scores (5-year vs. last-year) across countries. This gives us a final correlation score at the indicator level, e.g., `0.982`. The same procedure is repeated for each indicator. Finally, we produce a histogram of all correlation scores at the indicator level. We find that the majority of scores (96.4%) have a correlation above 0.95. 


``` r
ctf_static_last_year_long <- ctf_static_last_year |> 
  pivot_longer(
    cols = c(
      all_of(vars_static_ctf)
    ),
    names_to = "variable",
    values_to = "ctf_last_year"
  )

ctf_static_long <- ctf_clean |> 
  filter(country_group == 0) |> 
  select(country_code, all_of(vars_static_ctf)) |> 
  pivot_longer(
    cols = c(
      all_of(vars_static_ctf)
    ),
    names_to = "variable",
    values_to = "ctf_year_average"
  )

# merge datasets using country code and variable to compute correlation
ctf_robustness <- ctf_static_last_year_long |> 
  inner_join(
    ctf_static_long,
    by = c("country_code", "variable")
  )

# (a) scatterplot of last year vs. year-average ctf scores
ctf_robustness |> 
  ggplot() +
  geom_point(
    aes(ctf_year_average, ctf_last_year),
    color = "steelblue3",
    alpha = 0.7
  ) +
  labs(
    x = "CTF score: 5-year average",
    y = "CTF score: latest available data",
    caption = "The unit of analysis is at the country-indicator level. Please note that indicators for which either value were missing are not plotted."
  ) +
  ggtitle(
    "Correlation between CTF scores computed using (a) 5-Year Average and (b) Last-Year values for indicators"
  ) +
  theme_minimal()
```

```
## Warning: Removed 19869 rows containing
## missing values or values
## outside the scale range
## (`geom_point()`).
```

<img src="_main_files/figure-html/unnamed-chunk-30-1.png" width="672" />

``` r
# (b) distribution of correlations by indicator
ctf_robustness |> 
  group_by(variable) |> 
  summarise(
    correlation = cor(ctf_last_year, ctf_year_average, use = "pairwise.complete", method = "pearson")
  ) |> 
  ggplot() +
  geom_histogram(
    aes(correlation, y = stat(width*density), binwidth = 0.01)
  ) +
  geom_vline(
    xintercept = 0.95,
    linetype = "dashed",
    color = "red3"
  ) +
  scale_y_continuous(
    labels = percent_format()
  ) +
  coord_cartesian(
    xlim = c(0.9, 1)
  ) +
  labs(
    x = "Correlation between CTF scores",
    y = "Percentage of Indicators",
    caption = "Dashed red line indicators a correlation above 0.95. 5 out of 137 (3.6%) indicators have a correlation below 0.95. No indicators have a correlation between 0.9."
  ) +
  ggtitle(
    "Distribution of indicator-level correlations of CTF scores"
  ) +
  theme_minimal()
```

```
## Warning in geom_histogram(aes(correlation, y = stat(width * density), binwidth = 0.01)): Ignoring unknown aesthetics:
## binwidth
```

```
## `stat_bin()` using `bins = 30`.
## Pick better value with
## `binwidth`.
```

```
## Warning: Removed 52 rows containing
## non-finite outside the scale
## range (`stat_bin()`).
```

<img src="_main_files/figure-html/unnamed-chunk-30-2.png" width="672" />

## Write-out data


``` r
write_rds(
  ctf_complete,
  here(
    "data",
    "output",
    "closeness_to_frontier.rds"
  )
)

write_rds(
  ctf_long_clean,
  here(
    "data",
    "output",
    "closeness_to_frontier_long.rds"
  )
)

write_rds(
  ctf_dynamic_complete,
  here(
    "data",
    "output",
    "closeness_to_frontier_dynamic.rds"
  )
)

write_rds(
  ctf_dynamic_long_clean,
  here(
    "data",
    "output",
    "closeness_to_frontier_dynamic_long.rds"
  )
)

write_rds(
  ctf_static_variance,
  here(
    "data",
    "output", 
    "closeness_to_frontier_var_static.rds"
  )
)

write_rds(
  ctf_dynamic_variance,
  here(
    "data",
    "output", 
    "closeness_to_frontier_var_dynamic.rds"
  )
)

write_rds(
  db_variables,
  here(
    "data",
    "output",
    "db_variables.rds"
  )
)
```


<!--chapter:end:05-ctf.Rmd-->

# Create spatial data

- Inputs:
  - `data/final/closeness_to_frontier.rds`
  - `data/final/compiled_indicators.rds`
  - `data/raw/WB_countries_Admin0_lowres.geojson`, obtained from https://datacatalog.worldbank.org/int/search/dataset/0038272 on September 1, 2022
  - `data/raw/WB_disputed_areas_Admin0_10m_lowres.geojson`, obtained from https://datacatalog.worldbank.org/int/search/dataset/0038272 on September 1, 2022
  
- Output:
  - `data/final/indicators_map.rds`

## Input data 


``` r
ctf <-
  read_rds(
    here(
      "data",
      "output",
      "closeness_to_frontier.rds"
    )
  )
avg_columns = names(ctf)[grep("_avg", names(ctf))]
raw_indicators <-
  read_rds(
    here(
      "data",
      "output",
      "compiled_indicators.rds"
    )
  )

db_variables <-
  read_rds(
    here(
      "data",
      "output",
      "db_variables.rds"
    )
  )
```

## Official WB maps 


``` r
world_map <-
  read_sf(
    here(
      "data",
      "input",
      "wb",
      "WB_countries_Admin0_lowres.geojson"
    )
  )

disputed_areas <-
  read_sf(
    here(
      "data",
      "input",
      "wb",
      "WB_disputed_areas_Admin0_10m_lowres.geojson"
    )
  )
```

## Clean maps 

In this section, we combine the world map data with disputed areas, in order to address potential boundary conflicts. We also simplify the world map through the `st_simplify` command in order to improve loading performance on our Shiny App.


``` r
disputed_areas <-
  disputed_areas %>%
  transmute(country_code = str_trim(WB_A3)) %>%
  filter(
    !is.na(country_code),
    country_code != ""
  )

world_map <-
  world_map %>%
  select(country_code = WB_A3) 

world_map <-
  world_map %>%
  bind_rows(
    disputed_areas
  )

# simplify map to improve loading performance
simple_world_map <-
  world_map %>%
  # fix wrapping of dateline to avoid spurious ribbon
  # source: https://github.com/r-spatial/sf/issues/1046
  st_transform(4326) %>% 
  st_wrap_dateline() |> 
  # project into robinson coordinate system
  st_transform(crs = '+proj=robin') %>%
  # simplify polygons to improve rendering
  st_simplify(
    dTolerance = 0.05
  ) 
```


## Combine maps and data

## Closeness to frontier


``` r
ctf <-
  ctf %>%
  pivot_longer(
    cols = all_of(c(vars_static_ctf, avg_columns)),
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


``` r
raw <-
  raw_indicators %>%
  select(
    -c(income_group, region)
  ) %>%
  pivot_longer(
    cols = 4:ncol(.)
  ) %>%
  filter(!is.na(value)) %>%
  group_by(country_code,name) %>%
  filter(year == max(year)) %>%
  pivot_wider(
    values_from = c(value, year),
    names_from = name,
    id_cols = country_code
  )

final_world_map <-
  world_map %>%
  left_join(
    raw
  ) %>%
  left_join(
    ctf
  )
```

```
## Joining with `by =
## join_by(country_code)`
## Joining with `by =
## join_by(country_code)`
```

## Save datasets


``` r
final_world_map %>%
  write_rds(
    here(
      "data",
      "output",
      "indicators_map.rds"
    )
  )
```

<!--chapter:end:06-map.Rmd-->

# Move final data to app folder


``` r
file.copy(
  list.files(
    here(
      "data",
      "output"
    ),
    full.names = TRUE
  ),
  here(
    "..",
    "app",
    "data"
  ),
  recursive = TRUE,
  overwrite = TRUE
)
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE
##  [6] TRUE TRUE TRUE TRUE TRUE
## [11] TRUE TRUE TRUE TRUE TRUE
## [16] TRUE
```

<!--chapter:end:07-copy-final-data.Rmd-->

