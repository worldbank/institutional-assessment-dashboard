
# Overview

Our goal is to provide you with an overview of the data infrastructure for CLIAR, describing entities, their relationships, as well as a description on the data pipeline, including (1) data extraction, (2) data quality controls, (3) data transformation and (4) data loading. As such, the document provides an overview of the data ETL process for CLIAR. We also present an overview of how data from CLIAR is used in the CLIAR Dashboard, providing an end-to-end documentation of how data is applied for analytics. This documentation is replicated in `bookdown`.

To run the pipeline and render the documentation, please open the `index.Rmd file`. There, you have the option to either point and click on `RStudio` using the `Build Book` command, or enter the `bookdown::render_book()` command on the console. If instead, you plan on testing particular sections in the pipeline, you may simply run the `00-setup.Rmd` file to load all required packages, and test individual sections.

## Data Pipeline:

This section describes the different stages of data processing in CLIAR. We describe these different stages using the convention nomenclature Extraction-Transformation-Loading (ETL). We focus on these three stages:

1.	Data Extraction: this section describes the different data sources extracted by the CLIAR data pipeline and how they are accessed. Currently, the data is being imported from two sources:
    a.	EFI 360 API: GovData360 is an initiative of the World Bank’s Governance Global Practice (GGP). It contains more than 4,700 governance-related indicators on state capacity, efficiency, openness, inclusiveness, accountability, integrity, and trust in government. The site gathers information from 35 data sources, including other World Bank sources. Gov360 is a powerful R Package that facilitates the retrieval and analysis of data related to various indicators. 
    b. Original Data: The team extracts indicators from an original table of indicators manually produced by the legacy team (merged_for_residuals.rds). This dataset should be updated, and ensure that the indicators are extracted from the original sources instead of a legacy file.
    c. Central Bank Independence Data: The database contains information on the CBIE index in (Romelli, 2022), as well as the most used indices of central bank independence for 154 countries between 1972 and 2017. Using the dta file provided, we extract the variable LVAU and use that for representing the central bank independence. 

2.	Data Quality Controls: this section describes the different protocols used to assess the quality of the data once it is ingested. In particular, we highlight (1) the quantitative review of data imports, as well as (2) inclusion criteria for CLIAR.
    a.	Note: Currently, the data quality controls are not being applied to the Data Extraction. That is because in the legacy system, no quality controls were applied. This will be modified in the next iteration of CLIAR. Our assessment is that the quality controls should be applied at the Data Extraction step. 

3.	Data Transformation: this section describes how data is transformed once it has been both ingested and vetted for inclusion within CLIAR. This includes transformation of indicators such as normalization, as well as the application of the Closeness-to-Frontier methodology.
    a.	For the governence data pertaining to Oil and Gas, we combine them and take the average of the indicators
    b.	GCI indicators are supposed to take values from 1 to 7. However, each indicator has one country with a score above 7 and replace any values above 7 with NA.
    c.	For computation of CTF, we filter data post 2013 and we rescale indicators so a higher number is always a better performance.
    d.	Later, we calculate the country-level average for each indicator, identify worst and best performance for each indicator and finally compute closeness to frontier at indicator level by using the min –max scaling.

4.	Data Loading: this section describes how data is exported for use by the CLIAR dashboard. Note that individual TTLs may request data to be exported to them for customized analysis. As such, this section should also accommodate these use-cases.
    a.	We have two stream of codes. One for data preparation and other for the application. Once the data preparation and processing has been completed, the processed data is copied to the data folder inside the application (app) folder. This powers the functioning of the application locally. 
    b.	Once we host the application on Posit, the app folder (which includes data folder) is completely copied to the R server from where the application runs.
    c.	Note: The loading of the data is being handled in the backend by Posit Connect. This means that all of the data contained in the GitHub repository is being converted into a back-end sqlite file, which is then accessed by the Shiny App. There is no circumventing it, but what the team should aim for is to directly load a sqlite file into the RShiny repository, so that the database itself is loaded into PositConnect. Additionally, the sqlite file should be stored in a secure folder in the World Bank’s OneDrive, instead of living locally in each one of the team member’s computers.
