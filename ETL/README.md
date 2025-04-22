
<!-- README.md is generated from README.Rmd. Please edit that file -->

# REPRODUCIBILITY PACKAGE

This script reproduces the data used to produce the CLIAR dashbaord. All
you have to do is:

- Please reach out to Public Institutions Data and Analytics Team for
  the input data which you should place in the `data/input` folder.
- Open the `code.Rproj` in the base directory of the ETL folder.
- Simply click the knit button to the `README.Rmd` file.

## Load List of selected indicators

## Process Data

    #> Test passed 🎉
    #> # A tibble: 0 × 445
    #> # ℹ 445 variables: country_code <chr>, country_name <chr>, year <dbl>,
    #> #   wb_aspire_adequacy_benefits <dbl>, wb_aspire_coverage <dbl>,
    #> #   bs_bti_q1_2 <dbl>, bs_bti_q12_1 <dbl>, bs_bti_q12_2 <dbl>,
    #> #   bs_bti_q14 <dbl>, bs_bti_q14_1 <dbl>, bs_bti_q14_2 <dbl>,
    #> #   bs_bti_q14_3 <dbl>, bs_bti_q15_1 <dbl>, bs_bti_q15_2 <dbl>,
    #> #   bs_bti_q2_1 <dbl>, bs_bti_q2_3 <dbl>, bs_bti_q3_1 <dbl>, bs_bti_q3_2 <dbl>,
    #> #   bs_bti_q7_2 <dbl>, bs_bti_q7_4 <dbl>, bs_bti_q8_1 <dbl>, …
    #> Test passed 🎊
    #> Test passed 🌈
    #> Test passed 🎉

## Clean List of Countries

## Calculate distance to frontier

Closeness to frontier (CTF) is global, meaning that we identify the
worst and best performance in the full sample (all countries). For each
indicator $i$, we compare the last available value of indicator $i$ with
the worst and best performance for indicator $i$ among all countries and
in the last $y$ years (2013 - most recent data).[^1]

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

    #> Test passed 🌈
    #> Test passed 🥇

![](README_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->

## Output the spatial data

    #> Simple feature collection with 257 features and 1181 fields
    #> Geometry type: MULTIPOLYGON
    #> Dimension:     XY
    #> Bounding box:  xmin: -180 ymin: -59.47194 xmax: 180 ymax: 83.6341
    #> Geodetic CRS:  WGS 84
    #> # A tibble: 257 × 1,182
    #>    country_code                                     geometry value_wb_gfdb_di_01
    #>    <chr>                                  <MULTIPOLYGON [°]>               <dbl>
    #>  1 IDN          (((117.7036 4.163415, 117.8386 4.040188, 11…                31.9
    #>  2 MYS          (((117.7036 4.163415, 117.907 4.156683, 117…               128. 
    #>  3 CHL          (((-69.51009 -17.50659, -69.6839 -17.65604,…                80.6
    #>  4 BOL          (((-69.51009 -17.50659, -69.49712 -17.6214,…                76.0
    #>  5 PER          (((-69.51009 -17.50659, -69.5226 -17.36912,…                47.8
    #>  6 ARG          (((-67.28475 -23.83414, -67.36237 -24.03037…                15.4
    #>  7 CYP          (((32.6262 35.16509, 32.64069 35.18708, 32.…               181. 
    #>  8 IND          (((76.82459 35.6478, 76.77735 35.64611, 77.…                51.9
    #>  9 CHN          (((110.6851 20.15331, 110.5693 20.07978, 11…               178. 
    #> 10 ISR          (((35.60385 33.24009, 35.54949 33.28102, 35…                70.2
    #> # ℹ 247 more rows
    #> # ℹ 1,179 more variables: value_imf_world_rt_rm_gdp <dbl>,
    #> #   value_wb_wwbi_bi_wag_totl_gd_zs <dbl>,
    #> #   value_wb_wwbi_bi_wag_totl_pb_zs <dbl>, value_vars_hrm_avg <dbl>,
    #> #   value_vars_mkt_avg <dbl>, value_wdi_nygdpminrrtzs <dbl>,
    #> #   value_wdi_nygdpngasrtzs <dbl>, value_wdi_nygdppetrrtzs <dbl>,
    #> #   value_wdi_nygdptotlrtzs <dbl>, value_wdi_nygnpmktpppkd <dbl>, …

## Move final data to app folder

    #>  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    #> [16] TRUE TRUE

[^1]: In the [Doing Business
    report](https://www.doingbusiness.org/content/dam/doingBusiness/media/Annual-Reports/English/DB17-Chapters/DB17-DTF-and-DBRankings.pdf)
    they consider the last 5 years, but here for some indicators we have
    shorter time series.
