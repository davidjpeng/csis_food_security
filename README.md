# Overview

This is public repository for *Beyond Prevalence: Why Population Counts Matter for Food Insecurity Policy*. 

- Editors and reviewers should access the private repository we have provided instead, as it includes data to facilitate the peer-review process based on special data sharing agreements for those parties only. 

- The public may access data required to reproduce our estimates of food-insecure population counts across the urban-rural continuum through a combination of public downloads and data access requests. We direct interested parties to the Data Availability section of our paper for more information. 

# Steps 

## Country-level estimates 

1. We provide code for a single country, Afghanistan in *food_security_fies_degurba_afg.R*. The method can be adapted for other countries for which there is available data. 

2. At the end of the file, a new folder in your working directory, */results/* will be created from the code where output tables are saved as .csv. 

3. We also provide .csv aggregation of results across all countries in our analysis located at */combined_aggregate_results/summary_tables.csv* that is a combination of each country's output tables. 

## Tables and Graphics 

1. Within */graphics/* you will find individual .R files that generate the various tables and figures used in our main manuscript and also supplemental discussion. 

2. The naming convention of each file will point you to which table or figure should you choose to reproduce them. 

3. An empty folder located in */graphics/output/* is already available to store output should you choose to recreate them. 

4. Figure 3 and Table 3 cannot be run without microdata, but other graphics are generated based on the *summary_tables.csv*. 

# Dependencies 

- For more information should you run into dependency issues, the authors' sessionInfo() within R is provided below: 

R version 4.4.2 (2024-10-31)
Platform: x86_64-pc-linux-gnu
Running under: Pop!_OS 22.04 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0 
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

time zone: America/New_York
tzcode source: system (glibc)

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] officer_0.6.8        flextable_0.9.7      plotly_4.10.4        patchwork_1.3.1      scales_1.3.0         survey_4.4-2         survival_3.8-3       Matrix_1.7-2        
 [9] flexurba_0.0.1.0     tidyterra_0.7.0      terra_1.8-21         exactextractr_0.10.0 sfheaders_0.4.4      sf_1.0-19            readxl_1.4.3         haven_2.5.4         
[17] lubridate_1.9.4      forcats_1.0.0        stringr_1.5.1        dplyr_1.1.4          purrr_1.0.4          readr_2.1.5          tidyr_1.3.1          tibble_3.2.1        
[25] ggplot2_3.5.1        tidyverse_2.0.0     

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.1        viridisLite_0.4.2       farver_2.1.2            nngeo_0.4.8             lazyeval_0.2.2          fastmap_1.2.0           fontquiver_0.2.1       
 [8] digest_0.6.37           timechange_0.3.0        lifecycle_1.0.4         magrittr_2.0.3          compiler_4.4.2          rlang_1.1.5             tools_4.4.2            
[15] utf8_1.2.4              yaml_2.3.10             data.table_1.17.0       knitr_1.49              askpass_1.2.1           labeling_0.4.3          htmlwidgets_1.6.4      
[22] bit_4.5.0.1             sp_2.2-0                classInt_0.4-11         xml2_1.3.6              rsconnect_1.3.4         KernSmooth_2.23-26      withr_3.0.2            
[29] gdtools_0.4.2           geos_0.2.4              e1071_1.7-16            colorspace_2.1-1        cli_3.6.4               rmarkdown_2.29          crayon_1.5.3           
[36] ragg_1.3.3              generics_0.1.3          rstudioapi_0.17.1       httr_1.4.7              tzdb_0.4.0              DBI_1.2.3               proxy_0.4-27           
[43] splines_4.4.2           parallel_4.4.2          cellranger_1.1.0        mitools_2.4             vctrs_0.6.5             fontBitstreamVera_0.1.1 jsonlite_1.9.0         
[50] hms_1.1.3               bit64_4.6.0-1           systemfonts_1.2.1       units_0.8-5             glue_1.8.0              codetools_0.2-19        libgeos_3.11.1-2       
[57] stringi_1.8.4           gtable_0.3.6            raster_3.6-31           munsell_0.5.1           pillar_1.10.1           htmltools_0.5.8.1       openssl_2.3.2          
[64] R6_2.6.1                textshaping_1.0.0       wk_0.9.4                vroom_1.6.5             evaluate_1.0.3          lattice_0.22-5          fontLiberation_0.1.0   
[71] class_7.3-23            zip_2.3.2               uuid_1.2-1              Rcpp_1.0.14             fastmatch_1.1-6         xfun_0.52               pkgconfig_2.0.3    

- In addition, you may need to manually install GDAL, depending on your operating system because the geospatial packages sf and terra rely on this C++ library. 

- See https://gdal.org/en/stable/ and https://r-spatial.github.io/sf/ for more information. 

- Author's have tested this private repo on both Linux and Mac successfully. 