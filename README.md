# ShinyR-DAM

### Authors 
Karol Cichewicz, Jay Hirsh, University of Virginia, Charlottesville, VA

### Short description
ShinyR-DAM is an application for analyzing Drosophila locomotor activity, sleep, and circadian periodicity recorded by the Drosophila Activity Monitor (DAM) system, developed and manufactured by TriKinetics (Waltham, MA). Our program operates in the cloud and can be accessed via this link: https://karolcichewicz.shinyapps.io/shinyr-dam/ , or deployed locally using RStudio. For optimization and grant writing purposes, we track the usage of our app using google analytics java script included in this repository. We do not collect any information about the data processed by our program. 

The source code is divided into the app.R file with the main program structure, and multiple *_ui.R and *_server.R files comprising pairs of the user interface and the algorithms used in the specific tabs of the program. *_ui.R and *_server.R files are sourced in  app.R.

### Testing
For testing, we provide 5 monitor files in this repository. Each monitor file contains data of 32 flies, each representing a unique condition (genotype). There are 5 LD days: 23-06-2017 - 27-06-2017; and 5 DD days: 28-06-2017 - 02-07-2017 in this dataset. Light onset time was set at 6 AM, and acquisition frequency was set to 1 min. 

### Abstract
We developed a web application ShinyR-DAM for analyzing Drosophila locomotor activity, sleep, and circadian rhythms recorded by the Drosophila Activity Monitor (DAM) system (TriKinetics, Waltham, MA). The DAM system measures locomotor activity as infrared beam break counts of flies walking in glass tubes. It allows long-term recording of behavior, making it particularly suitable for circadian biology studies. Comparing with the existing programs for DAM data analysis, ShinyR-DAM substantially decreases the complexity and time required to analyze the data, producing aesthetically pleasing plots, summary tables, and data files for further statistical analysis. Our program has an intuitive graphical user interface that enables novice users to quickly learn the analyses. It runs in a web browser and requires no installation on a local computer, nor programming skills. 

### License
ShinyR-DAM source code is provided under the GPLv3 free software license.

### Current ShinyR-DAM version: 3.0

### R session info:
R version 3.4.3 (2017-11-30)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] bindrcpp_0.2        colourpicker_1.0    lubridate_1.7.1     Kmisc_0.5.0        
 [5] data.table_1.10.4-3 gridExtra_2.3       scales_0.5.0        gtools_3.5.0       
 [9] zoo_1.8-1           dplyr_0.7.4         ggplot2_2.2.1       plyr_1.8.4         
[13] shiny_1.0.5        

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.14     pillar_1.1.0     compiler_3.4.3   bindr_0.1        bitops_1.0-6    
 [6] tools_3.4.3      digest_0.6.14    evaluate_0.10.1  jsonlite_1.5     tibble_1.4.1    
[11] gtable_0.2.0     lattice_0.20-35  pkgconfig_2.0.1  rlang_0.1.6      yaml_2.1.16     
[16] stringr_1.2.0    knitr_1.18       htmlwidgets_0.9  rprojroot_1.3-2  glue_1.2.0      
[21] R6_2.2.2         rmarkdown_1.8    RJSONIO_1.3-0    reshape2_1.4.3   magrittr_1.5    
[26] backports_1.1.2  htmltools_0.3.6  rsconnect_0.8.5  assertthat_0.2.0 mime_0.5        
[31] xtable_1.8-2     colorspace_1.3-2 httpuv_1.3.5     labeling_0.3     miniUI_0.1.1    
[36] stringi_1.1.6    RCurl_1.95-4.10  lazyeval_0.2.1   munsell_0.4.3    markdown_0.8  



