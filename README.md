# Hybridisation as a potential extinction threat to an endangered Australian frog

This repository contains data and code to reproduce the analyses and figures in Liu, G & Rowley, JJL 2025, ‘Hybridisation as a potential extinction threat to an endangered Australian frog’, Ecology and Evolution. 

The following R information was used at the time of analysis:

sessionInfo()

R version 4.1.2 (2021-11-01)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 26100)

Matrix products: default

locale:
[1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252   
[3] LC_MONETARY=English_Australia.1252 LC_NUMERIC=C                      
[5] LC_TIME=English_Australia.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggsn_0.5.0      cowplot_1.1.1   gridExtra_2.3   ggmap_4.0.0     sf_1.0-12       lubridate_1.8.0
 [7] forcats_1.0.0   stringr_1.5.0   dplyr_1.1.2     purrr_1.0.1     readr_2.1.2     tidyr_1.3.0    
[13] tibble_3.2.1    tidyverse_2.0.0 dartR_1.9.9.1   ggplot2_3.5.2   adegenet_2.1.10 ade4_1.7-22    

loaded via a namespace (and not attached):
  [1] colorspace_2.1-0    seqinr_4.2-8        ellipsis_0.3.2      class_7.3-19       
  [5] rgdal_1.5-28        StAMPP_1.6.3        fs_1.6.3            rstudioapi_0.13    
  [9] proxy_0.4-27        remotes_2.4.2       fansi_1.0.6         mvtnorm_1.1-3      
 [13] codetools_0.2-18    splines_4.1.2       R.methodsS3_1.8.2   doParallel_1.0.17  
 [17] robustbase_0.93-9   cachem_1.0.6        knitr_1.45          pegas_1.1          
 [21] pkgload_1.3.2.1     cluster_2.1.2       png_0.1-8           R.oo_1.24.0        
 [25] shiny_1.8.0         httr_1.4.2          compiler_4.1.2      Matrix_1.5-1       
 [29] fastmap_1.1.1       cli_3.4.1           later_1.3.0         htmltools_0.5.7    
 [33] prettyunits_1.2.0   tools_4.1.2         igraph_1.2.11       gtable_0.3.3       
 [37] glue_1.7.0          reshape2_1.4.4      PopGenReport_3.0.4  Rcpp_1.0.14        
 [41] raster_3.5-15       vctrs_0.6.3         gdata_2.18.0        ape_5.7-1          
 [45] nlme_3.1-153        iterators_1.0.14    genetics_1.3.8.1.3  xfun_0.50          
 [49] ps_1.6.0            mime_0.12           miniUI_0.1.1.1      lifecycle_1.0.4    
 [53] gtools_3.9.5        devtools_2.4.5      terra_1.5-21        SNPRelate_1.28.0   
 [57] DEoptimR_1.1-3      MASS_7.3-54         scales_1.3.0        hms_1.1.3          
 [61] promises_1.2.0.1    mmod_1.3.3          gdsfmt_1.30.0       parallel_4.1.2     
 [65] RColorBrewer_1.1-3  memoise_2.0.1       reshape_0.8.9       calibrate_1.7.7    
 [69] stringi_1.7.12      maptools_1.1-4      gap_1.2.3-1         foreach_1.5.2      
 [73] e1071_1.7-14        permute_0.9-7       pkgbuild_1.3.1      bitops_1.0-7       
 [77] RgoogleMaps_1.4.5.3 rlang_1.1.1         pkgconfig_2.0.3     lattice_0.20-45    
 [81] htmlwidgets_1.6.4   hierfstat_0.5-11    tidyselect_1.2.0    processx_3.8.2     
 [85] GGally_2.1.2        plyr_1.8.6          magrittr_2.0.3      R6_2.5.1           
 [89] profvis_0.3.7       generics_0.1.3      combinat_0.0-8      DBI_1.2.2          
 [93] foreign_0.8-81      pillar_1.9.0        withr_3.0.2         mgcv_1.8-38        
 [97] units_0.8-0         sp_2.1-3            crayon_1.5.2        KernSmooth_2.23-20 
[101] utf8_1.2.4          tzdb_0.2.0          urlchecker_1.0.1    jpeg_0.1-10        
[105] usethis_2.1.5       data.table_1.15.2   callr_3.7.3         vegan_2.6-4        
[109] dismo_1.3-5         digest_0.6.35       classInt_0.4-3      xtable_1.8-4       
[113] gdistance_1.3-6     httpuv_1.6.5        R.utils_2.11.0      munsell_0.5.0      
[117] sessioninfo_1.2.2  
