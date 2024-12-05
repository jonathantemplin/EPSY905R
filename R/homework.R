#' A Function to check homework of ESRM 64504
#'
#' This function allows you to check out the progress of homework
#' @keywords homework
#' @export
#' @examples
#' homework()
homework <- function(){
message("
There will be four homeworks in total for ESRM 64503.

Homework 0's answer:
Q2: In Rstudio, load the tidyverse package and copy-paste the console output of sessioninfo()
A2:
> sessionInfo()
R version 4.3.3 (2024-02-29)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Sonoma 14.5

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Chicago
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
[1] cmdstanr_0.7.1     rstan_2.32.6       StanHeaders_2.32.9

loaded via a namespace (and not attached):
 [1] tensorA_0.36.2.1     utf8_1.2.4           generics_0.1.3       xml2_1.3.6           stringi_1.8.4
 [6] digest_0.6.37        magrittr_2.0.3       evaluate_0.24.0      grid_4.3.3           fastmap_1.2.0
[11] jsonlite_1.8.8       processx_3.8.4       pkgbuild_1.4.4       backports_1.5.0      ps_1.7.7
[16] gridExtra_2.3        fansi_1.0.6          viridisLite_0.4.2    QuickJSR_1.3.1       scales_1.3.0
[21] codetools_0.2-19     abind_1.4-5          cli_3.6.3            rlang_1.1.4          ESRM64503_2024.08.20
[26] munsell_0.5.1        tools_4.3.3          inline_0.3.19        parallel_4.3.3       checkmate_2.3.2
[31] dplyr_1.1.4          colorspace_2.1-1     ggplot2_3.5.1        kableExtra_1.4.0     curl_5.2.1
[36] vctrs_0.6.5          posterior_1.5.0      R6_2.5.1             matrixStats_1.3.0    stats4_4.3.3
[41] lifecycle_1.0.4      stringr_1.5.1        V8_5.0.0             pkgconfig_2.0.3      RcppParallel_5.1.9
[46] pillar_1.9.0         gtable_0.3.5         loo_2.7.0            glue_1.7.0           Rcpp_1.0.13
[51] systemfonts_1.1.0    xfun_0.47            tibble_3.2.1         tidyselect_1.2.1     rstudioapi_0.16.0
[56] knitr_1.48           htmltools_0.5.8.1    svglite_2.1.3        rmarkdown_2.28       compiler_4.3.3
[61] distributional_0.4.0

Q3: (Multiple-choice question) Assume you've already loaded the tidyverse and one dataset called flights in your R session, which R function below can get you know the number of rows AND columns of flights?
Note: you can load flights using the R code 'install.packages('nycflights13'); library(nycflights13)'
A3: Check all options.


You can work on HW1 now, please click the following link to have access:
https://jihongzhang.org/posts/Lectures/2024-07-21-applied-multivariate-statistics-esrm64503/HWs/HW_1.html



        ")


}

#' @export
show_table <- function(data, ...){
  kable(data) |>
    kable_styling(bootstrap_options = c("striped", "hover"), ...)
}

