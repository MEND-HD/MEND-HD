## R Markdown

Data was cleaned in sheets prior to upload, data was cleans by filtering
for wear days and isolating columns of interest, “Dates” Column was
renamed to “DAY” and numbered 1-15 for the 15 days. A new sheet was made
combining data for subjects 2 and 4.

``` r
# Load packages

install.packages("tidyverse")
```

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.5'
    ## (as 'lib' is unspecified)

``` r
library(tidyverse) 
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.2.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)

# Naming the file

remdata <- read.csv("0002+0004_REM+NREM - Sheet1.csv")

# Filter for subjects 2 and 4

remdata_filtered <- remdata |> 
  filter(SUBJECT %in% c(2, 4))

# summarize means for Total, NREM, and REM

sleep_means <- remdata_filtered |> 
  group_by(SUBJECT) |> 
  summarise(
mean_total = mean(TOTAL_SLEEP_TIME_MINUTES, na.rm = TRUE), 
sd_total = sd(TOTAL_SLEEP_TIME_MINUTES, na.rm = TRUE),

mean_nrem  = mean(NREM_SLEEP_TIME_MINUTES, na.rm = TRUE),
sd_nrem    = sd(NREM_SLEEP_TIME_MINUTES, na.rm = TRUE),

mean_rem   = mean(REM_SLEEP_TIME_MINUTES, na.rm = TRUE),
sd_rem     = sd(REM_SLEEP_TIME_MINUTES, na.rm = TRUE)

)|> 
  pivot_longer( 
    cols = c(mean_total, mean_nrem, mean_rem, sd_total,
            sd_nrem, sd_rem), 
names_to = c(".value", "Sleep_Type"), 
names_pattern = "(mean|sd)_(.*)" ) |> 
  mutate( Sleep_Type = factor(Sleep_Type, 
                              levels = c("total", "nrem", "rem"), 
                              labels = c("Total Sleep", "NREM", "REM")) )

#making a bar graph to compare the averages 
ggplot(sleep_means, aes(x = Sleep_Type, y = mean, fill = factor(SUBJECT))) + 
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                position = position_dodge(width = 0.9), width = 0.2 )+ 
  labs( title = "Comparisons of Mean Sleep Time between Total, REM, and NREM Sleep Across 15 Days for Subjects 2 and 4", 
  x = "Sleep Type", 
  y = "Mean Minutes", 
  fill = "Subject" ) + 
  theme_minimal() + scale_x_discrete(labels= 
  c( mean_total = "Total Sleep", mean_nrem = "NREM", mean_rem = "REM" ))
```

![](Sleep_Means_2,4_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
#Plots the sleep time based on sleep type
```
