## R Markdown

WASO Graphs were cleaned in sheets due to small size of individual
datasets, wear days were filtered and if multiple sleep periods occured
on one day, WASO minutes were summed up for the day. The code below
generates a graph for multiple overlayed subjects:

``` r
install.packages("googledrive") #to install a .csv file directly from google drive, use the code from lines 9-19, code from line 21 uses downloaded data directly in R
install.packages(c("googledrive", "googlesheets4"))
library(googledrive)
library(googlesheets4)

drive_auth()

file <- drive_get("WASO 2+4+9+3+14+18 - Sheet1.csv")
tmp <- tempfile(fileext = ".csv")
drive_download(file, path = tmp, overwrite = TRUE)
waso_data <- read.csv(tmp)

waso_data <- read.csv("WASO 2+4+9+3+14 - Sheet1.csv")

library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
ggplot(waso_data, aes(x = DAY, y = WAKE_AFTER_SLEEP_ONSET_MINUTES, color = as.factor(SUBJECT))) +
  geom_point(alpha=0.3) + #use alpha to make dots translucent
    geom_smooth(method = "lm", se = FALSE, linewidth = 1, linetype = "solid", fill= "grey")+
  scale_x_continuous(
    limits = c(1, max(waso_data$DAY)),
    breaks = seq(1, max(waso_data$DAY)),
  ) +
  scale_color_brewer(palette = "Dark2")+
  labs(
    title= "Time in WASO Trends Over 2 Weeks By Subject",
    x= "Days",
    y= "WASO (minutes)",
    color= "Subject"
  )+
  theme_minimal() +
theme(
    axis.line = element_line(color = "grey40", linewidth = 0.8),  # bold axis lines
    axis.ticks = element_line(color = "grey40", linewidth = 0.8),   # bold tick marks
    theme(panel.grid.major.x = element_blank()),
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines"), 
    panel.border = element_rect(color = "grey40", fill = NA),
    text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, face= "bold"),
    legend.position = "right"
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](FDA_WASO_Revised_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
