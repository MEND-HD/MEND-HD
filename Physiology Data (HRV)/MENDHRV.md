# Data Cleaning

Load the participant file:  
*“heartrate_18_34b3ec0c-1105-4f14-8dc3-d1b18d5f23f3_heartratevariabilitydays11.csv”*

``` r
setwd(dir = "/Users/meghanbjalmeevans/Desktop")
data <- read.csv("/Users/meghanbjalmeevans/Desktop/HRVdays.csv")
```

Remove unnecessary variables from the table, including data splits for
‘day’ and ‘night’, and data reporting 70% HRV.

``` r
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
data<-select(data, -contains("DAY"))
data<-select(data, -contains("NIGHT"))
data<-select(data, -contains("70PCT"))
```

Remove extraneous columns

``` r
data<-select(data, -c(1:5))
data<-select(data, -c(15,16))
data<-select(data, -c(1, 3))
```

Filter data for only the two weeks of interest and assign the filtered
data to a newly named table

``` r
data$DATE <- as.Date(data$DATE)
data_filtered <- data[data$DATE >= as.Date("2025-08-06") & data$DATE <= as.Date("2025-08-19"), ]
data <- data_filtered
```

When adding new participants, repeat data cleaning.

``` r
data0004 <- read.csv("/Users/meghanbjalmeevans/Desktop/HRVdays0004.csv")
data0004<-select(data0004, -contains("DAY"))
data0004<-select(data0004, -contains("NIGHT"))
data0004<-select(data0004, -contains("70PCT"))
data0004<-select(data0004, -c(1:5))
data0004<-select(data0004, -c(15,16))
data0004<-select(data0004, -c(1, 3))
data0004$DATE <- as.Date(data0004$DATE, format = "%m/%d/%y")
data_filtered0004 <- data0004[data0004$DATE >= as.Date("2025-08-30") & data0004$DATE <= as.Date("2025-09-12"), ]
data0004 <- data_filtered0004
totaldata <- rbind(data,data0004)
```

# Data Transformation

Calculate LF/HF Ratio and add to the filtered data table

``` r
data$LF_POWER_90PCT_24H<-as.numeric(data$LF_POWER_90PCT_24H)
data$HF_POWER_90PCT_24H<-as.numeric(data$HF_POWER_90PCT_24H)
data$LFHFRATIO <- data$LF_POWER_90PCT_24H/ data$HF_POWER_90PCT_24H
data0004[sapply(data0004, is.character)] <- lapply(data0004[sapply(data0004, is.character)], as.numeric)
data0004$LFHFRATIO <- data0004$LF_POWER_90PCT_24H/data0004$HF_POWER_90PCT_24H
```

Join tables using bind

``` r
totaldata <- rbind(data,data0004)
```

# Data Visualization

Group the data table by subject ID and arrange based on the date

``` r
totaldata <- totaldata %>%
  group_by(SUBJECT) %>%
  arrange(DATE) %>%
  mutate(Day = as.numeric(DATE - min(DATE)) + 1)
```

Graph Epochs to visualize Actigraph weartime

``` r
library(ggplot2)
totaldata$EPOCH_COUNT_90PCT_24H <- as.numeric(totaldata$EPOCH_COUNT_90PCT_24H)
ggplot(totaldata, aes(
  x = Day,
  y = EPOCH_COUNT_90PCT_24H,
  color = factor(SUBJECT),     
  group = SUBJECT
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 600, color = "black", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 1440, color = "black", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 4, y = 600, label = "True wear for 10 hours", vjust = -1, color = "black", size = 4, fontface = "italic") +
  annotate("text", x = 4, y = 1440, label = "True wear for 24 hours", vjust = -1, color = "black", size = 4, fontface = "italic") +
  scale_x_continuous(breaks = 1:14) +
  scale_y_continuous(
    name = "Epoch Count",
    limits = c(0,2000),
    breaks = seq(0,2000, by = 200)
  )+
  labs(
    title = "Epoch count across First 14 Days",
    x = "Day",
    y = "Epoch Count",
    color = "Subject"          
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold")
  )
```

![](MENDHRV_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> Graph LF/HF
ratio to visualize sympathetic/parasympathetic activation

``` r
ggplot(totaldata, aes(
  x = Day,
  y = LFHFRATIO,
  color = factor(SUBJECT),     
  group = SUBJECT
)) +
  geom_line(linewidth = 1, linetype = "dashed") +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, linetype = "solid") +
  scale_x_continuous(breaks = 1:14) +
  labs(
    title = "LF/HF Ratio across First 14 Days",
    x = "Day",
    y = "LF/HF Ratio",
    color = "Subject"          
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold")
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](MENDHRV_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
