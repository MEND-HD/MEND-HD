---
title: "Split Dataset by Subject"
author: "Meghan Evans"
output:
  md_document:
    variant: gfm
    preserve_yaml: true
---

# Overview

This document imports a dataset, splits it by `SUBJECT`,  
and exports each subject as an individual CSV file.

------------------------------------------------------------------------

## Setup

``` r
# Set working directory
setwd("/Users/meghanbjalmeevans/Desktop/R-MEND/Datasets")

# Required base package
library(tools)
```

------------------------------------------------------------------------

## Import Dataset

``` r
# Read dataset from project directory
HRV <- read.csv("HRVdays 02-26.csv", stringsAsFactors = FALSE)

# Define dataset name manually
dataset_name <- "HRVdays"

cat("Dataset loaded:", dataset_name)
```

    ## Dataset loaded: HRVdays

------------------------------------------------------------------------

## Validate SUBJECT Column

``` r
if(!"SUBJECT" %in% names(HRV)) {
  stop("Column 'SUBJECT' not found in dataset.")
}
```

------------------------------------------------------------------------

## Remove Missing SUBJECT Values

``` r
HRV <- HRV[!is.na(HRV$SUBJECT), ]
```

------------------------------------------------------------------------

## Split Dataset by SUBJECT

``` r
split_list <- split(HRV, HRV$SUBJECT)

cat("Number of subjects detected:", length(split_list))
```

    ## Number of subjects detected: 12

------------------------------------------------------------------------

## Export Individual Subject Files

``` r
for(subject_id in names(split_list)) {
  
  # Clean subject ID for safe filenames
  clean_id <- gsub("[^A-Za-z0-9_]", "_", subject_id)
  
  file_name <- paste0(dataset_name,
                      "-subjectID_",
                      clean_id,
                      ".csv")
  
  write.csv(split_list[[subject_id]],
            file = file.path(getwd(), file_name),
            row.names = FALSE)
}

cat("Export complete:", length(split_list), "subject files created.")
```

    ## Export complete: 12 subject files created.

------------------------------------------------------------------------

# Output

Each subject file will be saved in the working directory as:

    [datasetname]-subjectID_[ID].csv

Example:

    HRVdays-subjectID_2.csv
    HRVdays-subjectID_4.csv
    HRVdays-subjectID_101.csv
