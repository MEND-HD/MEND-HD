# Data Cleaning

Take the participant’s “subjectdaystats” file and delete rows not part
of the participant’s wear days. Rename the file to XXXdaystats. Upload
this CSV file to R. Assign the cleaned data file to a new object called
“activity_clean”. Create a new column called “Day” that is 1-15. Change
the date format to “Month”,“Day”, “Year”. Make new columns for each
activity level ie. look for “sedentary, count” and find the digits
directly after that. Filter so only it only shows Days 2-15

# Data Transformation and Summary

Reshape data to long format and calculate average counts and percentages
for each activity level. For each activity level, take the average
counts and turn it into a percentage so it shows the percentage of time
the participant spent in each activity level

# Average Activity Distribution (Pie Chart)

Create a pie chart showing the activity intensity distribution

![](MA-PA_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> **Figure 1**
displaying the activity intensity distribution for the participant
across 14 days.

# Daily Steps Plot

Create separate plots for Week 1( Day 2-7) and Week 2( Day 8-15) and add
a shaded confidence interval
![](MA-PA_files/figure-gfm/unnamed-chunk-4-1.png)![](MA-PA_files/figure-gfm/unnamed-chunk-4-2.png)
**Figure 2** displays the daily steps for the participant across 14 days
