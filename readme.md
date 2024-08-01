# Exploratory data analysis web app

Users choose from different datasets, and choose different variables in each
dataset, and choose a method of visualization of that variable, and choose
different summary statistics. Optionally, the chosen variables can be grouped
by categorical variables.


## Inferential analysis

We can use these datasets to demonstrate inferential analyses.
For example, here is how to test for gender differences in narcissism scores.

```r
# read in data using read.csv b/c no package dependencies
narcissim_df <- read.csv("data/npi.csv")
```

### Independent samples t-test

```r
t.test(narcissism ~ gender, data = narcissim_df)

## 
##  Welch Two Sample t-test
## 
## data:  narcissism by gender
## t = -12.188, df = 9879.7, p-value < 2.2e-16
## alternative hypothesis: true difference in means between group female and group male is not equal to 0
## 95 percent confidence interval:
##  -2.335935 -1.688643
## sample estimates:
## mean in group female   mean in group male 
##             12.31145             14.32374
```

### Mann-Whitney test

```r
# yes, it is Mann-Whitney, despite name
wilcox.test(narcissism ~ gender, data = narcissim_df)

## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  narcissism by gender
## W = 11461348, p-value < 2.2e-16
## alternative hypothesis: true location shift is not equal to 0
```