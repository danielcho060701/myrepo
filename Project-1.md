Violent crime vs Cancer in the 50 States of America
================
Daniel Cho
3/18/2021

``` r
#set up libraries
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
library(ggplot2)
library(tidyr)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ tibble  3.0.5     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0
    ## ✓ purrr   0.3.4

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter()          masks stats::filter()
    ## x kableExtra::group_rows() masks dplyr::group_rows()
    ## x dplyr::lag()             masks stats::lag()

``` r
library(grid)
library(cluster)
```

Introduction: The main goal of my research project is to compare and
look for relationships between murder rate, assault rate, rape rate,
cancer death rate, and whether the state is urban or rural for all 50
states of America. Both data sets were obtained off of kaggle.com. The
first data set was called “Violent crime rates by US state” and this
dataset showed the murder rate, assault rate, rape rate, and urban
population percentage for all 50 states. The second data set was called
“Cancer rates by US states” and showed the number of cancer deaths per
100,000 for all 50 states, as well as the range of this number. The
first data set’s information was sourced by the World Almanac while the
second data set’s information was sourced by the Centers for Disease
Control and Prevention. I thought that studying these two data sets
would be interesting because I wanted to know if there was an
association between the different types of violent crime. Additionally,
although cancer is not directly related to crime, I wanted to know if
there was a non-obvious relationship. I believe there will be a
positively linear relationship between the 3 types of violent crime. I
do not think there will be a correlation between violent crime and
cancer. I think that violent crime, as well as cancer deaths per 100,000
will be more common in majority urban states compared to majority rural
states.

Tidy:

``` r
#Set csv files as working directory and made vectors for the data found online
Crimerate <- read.csv("./US_violent_crime.csv")
CancerDeath <- read.csv("uscs_map_death_all.csv")
```

Obtained both data sets from kaggle.com
<https://www.kaggle.com/mathchi/violent-crime-rates-by-us-state>
<https://www.kaggle.com/rishidamarla/cancer-rates-by-us-state>

Both data sets were already tidy as every column is a variable and every
row is an observation. Because both data sets were already tidy, even
after joining the data, it remained tidy.

``` r
#Added extra csv file which contains state names and abbreviations
Statecode <- read.csv("csvData.csv")
#Added State abbreviations column and moved it to directly after state name
NewCrimeRate <- left_join(Crimerate,Statecode,by=c("X"="State")) %>%
  select(-Abbrev) %>%
  relocate(Code, .before=Murder)
```

Because one of the data sets used state abbreviations while the other
data set did not, I needed to add the abbreviations of each state so
that they could be joined together. I also moved the abbreviations to
directly after the state name for convenience.

``` r
#Removed unwanted cancer death per 100,000 range
# Select dplyr function
NewCancerDeath <- CancerDeath %>%
  select(-Range)
```

Since the violent crime data set gave exact per 100,000 statistics, the
range of cancer deaths per 100,000 did not fit in with the rest of the
data and as a result, a “select” dplyr function was used to drop the
unwanted variable.

Join/Merge

``` r
#Created merged data set
Merged <- inner_join(NewCrimeRate, NewCancerDeath, by=c("Code"="State")) %>%
  rename(CancerRate=Rate)
```

The two data sets were joined together with the variable of State Code.
Inner\_join was used to join the two tables of data together. The new
merged data remains tidy. There are 50 observations and 7 variables
which gives a total of 350 cells.

Create summary statistics

``` r
# Filter dplyr function
Merged %>% 
  filter(between(CancerRate,180,200))
```

    ##               X Code Murder Assault UrbanPop Rape CancerRate
    ## 1       Alabama   AL   13.2     236       58 21.2      182.1
    ## 2      Arkansas   AR    8.8     190       50 19.5      189.6
    ## 3      Kentucky   KY    9.7     109       52 16.3      199.3
    ## 4     Louisiana   LA   15.4     249       66 22.2      188.7
    ## 5   Mississippi   MS   16.1     259       44 17.1      196.5
    ## 6      Oklahoma   OK    6.6     151       68 20.0      185.4
    ## 7     Tennessee   TN   13.2     188       59 26.9      185.4
    ## 8 West Virginia   WV    5.7      81       39  9.3      190.5

Using the filter dplyr function, the states with cancer deaths from
180-200 per 100,000 people were isolated. This reveals the top 8 states
with the highest cancer death rates per 100,000.

``` r
# Arrange dplyr function
Merged %>% 
  arrange(desc(Murder))
```

    ##                 X Code Murder Assault UrbanPop Rape CancerRate
    ## 1         Georgia   GA   17.4     211       60 25.8      168.1
    ## 2     Mississippi   MS   16.1     259       44 17.1      196.5
    ## 3         Florida   FL   15.4     335       80 31.9      154.9
    ## 4       Louisiana   LA   15.4     249       66 22.2      188.7
    ## 5  South Carolina   SC   14.4     279       48 22.5      174.0
    ## 6         Alabama   AL   13.2     236       58 21.2      182.1
    ## 7       Tennessee   TN   13.2     188       59 26.9      185.4
    ## 8  North Carolina   NC   13.0     337       45 16.1      167.7
    ## 9           Texas   TX   12.7     201       80 25.5      156.9
    ## 10         Nevada   NV   12.2     252       81 46.0      164.9
    ## 11       Michigan   MI   12.1     255       74 35.1      170.2
    ## 12     New Mexico   NM   11.4     285       70 32.1      145.1
    ## 13       Maryland   MD   11.3     300       67 27.8      163.0
    ## 14       New York   NY   11.1     254       86 26.1      155.5
    ## 15       Illinois   IL   10.4     249       83 24.0      171.7
    ## 16         Alaska   AK   10.0     263       48 44.5      173.1
    ## 17       Kentucky   KY    9.7     109       52 16.3      199.3
    ## 18     California   CA    9.0     276       91 40.6      146.6
    ## 19       Missouri   MO    9.0     178       70 28.2      179.1
    ## 20       Arkansas   AR    8.8     190       50 19.5      189.6
    ## 21       Virginia   VA    8.5     156       63 20.7      162.3
    ## 22        Arizona   AZ    8.1     294       80 31.0      146.4
    ## 23       Colorado   CO    7.9     204       78 38.7      139.2
    ## 24     New Jersey   NJ    7.4     159       89 18.8      156.0
    ## 25           Ohio   OH    7.3     120       75 21.4      177.4
    ## 26        Indiana   IN    7.2     113       65 21.0      179.4
    ## 27        Wyoming   WY    6.8     161       60 15.6      147.7
    ## 28       Oklahoma   OK    6.6     151       68 20.0      185.4
    ## 29   Pennsylvania   PA    6.3     106       72 14.9      170.0
    ## 30         Kansas   KS    6.0     115       66 18.0      162.9
    ## 31        Montana   MT    6.0     109       53 16.4      154.0
    ## 32       Delaware   DE    5.9     238       72 15.8      167.1
    ## 33  West Virginia   WV    5.7      81       39  9.3      190.5
    ## 34         Hawaii   HI    5.3      46       83 20.2      134.9
    ## 35         Oregon   OR    4.9     159       67 29.3      163.2
    ## 36  Massachusetts   MA    4.4     149       85 16.3      159.7
    ## 37       Nebraska   NE    4.3     102       62 16.5      160.7
    ## 38     Washington   WA    4.0     145       73 26.2      156.3
    ## 39   South Dakota   SD    3.8      86       45 12.8      154.1
    ## 40   Rhode Island   RI    3.4     174       87  8.3      173.9
    ## 41    Connecticut   CT    3.3     110       77 11.1      147.8
    ## 42           Utah   UT    3.2     120       80 22.9      127.9
    ## 43      Minnesota   MN    2.7      72       66 14.9      155.1
    ## 44          Idaho   ID    2.6     120       54 14.2      156.3
    ## 45      Wisconsin   WI    2.6      53       66 10.8      164.6
    ## 46           Iowa   IA    2.2      56       57 11.3      168.2
    ## 47        Vermont   VT    2.2      48       32 11.2      164.1
    ## 48          Maine   ME    2.1      83       51  7.8      174.8
    ## 49  New Hampshire   NH    2.1      57       56  9.5      158.6
    ## 50   North Dakota   ND    0.8      45       44  7.3      150.8

The arrange dplyr function was used to order the 50 states by descending
murder rates per 100,000. This shows the states with the highest number
of murders per 100,000.

``` r
# mutate dplyr function
Merged2 <- Merged %>%
  mutate(Urban = case_when(
    UrbanPop >= 50 ~ "urban",
    UrbanPop < 50 ~ "rural"
  ))
```

The mutate dplyr function was used to create a new variable that is a
function of the urban percentage. If a state had 50 or higher percentage
of urban population, it was considered “urban” while if the percentage
was less than 50, it was considered “rural”

``` r
# Groupby dplyr function
Merged2 %>% 
  group_by(Urban) %>%
  summarize(mean=mean(CancerRate))
```

    ## # A tibble: 2 x 2
    ##   Urban  mean
    ## * <chr> <dbl>
    ## 1 rural  171.
    ## 2 urban  164.

The group\_by dplyr function was used to show the mean cancer death rate
per 100,000 between rural states and urban states. Surprisingly, urban
states had less cancer deaths per 100,000 compared to rural states,
which was the opposite of what I expected.

``` r
# Summary statistics for Murder rate per 100,000
Merged %>%
  summarize(Mean = mean(Murder),
            Median = median(Murder),
            SD = sd(Murder),
            Variation = var(Murder),
            Min. = min(Murder),
            Max. = max(Murder),
            Range = Max. - Min.) %>%
  kbl(caption = "Summary statistics for Murders per 100,000") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>
Summary statistics for Murders per 100,000
</caption>
<thead>
<tr>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Variation
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
Max.
</th>
<th style="text-align:right;">
Range
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
7.788
</td>
<td style="text-align:right;">
7.25
</td>
<td style="text-align:right;">
4.35551
</td>
<td style="text-align:right;">
18.97047
</td>
<td style="text-align:right;">
0.8
</td>
<td style="text-align:right;">
17.4
</td>
<td style="text-align:right;">
16.6
</td>
</tr>
</tbody>
</table>

The mean number of murders per 100,000 for the United States was 7.788
murders. The median was very similar at 7.25 murders. There was a fairly
large range with a state having only 0.8 murders per 100,000 compared to
a max of 17.4 murders per 100,000.

``` r
# Summary statistics for Assaults per 100,000
Merged %>%
  summarize(Mean = mean(Assault),
            Median = median(Assault),
            SD = sd(Assault),
            Variation = var(Assault),
            Min. = min(Assault),
            Max. = max(Assault),
            Range = Max. - Min.) %>%
  kbl(caption = "Summary statistics for Assault per 100,000") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>
Summary statistics for Assault per 100,000
</caption>
<thead>
<tr>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Variation
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
Max.
</th>
<th style="text-align:right;">
Range
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
170.76
</td>
<td style="text-align:right;">
159
</td>
<td style="text-align:right;">
83.33766
</td>
<td style="text-align:right;">
6945.166
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
337
</td>
<td style="text-align:right;">
292
</td>
</tr>
</tbody>
</table>

As expected, there were much more assaults per 100,000 in the United
States compared to murders. The mean number of assaults was 170.76 while
the median was 159. There was much more variation in the number of
assaults with the lowest state at only 45 assaults per 100,000 compared
to a max of 337 assaults.

``` r
# Summary statistics for Rape per 100,000
Merged %>%
  summarize(Mean = mean(Rape),
            Median = median(Rape),
            SD = sd(Rape),
            Variation = var(Rape),
            Min. = min(Rape),
            Max. = max(Rape),
            Range = Max. - Min.) %>%
  kbl(caption = "Summary statistics for Rape per 100,000") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>
Summary statistics for Rape per 100,000
</caption>
<thead>
<tr>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Variation
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
Max.
</th>
<th style="text-align:right;">
Range
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
21.232
</td>
<td style="text-align:right;">
20.1
</td>
<td style="text-align:right;">
9.366385
</td>
<td style="text-align:right;">
87.72916
</td>
<td style="text-align:right;">
7.3
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
38.7
</td>
</tr>
</tbody>
</table>

Surprisingly, there was a significantly lower rate of rapes compared to
assaults, although there were still more rapes than murders. In the
United States, the mean number of rapes per 100,000 was 21.232 while the
median was 20.1. The variation was somewhere in the middle of number of
murders and assaults with the minimum being 7.3 and the maximum being 46
rapes per 100,000.

``` r
# Summary Statistics for Cancer deaths per 100,000
Merged %>%
  summarize(Mean = mean(CancerRate),
            Median = median(CancerRate),
            SD = sd(CancerRate),
            Variation = var(CancerRate),
            Min. = min(CancerRate),
            Max. = max(CancerRate),
            Range = Max. - Min.) %>%
  kbl(caption = "Summary statistics for CancerRate per 100,000") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>
Summary statistics for CancerRate per 100,000
</caption>
<thead>
<tr>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Variation
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
Max.
</th>
<th style="text-align:right;">
Range
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
164.834
</td>
<td style="text-align:right;">
163.65
</td>
<td style="text-align:right;">
15.571
</td>
<td style="text-align:right;">
242.4562
</td>
<td style="text-align:right;">
127.9
</td>
<td style="text-align:right;">
199.3
</td>
<td style="text-align:right;">
71.4
</td>
</tr>
</tbody>
</table>

I was unsure on how frequent cancer deaths per 100,000 would be compared
to the 3 violent crimes. After running my summary statistics, you can
see that there is almost as much cancer deaths per 100,000 as there are
assaults which is pretty surprising. The mean number of cancer deaths in
the US per 100,000 was 164.834 while the median was 163.65. However,
cancer deaths seems to have a significantly less amount of variation
compared to assaults. The minimum was 127.9 deaths per 100,000 and the
maximum was 199.3 deaths per 100,000.

``` r
#Summary statistics for Urban Population %
Merged %>%
  summarize(Mean = mean(UrbanPop),
            Median = median(UrbanPop),
            SD = sd(UrbanPop),
            Variation = var(UrbanPop),
            Min. = min(UrbanPop),
            Max. = max(UrbanPop),
            Range = Max. - Min.) %>%
  kbl(caption = "Summary statistics for UrbanPop per 100,000") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>
Summary statistics for UrbanPop per 100,000
</caption>
<thead>
<tr>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Variation
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
Max.
</th>
<th style="text-align:right;">
Range
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
65.54
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
14.47476
</td>
<td style="text-align:right;">
209.5188
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
59
</td>
</tr>
</tbody>
</table>

As a rapidly urbanizing country, it was no surprise that the United
States averaged around 65.54% urban with a median of 66%. There was a
large range between the state with the least urban percentage with only
32% of their land being considered urban, while the max was 91% urban.
This is understandable as a state like Montana will have significantly
less of their land considered urban compared to a state like New York.

``` r
#Summary statistics for Urban vs Rural Murder per 100,000
Merged2 %>%
  group_by(Urban) %>%
  summarize(n = n(),
            Mean = mean(Murder),
            Median = median(Murder),
            SD = sd(Murder),
            Variation = var(Murder),
            Min. = min(Murder),
            Max. = max(Murder),
            Range = Max. - Min.) %>%
  kbl(caption = "Summary statistics for Murder per 100,000 depending on Rural vs Urban") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>
Summary statistics for Murder per 100,000 depending on Rural vs Urban
</caption>
<thead>
<tr>
<th style="text-align:left;">
Urban
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Variation
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
Max.
</th>
<th style="text-align:right;">
Range
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
rural
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
8.25
</td>
<td style="text-align:right;">
7.85
</td>
<td style="text-align:right;">
5.897699
</td>
<td style="text-align:right;">
34.78286
</td>
<td style="text-align:right;">
0.8
</td>
<td style="text-align:right;">
16.1
</td>
<td style="text-align:right;">
15.3
</td>
</tr>
<tr>
<td style="text-align:left;">
urban
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
7.70
</td>
<td style="text-align:right;">
7.25
</td>
<td style="text-align:right;">
4.084593
</td>
<td style="text-align:right;">
16.68390
</td>
<td style="text-align:right;">
2.1
</td>
<td style="text-align:right;">
17.4
</td>
<td style="text-align:right;">
15.3
</td>
</tr>
</tbody>
</table>

After splitting up the 50 states into majority rural or majority urban,
the murder rates were compared between the two groups. Very
surprisingly, there was a higher mean of murders per 100,000 in the 8
rural states compared to the 42 urban states. However this may be caused
by the significantly lower amount of states considered “rural” as both
rural and urban states had the same range of murders per 100,000.

``` r
#Summary statistics for Urban vs Rural Assault per 100,000
Merged2 %>%
  group_by(Urban) %>%
  summarize(n = n(),
            Mean = mean(Assault),
            Median = median(Assault),
            SD = sd(Assault),
            Variation = var(Assault),
            Min. = min(Assault),
            Max. = max(Assault),
            Range = Max. - Min.) %>%
  kbl(caption = "Summary statistics for Assault per 100,000 depending on Rural vs Urban") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>
Summary statistics for Assault per 100,000 depending on Rural vs Urban
</caption>
<thead>
<tr>
<th style="text-align:left;">
Urban
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Variation
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
Max.
</th>
<th style="text-align:right;">
Range
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
rural
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
174.75
</td>
<td style="text-align:right;">
172.5
</td>
<td style="text-align:right;">
120.5022
</td>
<td style="text-align:right;">
14520.786
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
337
</td>
<td style="text-align:right;">
292
</td>
</tr>
<tr>
<td style="text-align:left;">
urban
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
170.00
</td>
<td style="text-align:right;">
159.0
</td>
<td style="text-align:right;">
76.2723
</td>
<td style="text-align:right;">
5817.463
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
335
</td>
<td style="text-align:right;">
289
</td>
</tr>
</tbody>
</table>

Once again, rural states surprisingly had a higher rate of assault per
100,000. Because the range of assaults are similar, it appears that
there may be a “rural” state that has considerable higher amounts of
violent crime which skews the data of the rural states.

``` r
#Summary statistics for Urban vs Rural Rape per 100,000
Merged2 %>%
  group_by(Urban) %>%
  summarize(n = n(),
            Mean = mean(Rape),
            Median = median(Rape),
            SD = sd(Rape),
            Variation = var(Rape),
            Min. = min(Rape),
            Max. = max(Rape),
            Range = Max. - Min.) %>%
  kbl(caption = "Summary statistics for Rape per 100,000 depending on Rural vs Urban") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>
Summary statistics for Rape per 100,000 depending on Rural vs Urban
</caption>
<thead>
<tr>
<th style="text-align:left;">
Urban
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Variation
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
Max.
</th>
<th style="text-align:right;">
Range
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
rural
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
17.60000
</td>
<td style="text-align:right;">
14.45
</td>
<td style="text-align:right;">
11.886968
</td>
<td style="text-align:right;">
141.30000
</td>
<td style="text-align:right;">
7.3
</td>
<td style="text-align:right;">
44.5
</td>
<td style="text-align:right;">
37.2
</td>
</tr>
<tr>
<td style="text-align:left;">
urban
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
21.92381
</td>
<td style="text-align:right;">
20.85
</td>
<td style="text-align:right;">
8.812403
</td>
<td style="text-align:right;">
77.65844
</td>
<td style="text-align:right;">
7.8
</td>
<td style="text-align:right;">
46.0
</td>
<td style="text-align:right;">
38.2
</td>
</tr>
</tbody>
</table>

Interestingly, the rate of rape per 100,000 is the only violent crime
with a higher mean in urban states which is what I originally would be
the case for all the types of violent crime. Once again the range of
rape per 100,000 appears to be similar for both urban and rural states.

``` r
#Summary statistics for Urban vs Rural Cancer Deaths per 100,000
Merged2 %>%
  group_by(Urban) %>%
  summarize(n = n(),
            Mean = mean(CancerRate),
            Median = median(CancerRate),
            SD = sd(CancerRate),
            Variation = var(CancerRate),
            Min. = min(CancerRate),
            Max. = max(CancerRate),
            Range = Max. - Min.) %>%
  kbl(caption = "Summary statistics for Cancer Deaths per 100,000 depending on Rural vs Urban") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>
Summary statistics for Cancer Deaths per 100,000 depending on Rural vs
Urban
</caption>
<thead>
<tr>
<th style="text-align:left;">
Urban
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Variation
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
Max.
</th>
<th style="text-align:right;">
Range
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
rural
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
171.3500
</td>
<td style="text-align:right;">
170.40
</td>
<td style="text-align:right;">
16.01374
</td>
<td style="text-align:right;">
256.4400
</td>
<td style="text-align:right;">
150.8
</td>
<td style="text-align:right;">
196.5
</td>
<td style="text-align:right;">
45.7
</td>
</tr>
<tr>
<td style="text-align:left;">
urban
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
163.5929
</td>
<td style="text-align:right;">
162.95
</td>
<td style="text-align:right;">
15.36619
</td>
<td style="text-align:right;">
236.1197
</td>
<td style="text-align:right;">
127.9
</td>
<td style="text-align:right;">
199.3
</td>
<td style="text-align:right;">
71.4
</td>
</tr>
</tbody>
</table>

Finally, the mean number of cancer related deaths per 100,000 was
studied between urban and rural dominant states. In this statistic as
well, rural states had a higher mean of cancer deaths per 100,000. This
was the first statistic in which the range between rural and urban
significantly differed. The urban states had a significantly lower
minimum and this may be cause by a state that is medically advanced and
hospital accessibility is much higher than the other states.

Visualizations

``` r
# Build a correlation maxtrix between all numeric variables
Merged2_num <- Merged2 %>%
  select_if(is.numeric)
cor(Merged2_num, use = "pairwise.complete.obs")
```

    ##                Murder   Assault    UrbanPop       Rape CancerRate
    ## Murder     1.00000000 0.8018733  0.06957262  0.5635788  0.3480480
    ## Assault    0.80187331 1.0000000  0.25887170  0.6652412  0.0853317
    ## UrbanPop   0.06957262 0.2588717  1.00000000  0.4113412 -0.4214453
    ## Rape       0.56357883 0.6652412  0.41134124  1.0000000 -0.1485294
    ## CancerRate 0.34804801 0.0853317 -0.42144530 -0.1485294  1.0000000

``` r
# Make a heatmap
cor(Merged2_num, use = "pairwise.complete.obs") %>%
  # Save as a data frame
  as.data.frame %>%
  # Convert row names to an explicit variable
  rownames_to_column %>%
  # Pivot so that all correlations appear in the same column
  pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>%
  ggplot(aes(rowname, other_var, fill=correlation)) +
  # Heatmap with geom_tile
  geom_tile() +
  # Change the scale to make the middle appear neutral
  scale_fill_gradient2(low="red",mid="white",high="blue") +
  # Overlay values
  geom_text(aes(label = round(correlation,2)), color = "black", size = 4) +
  # Give title and labels
  labs(title = "Correlation matrix for the Merged Datasets", x = "variable 1", y = "variable 2")
```

![](Project-1-FINAL_files/figure-gfm/unnamed-chunk-19-1.png)<!-- --> A
correlation heatmap was creates to show the relationship between the
numeric variables. Not surprisingly, the two variables with the greatest
relationship was murder per 100,000 and assault per 100,000.
Surprisingly, the variables with the smallest correlation was murder per
100,000 and urban population percentage. I thought that murders were
more prevalent in urban locations because of the increased population
density, but there was almost zero correlation between the two. Finally,
I thought that it was very interesting that cancer deaths per 100,000
decreased as the percentage of urban population increased. This confirms
that there may be a certain trend that urban populations have better
access to hospitals and medical professionals that can help prevent
cancer death.

``` r
#Illustrating the relationship between murder and urban density
ggplot(Merged2, aes(Color=X, y=Murder, x=X, fill=Urban)) +
  geom_bar(position="dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Murder Rates per 100,000") + 
  theme(plot.title = element_text(hjust = 0.5))
```

![](Project-1-FINAL_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->
This grouped bar plot was created to visual the relationship between
murder rates per 100,000 and whether the state was urban or rural. There
is not a visible and obvious relationship between murder and urban
population although the state with by far the least amount of murders
per 100,000 is majority rural (North Dakota).

``` r
#Illustrating the relationship between Cancer deaths per 100,000 and urban density
ggplot(Merged2, aes(Color=X, y=CancerRate, x=X, fill=Urban)) +
  geom_bar(position="dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cancer deaths per 100,000") + 
  theme(plot.title = element_text(hjust = 0.5))
```

![](Project-1-FINAL_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->
This grouped bar plot was created to visualize the relationship between
number of cancer deaths per 100,000 and whether a state is majority
rural or urban. Again, there is no clear and obvious difference or
relationship between cancer death rates and whether a state is rural or
urban. However, it can be seen that as a whole, the 8 rural states have
fairly high cancer death rates.

Dimensionality Reduction

``` r
# Use the function pam to find 3 clusters
library(cluster)
pam1 <- Merged2 %>%
  pam(k=3)
pam1
```

    ## Medoids:
    ##      ID  X Code Murder Assault UrbanPop Rape CancerRate Urban
    ## [1,] 22 22   22   12.1     255       74 35.1      170.2     2
    ## [2,] 37 37   37    4.9     159       67 29.3      163.2     2
    ## [3,] 27 27   29    4.3     102       62 16.5      160.7     2
    ## Clustering vector:
    ##  [1] 1 1 1 2 1 1 3 1 1 1 3 3 1 3 3 3 3 1 3 1 2 1 3 1 2 3 3 1 3 2 1 1 1 3 3 2 2 3
    ## [39] 2 1 3 2 2 3 3 2 2 3 3 2
    ## Objective function:
    ##    build     swap 
    ## 38.25956 38.25956 
    ## 
    ## Available components:
    ##  [1] "medoids"    "id.med"     "clustering" "objective"  "isolation" 
    ##  [6] "clusinfo"   "silinfo"    "diss"       "call"       "data"

``` r
# Save cluster assignment as a column in your dataset
pamclust <- Merged2 %>%
  mutate(cluster = as.factor(pam1$clustering))
# Make a plot of data colored by final cluster assignment
pamclust %>% 
  ggplot(aes(Murder, Assault, Rape, CancerRate, UrbanPop, color = cluster)) +
  geom_point()
```

    ## Warning: Duplicated aesthetics after name standardisation:

![](Project-1-FINAL_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
# Calculate the means of each variable for each cluster
pamclust %>% 
  group_by(cluster) %>%
  summarize_if(is.numeric,~ mean(.x, na.rm=T))
```

    ## # A tibble: 3 x 6
    ##   cluster Murder Assault UrbanPop  Rape CancerRate
    ## * <fct>    <dbl>   <dbl>    <dbl> <dbl>      <dbl>
    ## 1 1        11.9    265.      68.4  28.8       165.
    ## 2 2         7.48   168.      70.9  21.3       168.
    ## 3 3         4.27    87.6     59.8  14.4       163.

``` r
# Find the observations that are the final medoids
Merged2[pam1$id.med,]
```

    ##           X Code Murder Assault UrbanPop Rape CancerRate Urban
    ## 22 Michigan   MI   12.1     255       74 35.1      170.2 urban
    ## 37   Oregon   OR    4.9     159       67 29.3      163.2 urban
    ## 27 Nebraska   NE    4.3     102       62 16.5      160.7 urban

After running PAM clustering, there were 3 distinct clusters when
comparing every numerical variable in our dataset. The first cluster had
a mean urban population of 68.39%, but had the highest rates of murder,
assault, and rape. This cluster had the second highest cancer rate. The
second cluster had the highest urban population of 70.92%. This cluster
had the 2nd highest rate of murder, assault and rape per 100,000.
Interestingly, the cancer death rate per 100,000 was the highest in this
cluster, although previous data showed a negative relationship between
cancer death rates and urban population percentage. The final cluster
had the lowest rate of murder, assault, rape, and cancer deaths per
100,000. This cluster also had the lowest percentage of urban population
at 59.75%. The medoids for the 3 clusters were Michigan, Oregon, and
Nebraska respectively. The PAM clustering analysis supports my initial
hypothesis that violent crime and cancer deaths were lower in rural
states compared to urban states. I believe that this hypothesis was not
supported previously because of the low sample size of “rural” states
when classifying states by a majority urban or majority rural
classification. In the next installment of this project, I think that
figuring out the median urban percentage and classifying states with
percentages greater than that as urban is a better method than using a
flat 50% as the determining factor.
