Violent crime vs Cancer in the 50 States of America
================
Daniel Cho
4/18/21

``` r
# Set up libraries
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
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(sandwich)
library(plotROC)
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

Because I am using the same data set as project 1, I used the data set
that I had previously tidy’d, removed extra columns, deleted data sets
with N/A data, and manipulated using dplyr functions.

``` r
# Import dataset from previous project which has already been tidy'd and manipulated with dplyr functions for convenience
DataSet <- read.csv("Project2Dataset.csv")
```

EDA

``` r
# Summary statistics for Murder rate per 100,000
DataSet %>%
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
DataSet %>%
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
# Summary Statistics for Cancer deaths per 100,000
DataSet %>%
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
DataSet %>%
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
# Scatterplot of Murder vs Assault per 100,000 in the US
ggplot(DataSet, aes(x=Murder, y=Assault)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method = lm)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Project-2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> When
looking at the scatterplot of Assault per 100,000 in the US and Murder
per 100,000 in the US, there seems to be a positively linear
relationship between the two types of violent crime. This relationship
is something that I anticipated in seeing before starting this project
and a relationship that I want to look into deeper with the following
tests.

``` r
# Scatterplot of Cancer deaths per 100,000 and urban %
ggplot(DataSet, aes(x=UrbanPop, y=CancerRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method = lm)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Project-2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- --> When
looking at the relationship between cancer deaths per 100,000 and the
percentage of population considered “urban” it is fairly reasonable to
say that there seems to be a negatively linear relationship between the
two. This relationship is very interesting to me because I would think
that states that have a higher % of urban land would have a higher
population density, would would lead to more cases of cancer as well as
more cancer related deaths. Maybe the reason that states with less urban
population have more cancer related deaths is because these states are
typically less modern and the health care system is less developed than
urbanized states. For example, someone who gets cancer in rural Montana
may have less treatment options, as well as preventative knowledge as
someone who gets cancer in New York City.

``` r
# mutate dplyr function
DataSet2 <- DataSet %>%
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
# Build a correlation maxtrix between all numeric variables
DataSet2_Num <- DataSet2 %>%
  select_if(is.numeric)
cor(DataSet2_Num, use = "pairwise.complete.obs")
```

    ##                Murder   Assault    UrbanPop       Rape CancerRate
    ## Murder     1.00000000 0.8018733  0.06957262  0.5635788  0.3480480
    ## Assault    0.80187331 1.0000000  0.25887170  0.6652412  0.0853317
    ## UrbanPop   0.06957262 0.2588717  1.00000000  0.4113412 -0.4214453
    ## Rape       0.56357883 0.6652412  0.41134124  1.0000000 -0.1485294
    ## CancerRate 0.34804801 0.0853317 -0.42144530 -0.1485294  1.0000000

``` r
# Make a heatmap
cor(DataSet2_Num, use = "pairwise.complete.obs") %>%
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

![](Project-2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> A
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

MANOVA test

``` r
# create MANOVA for the three relevant numerical predictor variables
manova_crime <- manova(cbind(Assault, Rape, CancerRate) ~ Urban, data = DataSet2)
summary(manova_crime)
```

    ##           Df   Pillai approx F num Df den Df Pr(>F)
    ## Urban      1 0.076148   1.2638      3     46 0.2979
    ## Residuals 48

After running our MANOVA tests to see if there is an interaction between
the variable “Urban”, and murder, assaults, and rape per 100,000 we now
know that with very large p values (F = 1.2638, df = 1, p-val = 0.2979),
there does not seem to be a significant mean difference for murder,
assault, and rape across the urban and rural dominant states. When
looking at the data and checking for assumptions such as normality and
equal variance, these assumptions were most likely not met because of
the variance between urban and rural is different.

Randomization Test

For the randomization test, I will be determining if the difference in
cancer deaths per 100,000 between urban and rural states is
significantly different. We will re-sample from our sample multiple
times and calculate the difference in cancer deaths between urban and
rural states.

The null hypothesis for this test is that the true mean difference in
cancer deaths per 100,000 between urban and rural states is zero

The alternative hypothesis for this test is that the true mean
difference in cancer per 100,000 is not equal to zero

``` r
# Find true difference in cancer deaths between urban and rural
true_diff <- DataSet2 %>%
  group_by(Urban) %>%
  summarize(means = mean(CancerRate)) %>%
  summarize(mean_diffs = diff(means)) %>%
  pull
# resample 5000 times
mean_diff <- vector()

for(i in 1:5000){
temp <- data.frame(Urban = DataSet2$Urban, CancerRate = sample(DataSet2$CancerRate))
  mean_diff[i] <- temp %>%
    group_by(Urban) %>%
    summarize(means = mean(CancerRate)) %>%
    summarize(mean_diff = diff(means)) %>%
    pull
}
# show null distribution
{hist(mean_diff, main = "Distribution of Mean Cancer deaths per 100,000 between urban and rural states"); abline(v = true_diff, col = "red")}
```

![](Project-2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# calculate a two side p value
mean(mean_diff > true_diff | mean_diff < -true_diff)
```

    ## [1] 1

The above histogram shows the distribution of cancer deaths per 100,000
mean differences between urban and rural states when the data charges
data was re-sampled 5000 times. The distribution is normally
distributed. The red line indicates the observed difference in mean
cancer deaths per 100,000 between urban and rural states.

``` r
# calculate a two sided p-value
mean(mean_diff > true_diff | mean_diff < -true_diff)
```

    ## [1] 1

With the randomization test constructed, it was determined that there
was a insignificant difference between urban and rural states when the
data charges data was resampled 5000 times. With the p-value being a
very large number we cannot reject our null hypothesis.

Linear Regression Model After both a MANOVA test and a randomization
test have been performed, a linear regression model can be fitted to the
data set. The explanatory variable is the number of murders per 100,000
while the predictor variables will be assaults per 100,000 , rapes per
100,000 and urban status. The assaults and rapes per 100,000 were mean
centered prior to fitting the model.

``` r
# consider character variables as factors
Murders <- DataSet2 %>%
  mutate_if(is.character, as.factor)
# mean center assaults and rape variable
Murders$Assault_c <- DataSet2$Assault - mean(DataSet2$Assault)
Murders$Rape_c <- DataSet2$Rape - mean(DataSet2$Rape)
# fit model
mlr_fit <- lm(Murder ~ Murders$Assault_c * Murders$Rape_c * Urban, data = DataSet2)
summary(mlr_fit)
```

    ## 
    ## Call:
    ## lm(formula = Murder ~ Murders$Assault_c * Murders$Rape_c * Urban, 
    ##     data = DataSet2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3109 -1.0607 -0.0557  1.0282  7.6108 
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error t value
    ## (Intercept)                                 10.623952   3.465851   3.065
    ## Murders$Assault_c                            0.023315   0.032178   0.725
    ## Murders$Rape_c                               0.144531   0.272555   0.530
    ## Urbanurban                                  -2.490875   3.505720  -0.711
    ## Murders$Assault_c:Murders$Rape_c            -0.002663   0.003120  -0.853
    ## Murders$Assault_c:Urbanurban                 0.010538   0.033100   0.318
    ## Murders$Rape_c:Urbanurban                   -0.038785   0.281337  -0.138
    ## Murders$Assault_c:Murders$Rape_c:Urbanurban  0.001633   0.003204   0.510
    ##                                             Pr(>|t|)   
    ## (Intercept)                                  0.00379 **
    ## Murders$Assault_c                            0.47272   
    ## Murders$Rape_c                               0.59871   
    ## Urbanurban                                   0.48131   
    ## Murders$Assault_c:Murders$Rape_c             0.39833   
    ## Murders$Assault_c:Urbanurban                 0.75178   
    ## Murders$Rape_c:Urbanurban                    0.89101   
    ## Murders$Assault_c:Murders$Rape_c:Urbanurban  0.61288   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.66 on 42 degrees of freedom
    ## Multiple R-squared:  0.6803, Adjusted R-squared:  0.6271 
    ## F-statistic: 12.77 on 7 and 42 DF,  p-value: 1.186e-08

``` r
summary(mlr_fit)$r.sq
```

    ## [1] 0.6803335

None of the interactions were significant, meaning that neither assault
per 100,000 , rape per 100,000 or state urban status was a good
predictor for the number of murders per 100,000. However we can still
interpret the calculated coeffcients.

While holding rapes per 100,000 and urban status constant, it is
expected that the average number of murders per 100,000 increase by
0.023315 for every 1 assault increase per 100,000.

While holding assault per 100,000 and urban status constant, it is
expected that the number of murders per 100,000 increase by 0.144531 for
every 1 rape increase per 100,000.

While holding both assaults and rapes per 100,000 constant, it is
expected that the average number of murders per 100,000 on average
2.490875 times less per 100,000 in “rural” states compared to “urban
states”

There is no significant interaction between assaults and rape per
100,000 indicating that the effect of assaults on murders per 100,000 is
the same even when manipulating the number of rapes per 100,000 and vice
versa.

Once again, there did not seem to be a significant interaction between
assaults per 100,000 and a state being primarily urban as assaults
increased only by 0.010538 assaults per 100,000 in urban states compared
to rural states.

Finally, there again does not seem to be a significant interaction
between rapes per 100,000 and a state being primary urban. Rapes
actually happen 0.038785 less per 100,000 in urban states compared to
rural states.

According to the calculated R squared value, it can be determined that
this model suggests that 68.03% of the variation in murders per 100,000
can be determined by the variation in assaults and rapes per 100,000 and
the urban/rural status of a state. Although none of the interactions
were significant, we can still interpret the coefficients to understand
what they mean.

``` r
ggplot(DataSet2, aes(x= Assault, y= Murder, color = Urban)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  xlab("Assaults per 100,000") + ylab("Murders per 100,000") +
  ggtitle("Murders per 100,000 by Assaults per 100,000 by Urban status")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Project-2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- --> It is
fairly obvious that the slopes between murders and assaults per 100,000
do not vary depending on whether the state is considered rural or urban.
Although there seems to be a clear positively linear relationship
between murders and assaults per 100,000 there does not seem to be a
distinct difference between rural and urban states.

Assumptions

``` r
#check for linearity and equal variance
plot(mlr_fit, which = 1)
```

![](Project-2_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# check for normality
plot(mlr_fit, which = 2)
```

![](Project-2_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
bptest(mlr_fit)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  mlr_fit
    ## BP = 7.6807, df = 7, p-value = 0.3616

``` r
shapiro.test(mlr_fit$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  mlr_fit$residuals
    ## W = 0.96338, p-value = 0.1231

Based on the graph of residuals by fitted values it is likely that the
linearity and homoscedasticity assumptions are met. Based on the QQ-plot
and the Shapiro-Wilk test, the normality assumption also seems to be
met. The independence assumption is likely to be met, but can’t be know
for sure based on the unknown data collection methods

``` r
# log transformation
trans_mlr_fit <- lm(log(Murder) ~ Murders$Assault_c * Murders$Rape_c * Urban, data = DataSet2)
bptest(trans_mlr_fit)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  trans_mlr_fit
    ## BP = 8.8182, df = 7, p-value = 0.266

``` r
shapiro.test(trans_mlr_fit$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  trans_mlr_fit$residuals
    ## W = 0.98346, p-value = 0.7039

``` r
# square root transformation
trans_mlr_fit <- lm(sqrt(Murder) ~ Murders$Assault_c * Murders$Rape_c * Urban, data = DataSet2)
bptest(trans_mlr_fit)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  trans_mlr_fit
    ## BP = 7.762, df = 7, p-value = 0.354

``` r
shapiro.test(trans_mlr_fit$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  trans_mlr_fit$residuals
    ## W = 0.98193, p-value = 0.6356

``` r
#reciprocal transformation
trans_mlr_fit <- lm(1/(Murder) ~ Murders$Assault_c * Murders$Rape_c * Urban, data = DataSet2)
bptest(trans_mlr_fit)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  trans_mlr_fit
    ## BP = 20.288, df = 7, p-value = 0.004981

``` r
shapiro.test(trans_mlr_fit$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  trans_mlr_fit$residuals
    ## W = 0.85225, p-value = 1.781e-05

Although the initial model indicated that the normality assumption was
met, I did subsequent transformations to confirm this. After making each
transformation the p values remained &gt;0.05 so the normality
assumption was further confirmed.

Bootstrapped/Robust Standard Errors

``` r
# original coefficients
summary(mlr_fit$coefficients)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -2.490875 -0.011693  0.006086  1.033956  0.053619 10.623952

``` r
# robust coefficients
coeftest(mlr_fit, vcov = vcovHC(mlr_fit))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                                               Estimate Std. Error t value
    ## (Intercept)                                 10.6239520  4.0349118  2.6330
    ## Murders$Assault_c                            0.0233154  0.0442350  0.5271
    ## Murders$Rape_c                               0.1445308  0.3881919  0.3723
    ## Urbanurban                                  -2.4908748  4.0872261 -0.6094
    ## Murders$Assault_c:Murders$Rape_c            -0.0026626  0.0036028 -0.7390
    ## Murders$Assault_c:Urbanurban                 0.0105384  0.0451590  0.2334
    ## Murders$Rape_c:Urbanurban                   -0.0387850  0.3961998 -0.0979
    ## Murders$Assault_c:Murders$Rape_c:Urbanurban  0.0016334  0.0036749  0.4445
    ##                                             Pr(>|t|)  
    ## (Intercept)                                  0.01179 *
    ## Murders$Assault_c                            0.60091  
    ## Murders$Rape_c                               0.71153  
    ## Urbanurban                                   0.54552  
    ## Murders$Assault_c:Murders$Rape_c             0.46400  
    ## Murders$Assault_c:Urbanurban                 0.81661  
    ## Murders$Rape_c:Urbanurban                    0.92248  
    ## Murders$Assault_c:Murders$Rape_c:Urbanurban  0.65898  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

After determining the robust standard errors, there seems to be no
changes in the significance of coefficients. No variables seem to show
significance in predicting the number of murders per 100,000 as well as
the interaction between assaults, rapes, and urban status.

``` r
# Bootstrap from observations
# Repeat bootstrapping 5000 times 
samp_SEs <- replicate(5000, {
  boot_data <- sample_frac(Murders, replace = TRUE)
  fitboot <- lm(Murder ~ Assault_c * Rape_c, data = boot_data)
  coef(fitboot)
})
#Estimated SEs
samp_SEs %>%
  t %>%
  as.data.frame %>%
  summarize_all(sd)
```

    ##   (Intercept)   Assault_c     Rape_c Assault_c:Rape_c
    ## 1   0.5303828 0.006796847 0.06201825     0.0005398842

When comparing the bootstrapped standard errors to the standard errors
of the original model, there seems to be a specific pattern as they are
small than before. The standard errors of the non-significant
coefficients appeared to become smaller. Because these numbers most
likely have a smaller p-value with the bootstrapped SE’s they are most
likely more robust.

Logistic Regression Because our data already has a binary variable of a
state being “urban” or “rural”, we can focus on building a regression
model which will predict a binary response.

``` r
# create logistic regression model
Urban_number <- DataSet2 %>%
  mutate(Urban = ifelse(Urban == "urban", 1, 0))
# Create logistic regression model
logi_fit <- glm(Urban ~ Murder + Assault + Rape, data = Urban_number)
summary(logi_fit)
```

    ## 
    ## Call:
    ## glm(formula = Urban ~ Murder + Assault + Rape, data = Urban_number)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.06585   0.04444   0.14962   0.19287   0.29320  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.7489423  0.1362576   5.497 1.63e-06 ***
    ## Murder      -0.0095321  0.0203203  -0.469   0.6412    
    ## Assault     -0.0006933  0.0011750  -0.590   0.5581    
    ## Rape         0.0133607  0.0075618   1.767   0.0839 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.1364007)
    ## 
    ##     Null deviance: 6.7200  on 49  degrees of freedom
    ## Residual deviance: 6.2744  on 46  degrees of freedom
    ## AIC: 48.117
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
# find exponential of coefficients
exp(coef(logi_fit))
```

    ## (Intercept)      Murder     Assault        Rape 
    ##   2.1147620   0.9905131   0.9993070   1.0134504

In a logistic regression model, the calculated coefficients represent
the log of the odds. When interpreting the coefficients, the exponential
of the coefficients is calculated in order to interpret only odds.

The odds of a state being “urban” increases by a factor of 0.9905131 for
every increase of 1 murder per 100,000

The odds of a state being “urban” increases by a factor of 0.9993070 for
every increase of 1 assault per 100,000

The odds of a state being “urban” increases by a factor of 1.0134504 for
every increase of 1 rape per 100,000

``` r
#Create named binary value
Urban_number$outcome <- ifelse(Urban_number$Urban == 1, "urban", "rural")
# Create variables for probability of being urban state
Urban_number$prob <- predict(logi_fit, type = "response")
# Create predictions
Urban_number$predicted <- ifelse(Urban_number$prob > .5, "urban", "rural")
# Confusion matrix
table(truth = Urban_number$outcome, prediction = Urban_number$predicted) %>% addmargins()
```

    ##        prediction
    ## truth   urban Sum
    ##   rural     8   8
    ##   urban    42  42
    ##   Sum      50  50

Above is the confusion matrix created for our predicted model. It
illustrates the counts of correctly and incorrect urban and rural states
Because there is no relationship between violent crime and a state being
considered “urban” there is no confusion and the model is 100% accurate.

ROC plot

``` r
# create roc plot
ROCplot <- ggplot(Urban_number) +
  geom_roc(aes(d = Urban, m = prob), n.cuts = 0, color = "blue") + ggtitle ("Assault, Rape and Urban Status Interaction ROC plot")
ROCplot
```

![](Project-2_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
# Calculate area under curve
calc_auc(ROCplot)
```

    ##   PANEL group       AUC
    ## 1     1    -1 0.6994048

The calculated cross-validation area under the curve is 0.6994048 which
indicates that the constructed model correctly classifies observations
69.94% of the time. The constructed model is a fairly good model.
