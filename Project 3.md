# Project 3
## Daniel Cho dhc682
## 5/8/21


```python
# Allows multiple outputs from a chunk
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = "all"
```


```python
# Import packages
import numpy as np
import pandas as pd
import seaborn as sns
import scipy.stats as stats
import matplotlib.pyplot as plt
```

### Features of Dataset


```python
# Open Death Rates dataset
Data = pd.read_csv("Project2Dataset.csv")
# head dataset
Data.head()
# Information from the dataset
Data.info()
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>X</th>
      <th>Code</th>
      <th>Murder</th>
      <th>Assault</th>
      <th>UrbanPop</th>
      <th>Rape</th>
      <th>CancerRate</th>
      <th>Urban</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>1</th>
      <td>Alabama</td>
      <td>AL</td>
      <td>13.2</td>
      <td>236</td>
      <td>58</td>
      <td>21.2</td>
      <td>182.1</td>
      <td>urban</td>
    </tr>
    <tr>
      <th>2</th>
      <td>Alaska</td>
      <td>AK</td>
      <td>10.0</td>
      <td>263</td>
      <td>48</td>
      <td>44.5</td>
      <td>173.1</td>
      <td>rural</td>
    </tr>
    <tr>
      <th>3</th>
      <td>Arizona</td>
      <td>AZ</td>
      <td>8.1</td>
      <td>294</td>
      <td>80</td>
      <td>31.0</td>
      <td>146.4</td>
      <td>urban</td>
    </tr>
    <tr>
      <th>4</th>
      <td>Arkansas</td>
      <td>AR</td>
      <td>8.8</td>
      <td>190</td>
      <td>50</td>
      <td>19.5</td>
      <td>189.6</td>
      <td>urban</td>
    </tr>
    <tr>
      <th>5</th>
      <td>California</td>
      <td>CA</td>
      <td>9.0</td>
      <td>276</td>
      <td>91</td>
      <td>40.6</td>
      <td>146.6</td>
      <td>urban</td>
    </tr>
  </tbody>
</table>
</div>



    <class 'pandas.core.frame.DataFrame'>
    Int64Index: 50 entries, 1 to 50
    Data columns (total 8 columns):
    X             50 non-null object
    Code          50 non-null object
    Murder        50 non-null float64
    Assault       50 non-null int64
    UrbanPop      50 non-null int64
    Rape          50 non-null float64
    CancerRate    50 non-null float64
    Urban         50 non-null object
    dtypes: float64(3), int64(2), object(3)
    memory usage: 3.5+ KB


In the Deathrates dataset, there are 50 observations each with 8 different variables. The catagorical variables are State names, state abbreviations, and urban/rural status while the numerical variables are murders per 100,000 , assaults per 100,000 , rapes per 100,000 , and cancer related deaths per 100,000.

### Explanatory Data Analysis


```python
# use describe to show summary statistics for variables
Data.describe()
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Murder</th>
      <th>Assault</th>
      <th>UrbanPop</th>
      <th>Rape</th>
      <th>CancerRate</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>count</th>
      <td>50.00000</td>
      <td>50.000000</td>
      <td>50.000000</td>
      <td>50.000000</td>
      <td>50.000000</td>
    </tr>
    <tr>
      <th>mean</th>
      <td>7.78800</td>
      <td>170.760000</td>
      <td>65.540000</td>
      <td>21.232000</td>
      <td>164.834000</td>
    </tr>
    <tr>
      <th>std</th>
      <td>4.35551</td>
      <td>83.337661</td>
      <td>14.474763</td>
      <td>9.366385</td>
      <td>15.571004</td>
    </tr>
    <tr>
      <th>min</th>
      <td>0.80000</td>
      <td>45.000000</td>
      <td>32.000000</td>
      <td>7.300000</td>
      <td>127.900000</td>
    </tr>
    <tr>
      <th>25%</th>
      <td>4.07500</td>
      <td>109.000000</td>
      <td>54.500000</td>
      <td>15.075000</td>
      <td>155.200000</td>
    </tr>
    <tr>
      <th>50%</th>
      <td>7.25000</td>
      <td>159.000000</td>
      <td>66.000000</td>
      <td>20.100000</td>
      <td>163.650000</td>
    </tr>
    <tr>
      <th>75%</th>
      <td>11.25000</td>
      <td>249.000000</td>
      <td>77.750000</td>
      <td>26.175000</td>
      <td>173.975000</td>
    </tr>
    <tr>
      <th>max</th>
      <td>17.40000</td>
      <td>337.000000</td>
      <td>91.000000</td>
      <td>46.000000</td>
      <td>199.300000</td>
    </tr>
  </tbody>
</table>
</div>



The mean murders, assaults, rapes, and cancer deaths per 100,000 are 7.788, 170.76, 21.232, and 162.834 per 100,000 respectively. The mean percentage of urban population in a state is 65.54%. The highest maximum violent crime per 100,000 in a stats was 337 assaults per 100,000 and the lowest minimum violent crime per 100,000 was 0.8 murders per 100,000. The highest percentage of urban population of a state was 91% while the lowest was 32%. 


```python
# Describe the State variable
Data['X'].value_counts()
# Describe Code variable
Data['Code'].value_counts()
# Describe Urban variable
Data['Urban'].value_counts()
```




    Nebraska          1
    Colorado          1
    Idaho             1
    New Jersey        1
    Arkansas          1
    Georgia           1
    Florida           1
    New York          1
    Montana           1
    Rhode Island      1
    Vermont           1
    Minnesota         1
    South Dakota      1
    Louisiana         1
    Virginia          1
    Alaska            1
    Maryland          1
    Oregon            1
    Iowa              1
    Ohio              1
    Indiana           1
    Maine             1
    Kentucky          1
    California        1
    Michigan          1
    Washington        1
    Massachusetts     1
    North Carolina    1
    South Carolina    1
    Hawaii            1
    Connecticut       1
    Wyoming           1
    Texas             1
    Delaware          1
    Missouri          1
    Kansas            1
    Utah              1
    Arizona           1
    Tennessee         1
    North Dakota      1
    West Virginia     1
    New Mexico        1
    Pennsylvania      1
    Nevada            1
    New Hampshire     1
    Mississippi       1
    Wisconsin         1
    Oklahoma          1
    Alabama           1
    Illinois          1
    Name: X, dtype: int64






    MS    1
    MT    1
    SC    1
    GA    1
    WA    1
    IN    1
    RI    1
    ME    1
    NC    1
    WI    1
    KS    1
    LA    1
    ID    1
    SD    1
    WV    1
    AR    1
    VA    1
    AZ    1
    KY    1
    MD    1
    CA    1
    OR    1
    ND    1
    MO    1
    UT    1
    NV    1
    AK    1
    IL    1
    PA    1
    NY    1
    OK    1
    MA    1
    MI    1
    NM    1
    OH    1
    CT    1
    HI    1
    NE    1
    NH    1
    WY    1
    AL    1
    TX    1
    NJ    1
    CO    1
    MN    1
    FL    1
    DE    1
    VT    1
    IA    1
    TN    1
    Name: Code, dtype: int64






    urban    42
    rural     8
    Name: Urban, dtype: int64



Within the death rates dataset, there were 50 states from the United States of America with each state having a unique abbreviation code which gives us 50 different state codes. There were a total of 42 states that had more than or equal to 50% of their population living in urban areas while there were 8 states that had less than 50% of their population living in urban areas


```python
# find descriptive statistics for the number of cancer related deaths per 100,000
Data['CancerRate'].value_counts()
# create histogram for cancer deaths per 100,000
Data['CancerRate'].plot(kind = "hist")
plt.title("Distribution of Cancer Deaths per 100,000 in United States")
plt.xlabel("Deaths per 100,000")
```




    185.4    2
    156.3    2
    162.3    1
    199.3    1
    171.7    1
    158.6    1
    147.8    1
    139.2    1
    156.9    1
    177.4    1
    179.4    1
    164.6    1
    179.1    1
    190.5    1
    145.1    1
    174.0    1
    155.1    1
    170.0    1
    155.5    1
    134.9    1
    156.0    1
    168.1    1
    154.0    1
    196.5    1
    168.2    1
    150.8    1
    167.1    1
    188.7    1
    173.9    1
    146.6    1
    154.9    1
    147.7    1
    164.1    1
    189.6    1
    163.2    1
    167.7    1
    173.1    1
    127.9    1
    160.7    1
    164.9    1
    146.4    1
    182.1    1
    174.8    1
    154.1    1
    170.2    1
    159.7    1
    162.9    1
    163.0    1
    Name: CancerRate, dtype: int64






    <matplotlib.axes._subplots.AxesSubplot at 0x7fd6b1e913c8>






    Text(0.5,1,'Distribution of Cancer Deaths per 100,000 in United States')






    Text(0.5,0,'Deaths per 100,000')




![png](output_11_4.png)


The mean number of cancer related deaths per 100,000 for each state in the US is 164.834. Looking at the histogram, the distribution is relatively normal with maybe a slight negative skew. The range for the number of cancer related deaths is 22.4 deaths per 100,000. 


```python
# Find counts for urban variable
Data['Urban'].value_counts()
# Find proportion of Urban and Rural dominant states
42/ (42 + 8)
8/ (42 + 8)
# Create pie graph to show porportion of urban and rural states in data
Data["Urban"].value_counts() \
.plot(kind = "pie") \
.axis('equal')
plt.title("Proportion of Urban and Rural States")
```




    urban    42
    rural     8
    Name: Urban, dtype: int64






    0.84






    0.16






    (-1.105174517327238,
     1.1002464055870114,
     -1.1065079663193065,
     1.1061387189582923)






    Text(0.5,1,'Proportion of Urban and Rural States')




![png](output_13_5.png)


In my dataset, there were 42 urban majority states and 8 rural majority states. These counts are equal to 84% of American states being predomincantly urban and 16% of American states being predominantly rural.

### Hypothesis Test

It can be tested to see if there appears to be a significant difference in the number of cancer related deaths per 100,000 between urban and rural states. A two-sample t test can be conducted. The null hypothesis in this test is that the number of cancer related deaths per 100,000 will be the same for rural and urban states while the alternative hypothesis is that the number of cancer related deaths per 100,000 is different for urban and rural states.


```python
# Violin plot to assess assumptions
sns.violinplot(data = Data, x = "Urban", y = "CancerRate")
# 2 sample t test
stats.ttest_ind(Data['CancerRate'][Data['Urban'] == "urban"],
               Data['CancerRate'][Data['Urban'] == "rural"])
```




    <matplotlib.axes._subplots.AxesSubplot at 0x7fd6afd11048>






    Ttest_indResult(statistic=-1.300504935671595, pvalue=0.19963751148458847)




![png](output_17_2.png)


Looking at our violin plot to assess assumptions, the normality and variance assumptions seem to both be met. After running the two-sample t test, it could not be determined that there was a significance difference in number of cancer deaths per 100,000 between urban and rural states with a p-value of 0.1996. 


```python

```
