
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ebase

<!-- badges: start -->

<!-- badges: end -->

The `ebase` package provides an ensemble of tools to model and visualize
metered energy data. Its primary use-case is baseline modeling and
savings calculations for net metered energy analysis, as it is used in
M\&V evaluation. However, the package also includes tools for other
applications such as temperature profiling, loadpath cluster analysis
and outlier detection.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AndrewYRoyal/ebase")
```

## Example

This is a example which shows how to use `ebase` to

1.  Estimate a baseline model of metered consumption prior to a measure
    installation,
2.  Use the baseline model to predict consumption before and after the
    installation, and
3.  Use the predictions to calculate savings associated with the
    measure(s).

<!-- end list -->

``` r
library(ebase)
library(magrittr)
library(data.table)
```

### Data

The package includes sample two years hourly metered data for two
simulated sites. Each meter is assigned a simulated measure installation
date. In the example below, we calculate energy savings realized over
the 365 days following the installation dates.

``` r
print(ebSample_sites)
#>                A                B                C 
#> "Example Site 1" "Example Site 1" "Example Site 2"
print(ebSample_install_dates)
#> $A
#> [1] "2017-08-15 UTC" "2018-01-25 UTC"
#> 
#> $B
#> [1] "2017-04-15 UTC" "2018-02-01 UTC"
#> 
#> $C
#> [1] "2017-08-15 UTC" "2018-03-01 UTC"
print(ebSample_deemed) # Engineering estimates for annual savings.
#> Example Site 1 Example Site 2 
#>           3000           1000
print(ebSample_hourly)
#>        meterID                date  use  temp
#>     1:       A 2016-08-15 04:00:00 0.64 68.21
#>     2:       A 2016-08-15 05:00:00 0.64 68.14
#>     3:       A 2016-08-15 06:00:00 0.31 68.49
#>     4:       A 2016-08-15 07:00:00 0.07 74.24
#>     5:       A 2016-08-15 08:00:00 0.06 79.85
#>    ---                                       
#> 68234:       C 2019-03-01 01:00:00 0.40 58.19
#> 68235:       C 2019-03-01 02:00:00 0.40 57.95
#> 68236:       C 2019-03-01 03:00:00 0.40 57.55
#> 68237:       C 2019-03-01 04:00:00 0.40 55.93
#> 68238:       C 2019-03-01 05:00:00 0.40 56.50
```

First, we format the hourly consumption data, dividing it into
basleline, blackout and performance periods that signify the time
intervals before, during and after the measure installations.

``` r
data_formatted = ebDataFormat(x = ebSample_hourly,
                              install_dates = ebSample_install_dates,
                              sites = ebSample_sites)
#> 3 total meters 
#> 3 with sufficient data
```

The resulting object can be used to return different versions of
formatted data. This can be achieved using the `stack()` or `list()`
methods. The `stack()` method creates a list of `data.table` objects,
one for each period, where each table includes (stacks) readings from
all meters. The `list()` method returns a list of lists, where each item
contains a `data.table` object for the meter-period pairing.

``` r
names(data_formatted$stack())
#> [1] "baseline"    "blackout"    "performance"
names(data_formatted$list())
#> [1] "baseline"    "blackout"    "performance"
names(data_formatted$list()[['baseline']])
#> [1] "A" "B" "C"
print(data_formatted$list()[['baseline']][['A']][, 1:5])
#>       meterID                date  use  temp   period
#>    1:       A 2016-08-15 04:00:00 0.64 68.21 baseline
#>    2:       A 2016-08-15 05:00:00 0.64 68.14 baseline
#>    3:       A 2016-08-15 06:00:00 0.31 68.49 baseline
#>    4:       A 2016-08-15 07:00:00 0.07 74.24 baseline
#>    5:       A 2016-08-15 08:00:00 0.06 79.85 baseline
#>   ---                                                
#> 8752:       A 2017-08-14 19:00:00 0.36 66.82 baseline
#> 8753:       A 2017-08-14 20:00:00 0.68 67.50 baseline
#> 8754:       A 2017-08-14 21:00:00 0.68 64.98 baseline
#> 8755:       A 2017-08-14 22:00:00 0.68 64.58 baseline
#> 8756:       A 2017-08-14 23:00:00 0.68 64.62 baseline
```

The `list()` formatting is useful when using `lapply()` to map a
function over the entire set of meters, as we do when fitting models.

### Modeling

To estimate model, we simply use the invoke the `ebModel()` function on
the desired subset of formatted data. We can then use the resulting
model to generate a predictions. The following code estimates a model
using meter A’s baseline period consumption (`use`), and then uses the
model predict consumption (`pUse`) over all periods:

``` r
ebModel(dat = data_formatted$list()[['baseline']][['A']]) %>%
  predict(data_formatted$stack('meter')[['A']])
#> X
#>        meterID                date      period  use       pUse
#>     1:       A 2016-08-15 04:00:00    baseline 0.64 0.64452052
#>     2:       A 2016-08-15 05:00:00    baseline 0.64 0.61831530
#>     3:       A 2016-08-15 06:00:00    baseline 0.31 0.37915549
#>     4:       A 2016-08-15 07:00:00    baseline 0.07 0.06320861
#>     5:       A 2016-08-15 08:00:00    baseline 0.06 0.03588094
#>    ---                                                        
#> 21424:       A 2019-01-24 19:00:00 performance 0.40 0.58693652
#> 21425:       A 2019-01-24 20:00:00 performance 0.41 0.72135244
#> 21426:       A 2019-01-24 21:00:00 performance 0.40 0.75441361
#> 21427:       A 2019-01-24 22:00:00 performance 0.40 0.72579152
#> 21428:       A 2019-01-24 23:00:00 performance 0.40 0.72414492
```

### Savings

To estimate total savings from measures, we must generate models and
predictions for all three meters. This is best achieved using the
`lapply()` function to map `ebModel` to all baseline meter data. The
`ebPredict()` simplifies the prediction process for multiple meters.
Lastly `ebSummary` and `ebSavings()` summarize the model predictions and
calculate savings:

``` r
lapply(data_formatted$list()[['baseline']], ebModel) %>%
  ebPredict(data_formatted) %>%
  ebSummary %>%
  ebSavings(deemed = ebSample_deemed)
#> XXX
#> $formatted
#>                id Baseline Deemed Deemed/Baseline Gross RRate
#> 1: Example Site 1    6,692  3,000            0.45 2,372  0.79
#> 2: Example Site 2    3,803  1,000            0.26 1,550  1.55
#> 
#> $raw
#>                id  norm    Gross Baseline Deemed var_gross
#> 1: Example Site 1 FALSE 2372.481  6692.47   3000  114110.2
#> 2: Example Site 2 FALSE 1549.747  3802.73   1000   34774.0
#> 
#> attr(,"class")
#> [1] "savings" "list"
```

The results show that the measures achieved savings of 2,374 kWh and
1,550 at the two sites, signifying realization rates of 0.79 and 1.55
when compared to deemed savings. Energy savings and loadpaths can be
visualized using the `ebPlot()` function.

Lastly, the intermediate `ebSummary()` function can be used to evaluate
metrics that indicate the quality of the models at the meter and site
level (such as CVRMSE):

``` r
lapply(data_formatted$list()[['baseline']], ebModel) %>%
  ebPredict(data_formatted) %>%
  ebSummary 
#> XXX
#> $meters
#> $meters$metrics
#>    meterID   r2 cvrmse  nmbe baseline
#> 1:       A 0.92     22 1e-13  3323.65
#> 2:       B 0.91     25 1e-13  3368.82
#> 3:       C 0.92     24 8e-14  3802.73
#> 
#> $meters$savings
#>    meterID    gross var_gross
#> 1:       A 1278.293  25885.93
#> 2:       B 1094.187  30266.52
#> 3:       C 1549.747  34774.00
#> 
#> $meters$norms
#> NULL
#> 
#> 
#> $sites
#> $sites$metrics
#>              site   r2 cvrmse  nmbe baseline
#> 1: Example Site 1 0.93     25 1e-13  6692.47
#> 2: Example Site 2 0.92     24 8e-14  3802.73
#> 
#> $sites$savings
#>              site    gross var_gross
#> 1: Example Site 1 2372.481  114110.2
#> 2: Example Site 2 1549.747   34774.0
#> 
#> $sites$norms
#> NULL
```
