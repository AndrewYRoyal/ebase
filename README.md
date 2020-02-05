
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
#>        meterID                date       use     temp
#>     1:       B 2016-04-15 04:00:00 0.7501613 53.30387
#>     2:       B 2016-04-15 05:00:00 0.7503226 51.45548
#>     3:       B 2016-04-15 06:00:00 0.4000000 51.45839
#>     4:       B 2016-04-15 07:00:00 0.1106452 55.98065
#>     5:       B 2016-04-15 08:00:00 0.0450000 59.08923
#>    ---                                               
#> 68234:       C 2019-03-01 01:00:00 0.4011111 58.19000
#> 68235:       C 2019-03-01 02:00:00 0.3986111 57.95333
#> 68236:       C 2019-03-01 03:00:00 0.3986111 57.54667
#> 68237:       C 2019-03-01 04:00:00 0.3991667 55.93000
#> 68238:       C 2019-03-01 05:00:00 0.4005556 56.50000
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
#> [1] "B" "C" "A"
print(data_formatted$list()[['baseline']][['A']][, 1:5])
#>       meterID                date       use     temp   period
#>    1:       A 2016-08-15 04:00:00 0.6400000 68.21276 baseline
#>    2:       A 2016-08-15 05:00:00 0.6395000 68.14000 baseline
#>    3:       A 2016-08-15 06:00:00 0.3090000 68.48735 baseline
#>    4:       A 2016-08-15 07:00:00 0.0707500 74.23700 baseline
#>    5:       A 2016-08-15 08:00:00 0.0617500 79.85300 baseline
#>   ---                                                        
#> 8752:       A 2017-08-14 19:00:00 0.3623810 66.81714 baseline
#> 8753:       A 2017-08-14 20:00:00 0.6811905 67.49973 baseline
#> 8754:       A 2017-08-14 21:00:00 0.6802381 64.98286 baseline
#> 8755:       A 2017-08-14 22:00:00 0.6845238 64.58286 baseline
#> 8756:       A 2017-08-14 23:00:00 0.6804762 64.62143 baseline
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
#>        meterID                date      period       use       pUse
#>     1:       A 2016-08-15 04:00:00    baseline 0.6400000 0.64311039
#>     2:       A 2016-08-15 05:00:00    baseline 0.6395000 0.61836415
#>     3:       A 2016-08-15 06:00:00    baseline 0.3090000 0.37841904
#>     4:       A 2016-08-15 07:00:00    baseline 0.0707500 0.06359560
#>     5:       A 2016-08-15 08:00:00    baseline 0.0617500 0.03531005
#>    ---                                                             
#> 21424:       A 2019-01-24 19:00:00 performance 0.4035714 0.58710930
#> 21425:       A 2019-01-24 20:00:00 performance 0.4069048 0.72100235
#> 21426:       A 2019-01-24 21:00:00 performance 0.4026190 0.75461848
#> 21427:       A 2019-01-24 22:00:00 performance 0.4002381 0.72482788
#> 21428:       A 2019-01-24 23:00:00 performance 0.3997619 0.72391385
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
#> 1: Example Site 1    6,694  3,000            0.45 2,374  0.79
#> 2: Example Site 2    3,803  1,000            0.26 1,550  1.55
#> 
#> $raw
#>                id  norm    Gross Baseline Deemed var_gross
#> 1: Example Site 1 FALSE 2374.343 6694.216   3000 114023.42
#> 2: Example Site 2 FALSE 1550.467 3802.650   1000  34779.22
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
#> 1:       A 0.92     22 1e-13 3324.284
#> 2:       B 0.91     25 8e-14 3369.932
#> 3:       C 0.92     24 1e-13 3802.650
#> 
#> $meters$savings
#>    meterID    gross var_gross
#> 1:       A 1279.495  25883.46
#> 2:       B 1094.847  30232.17
#> 3:       C 1550.467  34779.22
#> 
#> $meters$norms
#> NULL
#> 
#> 
#> $sites
#> $sites$metrics
#>              site   r2 cvrmse  nmbe baseline
#> 1: Example Site 1 0.93     25 1e-13 6694.216
#> 2: Example Site 2 0.92     24 1e-13 3802.650
#> 
#> $sites$savings
#>              site    gross var_gross
#> 1: Example Site 1 2374.343 114023.42
#> 2: Example Site 2 1550.467  34779.22
#> 
#> $sites$norms
#> NULL
```
