Rsample Time Series Evaluation
================
Matt Dancho
December 16, 2017

Overview
========

A brief document to test out the [`rsample` package](https://topepo.github.io/rsample/index.html) from Max Kuhn. This doc covers Time Series using the workflow discussed in the [rsample application examples](https://topepo.github.io/rsample/articles/Applications/Time_Series.html).

Libraries
=========

``` r
library(tidyquant)
library(rsample)
library(forecast)
library(sweep)
library(timetk)
```

Time Series
===========

A great article on [time series cross validation](https://robjhyndman.com/hyndsight/tscv/) discusses the procedure for using cummulative resampling. Another option is rolling origin resampling which keeps a fixed window and progressively increases the origin. Either way are good approaches to cross validation for time series.

Data
----

``` r
beer_sales <- tq_get("S4248SM144NCEN", get = "economic.data", from = "1990-01-01", to = as.character(today()))

beer_sales
```

    ## # A tibble: 310 x 2
    ##          date price
    ##        <date> <int>
    ##  1 1992-01-01  3459
    ##  2 1992-02-01  3458
    ##  3 1992-03-01  4002
    ##  4 1992-04-01  4564
    ##  5 1992-05-01  4221
    ##  6 1992-06-01  4529
    ##  7 1992-07-01  4466
    ##  8 1992-08-01  4137
    ##  9 1992-09-01  4126
    ## 10 1992-10-01  4259
    ## # ... with 300 more rows

Visualize the data.

``` r
beer_sales %>%
    ggplot(aes(x = date, y = price)) +
    geom_line(color = palette_light()[[1]], alpha = 0.5) +
    geom_point(color = palette_light()[[1]], alpha = 1) +
    geom_smooth(color = palette_light()[[2]], se = FALSE) +
    theme_tq() +
    labs(title = "Alcohol Sales: All Time")
```

![](rsample_time_series_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

Rolling Origin Resamples
------------------------

Create some rolling origin samples. Set `initial = 12 * 20` to return 20 years of samples.

``` r
ro_samples <- beer_sales %>%
    rolling_origin(initial = 12 * 20, assess = 12, skip = 6, cumulative = FALSE)

ro_samples
```

    ## # Rolling origin forecast resampling 
    ## # A tibble: 9 x 2
    ##         splits     id
    ##         <list>  <chr>
    ## 1 <S3: rsplit> Slice1
    ## 2 <S3: rsplit> Slice2
    ## 3 <S3: rsplit> Slice3
    ## 4 <S3: rsplit> Slice4
    ## 5 <S3: rsplit> Slice5
    ## 6 <S3: rsplit> Slice6
    ## 7 <S3: rsplit> Slice7
    ## 8 <S3: rsplit> Slice8
    ## 9 <S3: rsplit> Slice9

Visualize the sample strategy.

``` r
ro_samples %>%
    mutate(data = map(splits, ~ bind_rows(analysis = analysis(.x), assessment = assessment(.x), .id = "set"))) %>%
    select(id, data) %>%
    unnest() %>%
    group_by(id) %>%
    ggplot(aes(x = date, y = price, group = id, color = set)) +
    geom_line() +
    facet_wrap(~ id, ncol = 3) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Rolling Origin Sample Strategy")
```

![](rsample_time_series_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

Create the modeling function using the pipe of rsplit object:

-   `analysis()` - Extracts our test data in tibble form
-   `tk_ts()` - Converts to ts object
-   `auto.arima()` - produces the arima model

``` r
fit_arima <- function(x, ...) {
    x %>%
        analysis() %>%
        tk_ts(start  = .$date[[1]] %>% as.yearmon(), 
              freq   = 12, 
              silent = TRUE) %>%
        auto.arima(...)
}
```

Map our modeling function to the splits. This may take a f because of the `auto.arima()` function being applied to 9 training sets.

``` r
ro_samples <- ro_samples %>%
    mutate(arima = map(splits, fit_arima))
```

``` r
ro_samples <- readRDS(file = "data/ro_samples_arima.rds")
```

The arima column has been added to the data frame.

``` r
ro_samples
```

    ## # Rolling origin forecast resampling 
    ## # A tibble: 9 x 3
    ##         splits     id       arima
    ## *       <list>  <chr>      <list>
    ## 1 <S3: rsplit> Slice1 <S3: ARIMA>
    ## 2 <S3: rsplit> Slice2 <S3: ARIMA>
    ## 3 <S3: rsplit> Slice3 <S3: ARIMA>
    ## 4 <S3: rsplit> Slice4 <S3: ARIMA>
    ## 5 <S3: rsplit> Slice5 <S3: ARIMA>
    ## 6 <S3: rsplit> Slice6 <S3: ARIMA>
    ## 7 <S3: rsplit> Slice7 <S3: ARIMA>
    ## 8 <S3: rsplit> Slice8 <S3: ARIMA>
    ## 9 <S3: rsplit> Slice9 <S3: ARIMA>

Inspect insample error of the resamples.

``` r
ro_samples %>%
    mutate(in.error = map_dbl(arima, ~ sw_glance(.x) %>% pull(MAPE))) %>%
    pull(in.error) %>%
    summary()
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.841   2.926   2.942   2.951   2.965   3.135

Make some predictions.

``` r
pred_arima <- function(x, model_obj, ...) {
    
    h <- nrow(assessment(x))
    
    fcast <- forecast(model_obj, h = h, ...)
    
    ret <- x %>%
        assessment() %>%
        rename(actual = price) %>%
        mutate(pred   = fcast$mean %>% as.numeric())
    
    return(ret)
}
```

Make forecasts

``` r
ro_samples <- ro_samples %>%
    mutate(forecast = map2(splits, arima, pred_arima))

ro_samples
```

    ## # Rolling origin forecast resampling 
    ## # A tibble: 9 x 4
    ##         splits     id       arima          forecast
    ## *       <list>  <chr>      <list>            <list>
    ## 1 <S3: rsplit> Slice1 <S3: ARIMA> <tibble [12 x 3]>
    ## 2 <S3: rsplit> Slice2 <S3: ARIMA> <tibble [12 x 3]>
    ## 3 <S3: rsplit> Slice3 <S3: ARIMA> <tibble [12 x 3]>
    ## 4 <S3: rsplit> Slice4 <S3: ARIMA> <tibble [12 x 3]>
    ## 5 <S3: rsplit> Slice5 <S3: ARIMA> <tibble [12 x 3]>
    ## 6 <S3: rsplit> Slice6 <S3: ARIMA> <tibble [12 x 3]>
    ## 7 <S3: rsplit> Slice7 <S3: ARIMA> <tibble [12 x 3]>
    ## 8 <S3: rsplit> Slice8 <S3: ARIMA> <tibble [12 x 3]>
    ## 9 <S3: rsplit> Slice9 <S3: ARIMA> <tibble [12 x 3]>

``` r
mape <- function(x) {
    pct_error = (x$actual - x$pred) / x$actual * 100
    mean(abs(pct_error))
}
```

Out sample error.

``` r
ro_samples %>%
    mutate(out.error = map_dbl(forecast, mape)) %>%
    pull(out.error) %>%
    summary()
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.950   3.021   3.420   3.607   3.588   5.438
