Untitled
================

## code for hotspot detection

This is code for …

## Data management

``` r
library(tidyverse)
library(spaMM)
library(sf)
library(dagirlite)

data("geo_sogne")

geo_sogne$expected_women <- geo_sogne$population * (sum(geo_sogne$women) / sum(geo_sogne$men) / 2)

nbs <- st_intersects(geo_sogne, geo_sogne, sparse = FALSE)
nbs[] <- +nbs
geo_sogne$id <- 1:nrow(geo_sogne)

fit <- fitme(women ~ 0 + offset(log(expected_women)) + adjacency(1|id), adjMatrix = nbs, 
             data = geo_sogne, family = "poisson", method = "REML")

pred <- predict(fit, variances = list(linpred = TRUE, disp = TRUE, respVar = TRUE))

geo_sogne$pred <- pred[]
geo_sogne$fitted <- fitted(fit)
geo_sogne$pv <- attr(pred, which = "predVar")
geo_sogne$prob_02 <- pnorm(log(1.02), 
                           mean = log(geo_sogne$pred) - log(geo_sogne$expected_women), 
                           sd = sqrt(geo_sogne$pv), lower.tail = FALSE)
```

### Raw rates

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### Smoothed using the CAR model

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Detected hotspots

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
