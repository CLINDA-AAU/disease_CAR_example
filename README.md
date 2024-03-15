Disease detection using conditional autoregressive poisson model in R
================

## Background

Detecting disease hotspots of increased incidence in small geographical units is
usefull in investigating environmental exposures. The challenge is
determnining when something is increased across an area.

## Data management

We calculate expected number of women in a parish as the national rate
times population of each parish. Then the model is fitted.

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

pred <- predict(fit, variances = list(linpred = TRUE, 
                                      disp = TRUE, 
                                      respVar = TRUE))

geo_sogne$pred <- pred[]
geo_sogne$fitted <- fitted(fit)
geo_sogne$pv <- attr(pred, which = "predVar")

geo_sogne$prob_01 <- pnorm(log(1.005), 
                           mean = log(geo_sogne$pred) - log(geo_sogne$expected_women), 
                           sd = sqrt(geo_sogne$pv), lower.tail = FALSE)

geo_sogne$prob_02 <- pnorm(log(1.02), 
                           mean = log(geo_sogne$pred) - log(geo_sogne$expected_women), 
                           sd = sqrt(geo_sogne$pv), lower.tail = FALSE)
```

### Raw rates

Examining raw rates gives little hints in clusters as small parrishes
might have high/low rates by change, therefore smoothing using a CAR
model is adventageous.

``` r
geo_sogne |> ggplot() + 
  geom_sf(aes(fill = women / expected_women), color = NA) +
  labs(title="Rate ratio of women") + 
  scale_fill_distiller(palette = "RdPu", direction = 1)
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### Smoothed using the CAR model

Using a CAR model greatly reduces the range of rates as outliers are
penalized.

``` r
geo_sogne |> ggplot() + 
  geom_sf(aes(fill = fitted / expected_women), color = NA)+
  labs(title="Smoothed rate ratio of women") + 
  scale_fill_distiller(palette = "RdPu", direction = 1)
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Detected hotspots

If we choose to look for small levels of difference we find increased
rates of women close to large cities, and in high income areas. This
makes intutive sense.

``` r
geo_sogne |> pivot_longer(prob_01:prob_02, names_to = "increase", values_to = "val") %>% 
  ggplot() + 
  geom_sf(aes(fill = val > 0.95), color = NA) + 
  geom_point(data = . %>% filter(val > 0.95), 
             aes(x = visueltcenter_x, y = visueltcenter_y), size = 7, shape = 21) +
  labs(title="Significant clusters of women (0.5% and 2% increase)", x = NULL, y = NULL) + 
  scale_fill_grey(start = 0.8, end=0.2) + 
  facet_wrap(~increase)
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
