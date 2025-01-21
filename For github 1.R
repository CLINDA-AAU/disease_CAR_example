library(tidyverse)
library(sf)
library(dagirlite)
library(spdep)

data("geo_sogne")

## -----------------------------------------------------------------------------
## Setup
## -----------------------------------------------------------------------------

# Parish ids and expected number of women
geo_sogne$id <- 1:nrow(geo_sogne)
geo_sogne$expected_women <- 
  geo_sogne$population * (sum(geo_sogne$women) / sum(geo_sogne$men) / 2)

# Testing for smoothed irr >= 1 + theta with
theta <- 0.005

## -----------------------------------------------------------------------------
## Adjacency matrix - step 1: 
## Bordering parishes
## -----------------------------------------------------------------------------
nbs <- +st_intersects(geo_sogne, geo_sogne, sparse = F)
diag(nbs) <- 0

geo_tbl <- geo_sogne %>% st_drop_geometry()
geo_sp  <- as(geo_sogne, "Spatial")


## -----------------------------------------------------------------------------
## Adjacency matrix - step 2:
## Adding neighbors for
## 1) parishes = island (no neighbors). A neighbor relation was added for ferry 
##    and bridge connections
## -----------------------------------------------------------------------------

# Connections for parish = whole island (24)
island_con <- 
  tibble(kode = c(rep("7246", 3), "7257", "7326", "7327", "7485", 
                  rep("7521", 4), "7667", "7668", 
                  "7669", "7689", "7693", "7694", 
                  rep("7764", 2), rep("7765", 2), 
                  "7802", "7963" ,"8059", "8086", "8165", "8548", "8659",
                  "8855", rep("9183", 3), "9313", "7568"),
         navn = c(rep("Orø", 3), "Sejerø", "Agersø", "Omø", "Nyord", 
                  rep("Bogø", 4), "Fejø", "Femø",  
                  "Askø", "Thurø", "Drejø", "Strynø", 
                  rep("Lyø", 2), rep("Avernakø", 2), 
                  "Bågø", "Hjarnø", "Endelave", "Tunø", "Anholt", "Fur", 
                  "Jegindø", "Venø", rep("Samsø", 3), "Fanø", "Christiansø"), 
         kode_nabo = c("7456", "7275", "7274", 
                       "9323", "7325", "7325", "7483", 
                       "7469", "7521", "7491", "7611",
                       "7641", "7641", "7666", "9139", "7676", "7741", 
                       "7765", "7695", 
                       "7764", "7695",
                       "7801", "7962", "7962", "8097", "8231", "8581", 
                       "8657", "8854", 
                       "8097", "7268", "9066", 
                       "8906","7566"),  
         navn_nabo = c("Ferslev", "Tveje Merløse", "Sankt Nikolai",               # Orø
                       "Føllenslev-Særslev", "Magleby", "Magleby", "Stege", 
                       "Vordingborg", "Stubbekøbing", "Fanefjord", "Gundslev",    # Bogø
                       "Birket", "Birket", "Bandholm", "Fredens", "Sankt Nikolaj", 
                       "Rudkøbing",
                       "Avernakø", "Faaborg",                                     # Lyø
                       "Lyø", "Faaborg",                                          # Avernakø
                       "Assens", "Glud", "Glud", "Halling", "Grenaa", "Selde", 
                       "Søndbjerg", "Humlum", 
                       "Halling", "Vor Frue", "Nyvang",                           # Samsø
                       "Zions", "Gudhjem"))     


# Add island = whole parish connections (ferry + bridge) to adjacency matrix
island_nb <-
  island_con %>% 
  left_join(geo_tbl %>% select(kode, id_island = id), by = "kode") %>% 
  left_join(geo_tbl %>% select(kode, id_nb = id), by = c("kode_nabo" = "kode")) %>% 
  select(id_island, id_nb) %>% 
  as.matrix()

W_tmp <- nbs
W_tmp[island_nb] <- 1; W_tmp[island_nb[, c(2, 1)]] <- 1


## -----------------------------------------------------------------------------
## Adjacency matrix - step 3:
## Adding neighbors for parishes connected by bridge
## -----------------------------------------------------------------------------

# Other connections - including bridges from a list at vejdirektoratet.dk

# Add connections via Lillebæltsbroen, Storebæltsbroen, 
# Dronning Alexandrines Bro (Møn), Storstrømsbroen, Guldborgsundbroen, 
# Kong Frederik d. IX-broen, Svendborgsundbroen, Kronprins Frederiks bro, 
# Aggersundbroen, Hadsundbroen, Langelandsbroen, Legindvejlebroen, 
# Limfjordstunellen, Limfjordsbroen, Oddesundbroen, Sallingsundbroen, 
# Siøsundbroen, Vejlefjordbroen

# Kronprinsesse Marys bro and Nordhavnsvejtunellen not added (opened in 2019 and 2017)
# Alssundbroen, Karrebækdsmindebroen, Stege Storebro, Funder Ådal Bro not 
# necessary to add as the bridge is inside a parish
#
# Farøbroen, Masnedsundbroen are included via Bogø above
# so already included in W_old)

# Gudenåbroen, Kalvebodbroen, Frederikssundsvejtunellen, Lindenborg Pæledæk, 
# Næstved Svingbro, Silkeborg Langsø Broen, Silkeborgtunellen, Skive Ådal Broen,
# Sorterendebroen not necessary to add (parishes already bordering) 
# (some of these also quite new)

other_con <- 
  tibble(kode = c("7826", "7825", "7724", "7481", "7469", rep("7594", 2), "7578", 
                  "7741", "7677", "7418", "9113", "8358", "9006", "9005", 
                  "9173", "7692", "8693", "8375", "8376", "8854", "8546", "9092", 
                  "8665"),
         navn = c("Strib", "Vejlby", "Nyborg", "Kalvehave", "Vordingborg", 
                  rep("Majbølle", 2), "Nykøbing F", "Rudkøbing", "Sankt Jørgens", 
                  "Frederikssund", "Islebjerg", "Næsborg", "Dybbøl", "Egernsund", 
                  "Falslev-Vindblæs", "Bjerreby", "Lødderup", "Nørresundby", 
                  "Hvorup", "Humlum", "Glyngøre", "Nørremarks", "Stagstrup"), 
         kode_nabo = c(rep("9163", 2), "9070", "7483", "9318", "7575", "9318", 
                       "7596", "7692", "7690", rep("7453", 2), "8399", "8992", 
                       "9025", "9191", "7741", "8546", "8364", "8372", "8658", 
                       "8693", "7917", "8676"), 
         navn_nabo = c(rep("Lyng", 2), "Halsskov", "Stege", "Nordvestfalster", 
                       "Ønslev", "Nordvestfalster", "Toreby", "Bjerreby", 
                       "Bregninge", rep("Gerlev", 2), "Aggersborg", "Ulkebøl", 
                       "Rinkenæs", "Vive-Hadsund", "Rudkøbing", "Glyngøre", 
                       "Budolfi", "Vejgård", "Odby", "Lødderup", "Vinding", 
                       "Sundby"))



# Add bridges for any pair of parish
other_nb <- 
  other_con %>% 
  left_join(geo_tbl %>% select(kode, id_1 = id), by = "kode") %>% 
  left_join(geo_tbl %>% select(kode, id_2 = id), by = c("kode_nabo" = "kode")) %>% 
  select(id_1, id_2) %>% 
  as.matrix()

W <- W_tmp
W[other_nb] <- 1; W[other_nb[, c(2, 1)]] <- 1

## -----------------------------------------------------------------------------
##  Plot neighbours
## -----------------------------------------------------------------------------
nb <- mat2listw(W, style = "B")
nb_sf <- as(listw2lines(nb, coords = sp::coordinates(geo_sp)), "sf")
nb_sf <- st_set_crs(nb_sf, st_crs(geo_sogne))

geo_sogne %>% 
  ggplot() + 
  geom_sf(fill = "Dark Sea Green 3", color = "white", linewidth = 0.01) + 
  geom_sf(data = nb_sf, linewidth = 0.1) + 
  theme_void()

##------------------------------------------------------------------------------
## CAR model
##------------------------------------------------------------------------------

# Fit Leroux model (using default priors)
burnin    <- 100000
n_chains  <- 3
thin      <- 20
n_samples <- 6000

fit <-                                                                   
  CARBayes::S.CARleroux(
    formula  = women ~ offset(log(expected_women)), 
    data     = geo_sogne, 
    family   = "poisson", 
    W        = W, 
    thin     = thin,
    burnin   = burnin,
    n.chains = n_chains, 
    n.cores  = n_chains, 
    n.sample = burnin + (n_samples * thin / n_chains) 
  )                                                                             

# Check for residual autocorrelation
ape::Moran.I(residuals(fit), W)

# Smoothed IRR
fitted_list <- fit$samples$fitted
fitted      <- coda:::as.matrix.mcmc.list(fitted_list) 
irr         <- t(t(fitted) / geo_sogne$expected_women)

geo <- 
  geo_sogne %>% 
  mutate(
    # Posterior mean smoothed IRR
    irr_mean = apply(irr, 2, mean),  
    # 95% credible interval (2.5th and 97.5th percentiles) 
    irr_025  = apply(irr, 2, quantile, 0.025), 
    irr_975  = apply(irr, 2, quantile, 0.975), 
    # Posterior prob. of the smoothed IRR > 1+theta
    prob     = apply(irr, 2, function(x) sum(x >= 1 + theta)) / n_samples)





