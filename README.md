# TerraR: Australian Population Spike Maps

3D population spike maps of Australia's states and territories, rendered in R using rayshader.

<!-- Replace with an actual output image once rendered -->
<!-- ![NSW Population Spike Map](outputs/images/state-spike-map-New-South-Wales.png) -->

---

## Data sources

**[Kontur Population Dataset](https://data.humdata.org/dataset/kontur-population-australia)**
High-resolution 400m hexagonal population grid for Australia, produced by Kontur and distributed via the Humanitarian Data Exchange. Download the GeoPackage and place it at `data/raw/kontur_population_AU_20231101.gpkg`.

**[ABS State and Territory Boundaries](https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files)**
2021 Census digital boundary files from the Australian Bureau of Statistics. Included in `data/boundaries/`.

---

## How it works

The Kontur population hexagons are clipped to each state boundary using `sf::st_intersection()`, rasterized to a height matrix, log-transformed to keep smaller towns visible alongside major cities, and rendered as a 3D spike map with rayshader.

---

## How to reproduce

**Prerequisites**

- R (≥ 4.1)
- R packages: `tidyverse`, `sf`, `stars`, `rayshader`, `rgl`, `rayrender`
- The Kontur population GeoPackage (see Data sources above)

**Steps**

```r
# Step 1: Load, clip, and cache state population data
# Warning: the clipping step takes several hours on first run.
# Results are cached to data/processed/state_pops.rds.
source("R/01_prepare_data.R")

# Step 2: Render spike maps for all states and territories
source("R/02_render_maps.R")
```

Output images are saved to `outputs/images/`.

---

## Credits

The spike map technique is adapted from Milos Popovic's original tutorial
[*Making crisp spike maps with R*](https://github.com/milos-agathon/spike-maps) (2023).
The Australian adaptation adds a two-source data pipeline combining Kontur population data
with ABS state boundaries, per-state rasterization with aspect ratio preservation,
and tuned camera settings for each state and territory.
