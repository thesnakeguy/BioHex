# BioHex Studio

**An R Shiny application for hexagonal biodiversity mapping and joint species distribution modelling.**

BioHex Studio combines H3 hexagonal diversity mapping with Bayesian joint species distribution modelling (jSDM) in a single, browser-based interface. It is designed for ecologists and biodiversity scientists who want to go from a list of taxon names to publication-ready diversity maps and species co-occurrence models without writing code.

---

## Features

### 01 · Data acquisition
- Query GBIF by **species name, genus, family, order, suborder, or any higher taxon** (e.g. `Serpentes`, `Viperidae`, `Podarcis`) — higher taxa are resolved automatically via the GBIF backbone and downloaded in one request
- **Multi-taxon queries**: mix higher taxa and species names in the same search box, one per line or comma-separated
- Filter by year range and coordinate uncertainty
- Optional **CoordinateCleaner** pass to flag capitals, GBIF headquarters, institution centroids, and duplicate coordinates
- **CSV upload** fallback for pre-prepared datasets with custom column mapping
- Live occurrence preview map coloured by species

### 02 · Diversity mapping
- **H3 hexagonal grid** at any resolution (1 = continent-scale → 9 = neighbourhood-scale)
- Eight diversity metrics: Species Richness, Shannon H, Simpson D, ES (Hurlbert), MaxP, Hill 1, Hill 2, Hill ∞
- Interactive **Leaflet map** with five base tile options, eight colour palettes, adjustable opacity, and clickable hexagons
- **Click any hexagon** to see a ranked list of all species recorded within it
- Richness distribution histogram and lollipop chart of the top 10 richest hexagons (dot size = survey effort, colour = Shannon diversity)

### 03 · Joint Species Distribution Model (jSDM)
- One-click download of **WorldClim bioclim** (19 variables), elevation, built area, grassland, tree cover, human footprint, **surface water fraction**, and **wetland fraction** via `geodata`
- Optional **NDVI proxy** layer and **CORINE land cover** (EU only, upload as GeoTIFF)
- **Persistent layer cache** — layers are saved to `./env_cache/` next to `app.R` and loaded from disk on subsequent runs; the UI shows which layers are already cached
- **VIF stepwise filtering** removes collinear predictors; retained and dropped variables are shown in colour-coded panels
- **Manual predictor selection** — override VIF recommendations before fitting
- Bayesian probit jSDM fitted with MCMC via the [`jSDM`](https://ecology.ghislainv.fr/jSDM/) package
- Diagnostic plots:
  - MCMC **trace** and **posterior density** per species and parameter
  - **Residual correlation** matrix across species
  - **Species responses** (beta coefficients with 95% credible intervals)
  - **Predicted θ histogram** (occupancy probability distribution)
  - **Latent variable spatial maps** — site scores W₁ / W₂ mapped back onto the geographic raster grid
  - **Species biplot** — latent factor loadings (λ₁ vs λ₂) as arrows; species in the same direction and far from the origin are positively correlated in occurrence beyond what environment explains
  - **Deviance trace** with LOESS trend

### 04 · Export
- Filterable, sortable diversity table with CSV and Excel download
- **Map preview** before full-resolution export
- PNG map export (configurable size and DPI)
- GeoJSON hexagon polygons
- Cleaned occurrence CSV
- jSDM model object as `.rds`

---

## Installation

### 1. Clone the repository

```bash
git clone https://github.com/your-username/biohex-studio.git
cd biohex-studio
```

### 2. Install R dependencies

Run this once in R or RStudio:

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "shinyjs", "shinycssloaders",
  "h3", "sf", "terra", "dplyr", "tidyr", "purrr", "tibble",
  "rgbif", "CoordinateCleaner", "geodata", "usdm",
  "obisindicators", "jSDM", "coda",
  "ggplot2", "viridis", "leaflet", "leaflet.extras",
  "DT", "rnaturalearth", "rnaturalearthdata"
))
```

> **Note:** `obisindicators` may need to be installed from GitHub if not on CRAN:
> ```r
> remotes::install_github("iobis/obisindicators")
> ```

### 3. Set GBIF credentials

GBIF requires a free account for bulk downloads. Register at [gbif.org](https://www.gbif.org/user/profile) and add your credentials to your `.Renviron` file:

```
GBIF_USER=your_username
GBIF_PWD=your_password
GBIF_EMAIL=your_email@example.com
```

Open `.Renviron` with `usethis::edit_r_environ()`, add the three lines above, save, and restart R.

### 4. Launch the app

Open `app.R` in RStudio and click **Run App**, or run from the console:

```r
shiny::runApp("path/to/biohex-studio")
```

---

## Workflow

```
01 · Data  ──►  02 · Diversity Map  ──►  03 · jSDM  ──►  04 · Export
```

1. **Enter taxon names** — species, genus, family, order, or class. Higher taxa expand automatically.
2. **Set geographic scope** — select countries from the dropdown or paste a WKT polygon.
3. **Fetch from GBIF** — a single `occ_download` request handles all taxa. Wait for the confirmation message in the activity log.
4. **Build hex map** — choose H3 resolution and diversity metric. Click hexagons to explore species composition.
5. **Download env. layers** — select which layers to include. Cached layers load instantly; missing ones are downloaded automatically.
6. **Review VIF results** — adjust the predictor selection if needed, then run the jSDM.
7. **Explore model outputs** — convergence diagnostics, residual correlations, species responses, and spatial latent variable maps.
8. **Export** — download maps, spatial data, and the model object.

---

## Environmental layers

| Layer | Source | Notes |
|---|---|---|
| Bioclim (bio1–bio19) | WorldClim v2.1 | Temperature and precipitation variables |
| Elevation | SRTM via WorldClim | |
| Built area | ESA WorldCover | Fraction of built-up land per cell |
| Grassland | ESA WorldCover | |
| Tree cover | ESA WorldCover | |
| Human footprint | Wildlife Conservation Society | Year 2009 |
| Surface water | ESA WorldCover | Fraction of permanent/seasonal water |
| Wetland | ESA WorldCover | Fraction of wetland per cell |
| NDVI proxy (optional) | ESA WorldCover (bare soil inverse) | Selectable in UI |
| CORINE land cover (optional) | Copernicus / EEA | EU only; upload local GeoTIFF |

All base layers are downloaded at **5 arc-minute resolution** via the `geodata` package and cached locally to `./env_cache/`.

---

## Diversity metrics

| Metric | Description |
|---|---|
| Species richness | Number of unique species per hexagon |
| Shannon H | Information-theoretic diversity; sensitive to rare species |
| Simpson D | Probability that two random individuals are different species |
| ES (Hurlbert) | Expected number of species in a rarefied sample of *n* records |
| MaxP | Maximum occurrence probability |
| Hill 1 | exp(Shannon H) — effective number of species |
| Hill 2 | 1 / Simpson D — effective number of species |
| Hill ∞ | 1 / MaxP |

---

## jSDM — methodological notes

BioHex Studio fits a **Bayesian probit joint species distribution model** using the [`jSDM`](https://ecology.ghislainv.fr/jSDM/) R package (Warton et al. framework). Key details:

- **Sites** are raster grid cells from the environmental stack — not individual GBIF occurrence points. Each cell is assigned a presence (1) or absence (0) per species based on whether any occurrence falls within it.
- **Latent factors** (W₁, W₂, …) capture residual co-occurrence structure — the part of species associations not explained by the measured environmental predictors.
- **Species biplot**: arrows represent each species' loadings on the latent axes (λ₁, λ₂). Species pointing in the same direction and far from the origin tend to co-occur more than environment alone would predict.
- **Latent variable spatial maps**: site scores W₁ and W₂ are mapped back onto their geographic coordinates, revealing where unexplained environmental gradients are strongest.
- Predictions are **in-sample** (at the same grid cells used for fitting). Out-of-sample projection to new sites requires marginalising over the latent variables and is not currently implemented.

---

## File structure

```
biohex-studio/
├── app.R           # Main application (single-file Shiny app)
├── env_cache/      # Downloaded environmental layers (created automatically)
│   ├── wc2.1_5m_bio.tif
│   ├── wc2.1_5m_elev.tif
│   ├── built.tif
│   ├── grassland.tif
│   ├── trees.tif
│   ├── footprint.tif
│   ├── water.tif
│   └── wetland.tif
└── README.md
```

---

## Known limitations

- **GBIF download time**: bulk downloads for large higher-taxon queries (e.g. all of *Serpentes* globally) can take several minutes. The activity log shows progress.
- **CORINE upload**: maximum upload size is set to 500 MB. The CORINE GeoTIFF must cover the study area; cells outside CORINE coverage will be `NA` in the model.
- **jSDM computation time**: MCMC is computationally intensive. For exploratory work, use fewer iterations (e.g. 5000 iterations, 1000 burn-in). For publication, increase to 50 000+ with thinning.
- **In-sample predictions only**: the jSDM currently predicts occupancy at observed grid cells, not at new locations.
- **H3 resolution and GBIF data**: at fine resolutions (≥ 7), many hexagons will contain only 1–2 records. Diversity metrics based on abundance (Shannon, Simpson, ES) become unreliable at very low sample sizes.

---

## Dependencies

| Package | Purpose |
|---|---|
| `shiny`, `bslib`, `bsicons`, `shinyjs`, `shinycssloaders` | UI framework |
| `h3` | Uber H3 hexagonal grid |
| `sf`, `terra` | Spatial data handling |
| `dplyr`, `tidyr`, `purrr`, `tibble` | Data wrangling |
| `rgbif` | GBIF occurrence downloads |
| `CoordinateCleaner` | Coordinate quality filtering |
| `geodata` | Environmental raster downloads |
| `usdm` | VIF-based variable selection |
| `obisindicators` | Diversity metric calculation |
| `jSDM`, `coda` | Joint species distribution modelling |
| `ggplot2`, `viridis`, `leaflet`, `leaflet.extras` | Visualisation |
| `DT` | Interactive data tables |
| `rnaturalearth`, `rnaturalearthdata` | Country boundaries |

---

## Citation

If you use BioHex Studio in published work, please cite the underlying methods packages:

- **jSDM**: Warton D.I. et al. (2015). So many variables: joint modeling in community ecology. *Trends in Ecology & Evolution*, 30(12), 766–779.
- **rgbif**: Chamberlain S. et al. (2024). rgbif: Interface to the Global Biodiversity Information Facility API. R package. <https://CRAN.R-project.org/package=rgbif>
- **H3**: Brodsky N. (2018). H3: Uber's Hexagonal Hierarchical Spatial Index. <https://h3geo.org>
- **obisindicators**: Provoost P. & Bosch S. (2022). obisindicators: Calculate diversity indicators. R package. <https://github.com/iobis/obisindicators>

---

## License

MIT License — see `LICENSE` for details.

---

## Contributing

Pull requests are welcome. For major changes please open an issue first to discuss what you would like to change.

---

*Built with R, Shiny, and a lot of hexagons.*
