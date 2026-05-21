# ============================================================
#  BioHex Studio — v3
#  H3 Diversity Explorer + jSDM · Fixed edition
# ============================================================
# Required packages:
#   shiny bslib bsicons shinyjs shinycssloaders
#   h3 sf terra dplyr tidyr purrr tibble
#   rgbif CoordinateCleaner geodata usdm
#   obisindicators jSDM coda
#   ggplot2 viridis leaflet leaflet.extras DT rnaturalearth
# (base64enc is NOT required)
# ============================================================

# ── Startup dependency check ────────────────────────────────
# If the app fails to source, run this in your console to see the real error:
#   tryCatch(source("app.R"), error = function(e) message("REAL ERROR: ", e$message))
#
# Install ALL required packages with:
#   install.packages(c("shiny","bslib","bsicons","shinyjs","shinycssloaders",
#     "h3","sf","terra","dplyr","tidyr","purrr","tibble",
#     "rgbif","CoordinateCleaner","geodata","usdm",
#     "obisindicators","jSDM","coda",
#     "ggplot2","viridis","leaflet","leaflet.extras","DT",
#     "rnaturalearth","rnaturalearthdata"))
# ─────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(bsicons)
  library(shinyjs); library(shinycssloaders)
  library(h3); library(sf); library(terra)
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(rgbif); library(CoordinateCleaner)
  library(geodata); library(usdm)
  library(obisindicators); library(jSDM); library(coda)
  library(ggplot2); library(viridis)
  library(leaflet); library(leaflet.extras)
  library(DT); library(rnaturalearth); library(rnaturalearthdata)
})

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[[1]])) a else b

# ── Persistent env layer cache ───────────────────────────────
# Layers are saved in ./env_cache/ next to app.R so they only
# need to be downloaded once across sessions.
ENV_CACHE_DIR <- file.path(dirname(normalizePath(
  if (nchar(Sys.getenv("SHINY_PORT")) > 0) "/app/app.R"
  else ifelse(interactive(), rstudioapi::getActiveDocumentContext()$path, "app.R")
)), "env_cache")
if (!dir.exists(ENV_CACHE_DIR)) dir.create(ENV_CACHE_DIR, recursive=TRUE)

# Which base layer files exist in the cache?
layer_cache_files <- list(
  bio       = file.path(ENV_CACHE_DIR, "wc2.1_5m_bio.tif"),
  elev      = file.path(ENV_CACHE_DIR, "wc2.1_5m_elev.tif"),
  built     = file.path(ENV_CACHE_DIR, "built.tif"),
  grassland = file.path(ENV_CACHE_DIR, "grassland.tif"),
  trees     = file.path(ENV_CACHE_DIR, "trees.tif"),
  footprint = file.path(ENV_CACHE_DIR, "footprint.tif"),
  ndvi      = file.path(ENV_CACHE_DIR, "ndvi.tif"),
  corine    = file.path(ENV_CACHE_DIR, "corine.tif"),
  water     = file.path(ENV_CACHE_DIR, "water.tif"),
  wetland   = file.path(ENV_CACHE_DIR, "wetland.tif")
)

layers_cached <- function() {
  names(Filter(file.exists, layer_cache_files))
}

layers_missing <- function(requested) {
  requested[!requested %in% layers_cached()]
}

# ── Colour palette ──────────────────────────────────────────
D  <- "#070b12"; CARD <- "#0c1220"; PANEL <- "#0f1828"
BRD <- "#1a2740"; ACC <- "#22d3ee"; VIO <- "#a78bfa"
GRN <- "#34d399"; AMB <- "#fbbf24"; RED <- "#f87171"
TXT <- "#c8d6e8"; MUT <- "#4a6080"

# ── Constants ───────────────────────────────────────────────
PALETTE_CH <- c("Viridis"="viridis","Magma"="magma","Inferno"="inferno",
                "Plasma"="plasma","Cividis"="cividis","Mako"="mako",
                "Rocket"="rocket","Turbo"="turbo")
ESTIMATOR_CH <- c("Species Richness"="richness","Shannon"="shannon_diversity",
                   "Simpson"="simpson_diversity","ES (Hurlbert)"="ES","MaxP"="maxp",
                   "Hill 1"="hill_1","Hill 2"="hill_2","Hill inf"="hill_inf")
LEG <- list(richness="Species richness",shannon_diversity="Shannon H",
            simpson_diversity="Simpson D",ES="ES (Hurlbert)",maxp="MaxP",
            hill_1="Hill 1",hill_2="Hill 2",hill_inf="Hill inf")
TILE_CH <- c("CartoDB Dark"="CartoDB.DarkMatter","CartoDB Light"="CartoDB.Positron",
             "OpenStreetMap"="OpenStreetMap","Esri Imagery"="Esri.WorldImagery",
             "Esri NatGeo"="Esri.NatGeoWorldMap")

# ── CSS (pure paste0 — no glue, no curly-brace conflicts) ───
APP_CSS <- paste0("
@import url('https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;600;700&family=JetBrains+Mono:wght@300;400;500&display=swap');
:root{--d:",D,";--card:",CARD,";--panel:",PANEL,";--brd:",BRD,
      ";--acc:",ACC,";--vio:",VIO,";--grn:",GRN,";--amb:",AMB,
      ";--red:",RED,";--txt:",TXT,";--mut:",MUT,";}
*,*::before,*::after{box-sizing:border-box;}
body,.bslib-page-navbar{background:var(--d)!important;color:var(--txt)!important;
  font-family:'JetBrains Mono',monospace!important;font-size:13px;}
.navbar{background:rgba(7,11,18,.97)!important;border-bottom:1px solid var(--brd)!important;
  backdrop-filter:blur(16px);padding:0 1.5rem!important;}
.navbar-brand{font-family:'Space Grotesk',sans-serif!important;font-weight:700;
  font-size:1.1rem!important;color:var(--acc)!important;letter-spacing:-.02em;
  display:flex;align-items:center;gap:8px;}
.navbar-brand::before{content:'';display:inline-block;width:10px;height:10px;
  background:var(--acc);clip-path:polygon(50% 0%,100% 25%,100% 75%,50% 100%,0% 75%,0% 25%);}
.nav-link{font-family:'Space Grotesk',sans-serif!important;font-size:.74rem!important;
  font-weight:600!important;letter-spacing:.07em!important;text-transform:uppercase!important;
  color:var(--mut)!important;padding:.9rem .85rem!important;transition:color .2s;
  border-bottom:2px solid transparent!important;}
.nav-link:hover{color:var(--txt)!important;}
.nav-link.active{color:var(--acc)!important;border-bottom-color:var(--acc)!important;}
.card{background:var(--card)!important;border:1px solid var(--brd)!important;
  border-radius:10px!important;box-shadow:0 4px 20px rgba(0,0,0,.4);}
.card-header{background:transparent!important;border-bottom:1px solid var(--brd)!important;
  padding:.55rem 1rem!important;font-family:'Space Grotesk',sans-serif!important;
  font-size:.68rem!important;font-weight:700!important;letter-spacing:.1em!important;
  text-transform:uppercase!important;color:var(--mut)!important;}
.bslib-sidebar-layout>.sidebar{background:var(--panel)!important;
  border-right:1px solid var(--brd)!important;padding:1rem!important;}
label,.form-label{font-family:'Space Grotesk',sans-serif!important;font-size:.67rem!important;
  font-weight:600!important;letter-spacing:.08em!important;text-transform:uppercase!important;
  color:var(--mut)!important;margin-bottom:.25rem!important;}
.form-control,.form-select{background:var(--d)!important;border:1px solid var(--brd)!important;
  color:var(--txt)!important;border-radius:7px!important;
  font-family:'JetBrains Mono',monospace!important;font-size:.82rem!important;}
.form-control:focus,.form-select:focus{border-color:var(--acc)!important;
  box-shadow:0 0 0 3px rgba(34,211,238,.12)!important;outline:none!important;}
textarea.form-control{min-height:80px;resize:vertical;}
.selectize-input{background:var(--d)!important;border:1px solid var(--brd)!important;
  color:var(--txt)!important;border-radius:7px!important;
  font-family:'JetBrains Mono',monospace!important;font-size:.82rem!important;}
.selectize-dropdown{background:var(--card)!important;border:1px solid var(--brd)!important;
  color:var(--txt)!important;font-size:.82rem!important;}
.selectize-dropdown .active{background:var(--brd)!important;}
.btn{font-family:'Space Grotesk',sans-serif!important;font-weight:600!important;
  font-size:.74rem!important;letter-spacing:.07em!important;border-radius:7px!important;
  padding:.42rem 1rem!important;transition:all .18s ease!important;border:none!important;}
.btn:hover{filter:brightness(1.18);transform:translateY(-1px);box-shadow:0 4px 12px rgba(0,0,0,.3);}
.btn:active{transform:translateY(0);filter:brightness(.95);}
.btn-cyan{background:var(--acc)!important;color:var(--d)!important;}
.btn-green{background:var(--grn)!important;color:var(--d)!important;}
.btn-violet{background:var(--vio)!important;color:white!important;}
.btn-ghost{background:transparent!important;border:1px solid var(--brd)!important;color:var(--mut)!important;}
.btn-ghost:hover{border-color:var(--acc)!important;color:var(--acc)!important;}
.btn-primary{background:var(--acc)!important;color:var(--d)!important;}
.btn-success{background:var(--grn)!important;color:var(--d)!important;}
.irs--shiny .irs-bar{background:var(--acc)!important;border-color:var(--acc)!important;}
.irs--shiny .irs-handle{background:var(--acc)!important;border-color:var(--acc)!important;}
.irs--shiny .irs-single{background:var(--acc)!important;color:var(--d)!important;font-size:.74rem;}
.irs--shiny .irs-min,.irs--shiny .irs-max{color:var(--mut)!important;font-size:.7rem;}
/* Value boxes — force light text on dark bg */
.value-box{border:1px solid var(--brd)!important;background:var(--card)!important;border-radius:10px!important;}
.value-box-title{font-family:'Space Grotesk',sans-serif!important;font-size:.63rem!important;
  font-weight:700!important;letter-spacing:.1em!important;text-transform:uppercase!important;
  color:",TXT,"!important;}
.value-box-value{font-family:'Space Grotesk',sans-serif!important;font-weight:700!important;
  color:",ACC,"!important;font-size:1.6rem!important;}
.value-box .bi{color:",ACC,"!important;opacity:.8;}
/* Badges */
.badge{font-family:'Space Grotesk',sans-serif!important;font-size:.64rem!important;
  font-weight:600!important;letter-spacing:.05em!important;padding:.28em .72em!important;
  border-radius:999px!important;}
.badge-cyan{background:",ACC,";color:",D,";}
.badge-green{background:",GRN,";color:",D,";}
.badge-violet{background:",VIO,";color:white;}
.badge-amber{background:",AMB,";color:",D,";}
.badge-red{background:",RED,";color:white;}
.badge-muted{background:",BRD,";color:",MUT,";}
/* Log */
.log-box{background:var(--d);border:1px solid var(--brd);border-radius:8px;
  padding:9px 13px;font-family:'JetBrains Mono',monospace;font-size:.74rem;
  color:var(--grn);max-height:175px;overflow-y:auto;line-height:1.7;}
/* Section divider */
.sdiv{display:flex;align-items:center;gap:8px;margin:13px 0 7px;}
.sdiv span{font-family:'Space Grotesk',sans-serif;font-size:.6rem;font-weight:700;
  letter-spacing:.12em;text-transform:uppercase;color:var(--mut);white-space:nowrap;}
.sdiv::before,.sdiv::after{content:'';flex:1;height:1px;background:var(--brd);}
/* VIF table */
.vif-retained{color:",GRN,";font-weight:600;}
.vif-dropped{color:",RED,";text-decoration:line-through;opacity:.6;}
/* Notifications */
.shiny-notification{background:var(--panel)!important;color:var(--txt)!important;
  border:1px solid var(--brd)!important;border-left:3px solid var(--acc)!important;
  border-radius:8px!important;font-family:'JetBrains Mono',monospace!important;font-size:.78rem!important;}
.shiny-notification-error{border-left-color:",RED,"!important;}
.shiny-notification-warning{border-left-color:",AMB,"!important;}
/* DataTables */
.dataTables_wrapper{color:var(--mut)!important;}
table.dataTable{background:var(--card)!important;color:var(--txt)!important;}
table.dataTable thead th{background:var(--d)!important;color:var(--mut)!important;
  border-bottom:1px solid var(--brd)!important;font-family:'Space Grotesk',sans-serif!important;
  font-size:.66rem!important;letter-spacing:.08em!important;text-transform:uppercase!important;}
table.dataTable tbody tr{background:var(--card)!important;}
table.dataTable tbody tr:hover{background:var(--panel)!important;}
table.dataTable tbody td{border-top:1px solid var(--brd)!important;}
.checkbox label,.radio label{color:var(--txt)!important;
  font-family:'JetBrains Mono',monospace!important;font-size:.8rem!important;}
#hex_map,#occ_map{border-radius:10px;overflow:hidden;}
::-webkit-scrollbar{width:4px;height:4px;}
::-webkit-scrollbar-track{background:var(--d);}
::-webkit-scrollbar-thumb{background:var(--brd);border-radius:99px;}
::-webkit-scrollbar-thumb:hover{background:var(--mut);}
.shiny-spinner-output-container .load-container{background:transparent!important;}
/* Export preview */
#export_preview img{border-radius:8px;max-width:100%;margin-top:8px;}
")

# ══════════════════════════════════════════════════════════════
# HELPER FUNCTIONS
# ══════════════════════════════════════════════════════════════

# resolve_taxa ---------------------------------------------------------
# Returns list(keys, labels). Uses name_backbone first; falls back to
# name_suggest for single-word queries that didn't match a higher taxon.
# All keys go into one occ_download — GBIF expands higher taxa server-side.
resolve_taxa <- function(q) {
  raw <- trimws(strsplit(q, "[,\n]+")[[1]])
  raw <- raw[nchar(raw) > 0]

  HIGHER_RANKS <- c("kingdom","phylum","class","order","suborder",
                    "superfamily","family","subfamily","tribe","genus")
  keys   <- integer(0)
  labels <- character(0)

  for (x in raw) {
    key  <- NA_integer_
    rank <- NA_character_
    lbl  <- x

    # name_backbone
    safe_backbone <- function(x, tries = 5) {
      for(i in seq_len(tries)) {
        out <- try(rgbif::name_backbone(x), silent = TRUE)
        if(!inherits(out, "try-error")) return(out)
        Sys.sleep(1)
      }
      stop("Failed after retries")
    } # To circumvent "Error in the HTTP2 framing layer [api.gbif.org]"
    
    bb <- tryCatch(safe_backbone(x),
                   error=function(e) NULL)
    if (!is.null(bb) && !isTRUE(bb$matchType == "NONE")) {
      rank <- tolower(bb$rank %||% "species")
      key  <- as.integer(bb$usageKey)
      lbl  <- bb$canonicalName %||% x
      message(sprintf("  backbone: '%s' -> rank=%s key=%s", x, rank, key))
    }

    if (is.na(key)) {
      message(sprintf("  WARNING: could not resolve '%s' — skipping.", x))
      next
    }
    keys   <- c(keys,   key)
    labels <- c(labels, sprintf("%s [%s]", lbl, rank))
  }

  list(keys=unique(keys), labels=labels[!duplicated(keys)])
}

# download_gbif ---------------------------------------------------------
# Single occ_download for ALL keys (species and higher taxon alike).
# GBIF expands higher-taxon keys to all species underneath automatically.
# We do NOT use occ_search: it fails with 500 errors on large WKT polygons.
download_gbif <- function(taxa_resolved, wkt, yr_min, yr_max, max_unc) {
  keys <- taxa_resolved$keys
  if (!length(keys)) return(NULL)

  message(sprintf("  occ_download: %d key(s): %s",
                  length(keys), paste(keys, collapse=", ")))
  tryCatch({
    d <- rgbif::occ_download(
      rgbif::pred_in("taxonKey", keys),
      rgbif::pred("hasCoordinate", TRUE),
      rgbif::pred("hasGeospatialIssue", FALSE),
      rgbif::pred_gte("year", yr_min),
      rgbif::pred_lte("year", yr_max),
      rgbif::pred_within(wkt),
      rgbif::pred_lt("coordinateUncertaintyInMeters", max_unc),
      format = "SIMPLE_CSV"
    )
    rgbif::occ_download_wait(d)
    rgbif::occ_download_get(d) %>% rgbif::occ_download_import()
  }, error=function(e) {
    message("  occ_download failed: ", e$message)
    NULL
  })
}


clean_occ <- function(df) {
  df2 <- df %>%
    select(species, decimalLongitude, decimalLatitude, year, basisOfRecord) %>%
    filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%
    distinct(decimalLongitude, decimalLatitude, .keep_all=TRUE)
  flags <- tryCatch(
    CoordinateCleaner::clean_coordinates(x=df2, lon="decimalLongitude",
      lat="decimalLatitude", species="species",
      tests=c("capitals","centroids","duplicates","equal","gbif","institutions","zeros")),
    error=function(e){df2$.summary<-TRUE; df2})
  df2[flags$.summary,,drop=FALSE]
}

countries_to_wkt <- function(cvec) {
  w <- ne_countries(scale="medium", returnclass="sf")
  sel <- w[w$name %in% cvec,]
  if (!nrow(sel)) stop("No country polygons found.")
  st_as_text(st_union(sel))
}

build_h3_polys <- function(cd) {
  h3_to_geo_boundary_sf(cd$cell) %>%
    mutate(cell=cd$cell, survey_effort=cd$n, richness=cd$sp,
           shannon_diversity=cd$shannon, simpson_diversity=cd$simpson,
           ES=cd$es, maxp=cd$maxp, hill_1=cd$hill_1, hill_2=cd$hill_2, hill_inf=cd$hill_inf) %>%
    st_wrap_dateline(options=c("WRAPDATELINE=YES","DATELINEOFFSET=179.9999"))
}

# Download and crop a single named layer — returns NULL on error
.fetch_layer <- function(name, ext_obj, wdir) {
  tryCatch({
    rast_raw <- switch(name,
      bio      = worldclim_global(var="bio", res=5, path=wdir),
      elev     = elevation_global(res=5, path=wdir),
      built    = landcover(var="built",     path=wdir),
      grassland= landcover(var="grassland", path=wdir),
      trees    = landcover(var="trees",     path=wdir),
      footprint= footprint(year=2009,       path=wdir),
      ndvi     = {
        # geodata::landcover "bare" is the closest proxy; real NDVI needs MOD13 tiles
        # Use landcover snow as a placeholder — swap for your preferred NDVI source
        message("NDVI: using EVI proxy from geodata::landcover('bare') as stand-in.")
        landcover(var="bare", path=wdir)
      },
      NULL
    )
    if (is.null(rast_raw)) return(NULL)
    crop(rast_raw, ext_obj)
  }, error = function(e) { message("Layer failed: ", name, " — ", e$message); NULL })
}

get_env_stack <- function(ext_obj, extra=character(0), wdir=ENV_CACHE_DIR) {
  stack_list <- list()

  # Download (or load from cache), crop and resample one layer group
  load_layer <- function(key, dl_fn, method="bilinear", cache_file=layer_cache_files[[key]]) {
    if (file.exists(cache_file)) {
      message("  [cache] Loading ", key)
      r <- tryCatch(rast(cache_file), error=function(e) NULL)
    } else {
      message("  [download] Fetching ", key)
      r <- tryCatch(dl_fn(), error=function(e) {
        message("  Skipped (", key, "): ", e$message); NULL })
      if (!is.null(r)) {
        tryCatch(writeRaster(r, cache_file, overwrite=TRUE),
                 error=function(e) message("  Cache write failed: ", e$message))
      }
    }
    r
  }

  # Reference grid: bioclim (always first)
  bio_raw <- load_layer("bio", function() worldclim_global(var="bio", res=5, path=wdir))
  if (is.null(bio_raw)) stop("Failed to obtain bioclim reference layer.")
  ref_full <- bio_raw          # keep full global for cache
  ref      <- crop(bio_raw, ext_obj)
  stack_list[["bio"]] <- ref

  add_layer <- function(key, dl_fn, method="bilinear") {
    r_raw <- load_layer(key, dl_fn)
    if (is.null(r_raw)) return()
    r_crop <- tryCatch(crop(r_raw, ext_obj), error=function(e) NULL)
    if (is.null(r_crop)) return()
    r_res  <- tryCatch(resample(r_crop, ref[[1]], method=method), error=function(e) NULL)
    if (!is.null(r_res)) stack_list[[key]] <<- r_res
  }

  add_layer("elev",      function() elevation_global(res=5, path=wdir), "bilinear")
  add_layer("built",     function() landcover(var="built",     path=wdir), "near")
  add_layer("grassland", function() landcover(var="grassland", path=wdir), "near")
  add_layer("trees",     function() landcover(var="trees",     path=wdir), "near")
  add_layer("footprint", function() footprint(year=2009,       path=wdir), "near")
  add_layer("water",     function() landcover(var="water",     path=wdir), "near")
  add_layer("wetland",   function() landcover(var="wetland",   path=wdir), "near")

  if ("ndvi" %in% extra) {
    add_layer("ndvi", function() landcover(var="bare", path=wdir), "near")
  }
  if ("corine" %in% extra) {
    message("CORINE: must be uploaded by user (not auto-downloadable).")
  }

  if (!length(stack_list)) stop("No layers could be loaded.")
  terra::rast(stack_list)
}

run_vif <- function(env_rast, thr=10) {
  edf     <- as.data.frame(env_rast, xy=FALSE, na.rm=TRUE)
  vif_res <- usdm::vifstep(edf, th=thr)
  retained <- vif_res@results$Variables
  dropped  <- setdiff(names(env_rast), retained)
  list(rast=env_rast[[retained]], retained=retained, dropped=dropped,
       vif_table=as.data.frame(vif_res@results))
}

# prepare_jsdm ──────────────────────────────────────────────
# site_size_m controls the aggregation grid for jSDM "sites".
# If NULL / 0, uses the native env raster resolution (~9 km at 5').
# Otherwise, builds a custom grid of approximately site_size_m metres
# (converted to degrees at the study centroid) and aggregates both
# env values (mean) and occurrences (presence/absence) into it.
prepare_jsdm <- function(occ_df, env_rast, site_size_m=NULL) {

  use_custom_grid <- !is.null(site_size_m) && site_size_m > 0

  if (use_custom_grid) {
    # Convert metres to approximate degrees (WGS84)
    # 1 degree latitude ≈ 111 000 m everywhere
    # 1 degree longitude ≈ 111 000 * cos(lat) — use study centroid
    ext_r   <- ext(env_rast)
    lat_ctr <- (ext_r$ymin + ext_r$ymax) / 2
    deg_lat <- site_size_m / 111000
    deg_lon <- site_size_m / (111000 * cos(lat_ctr * pi / 180))

    message(sprintf("Custom site grid: %.0f m → %.4f° lat × %.4f° lon",
                    site_size_m, deg_lat, deg_lon))

    # Build a SpatRaster at the custom resolution covering env extent
    site_rast <- rast(ext=ext_r, resolution=c(deg_lon, deg_lat),
                      crs=crs(env_rast))
    site_rast[] <- seq_len(ncell(site_rast))
    names(site_rast) <- "site_id"

    # Aggregate env layers to the coarser grid (mean per site cell)
    env_agg <- resample(env_rast, site_rast, method="average")
    grid_rast <- site_rast
  } else {
    env_agg   <- env_rast
    grid_rast <- env_rast[[1]]
    grid_rast[] <- seq_len(ncell(grid_rast))
    names(grid_rast) <- "site_id"
  }

  # ── 1. Env data frame (non-NA cells only) ───────────────────
  edf      <- as.data.frame(env_agg, xy=TRUE, na.rm=TRUE)
  env_vars <- names(env_agg)

  safe_names <- make.names(env_vars, unique=TRUE)
  names(edf)[match(env_vars, names(edf))] <- safe_names
  env_vars <- safe_names

  ok_cols  <- safe_names[sapply(safe_names, function(v) !all(is.na(edf[[v]])))]
  if (!length(ok_cols)) stop("All environmental columns are NA after masking.")
  edf      <- edf[, c("x", "y", ok_cols), drop=FALSE]
  env_vars <- ok_cols

  # ── 2. Site IDs for each env row ────────────────────────────
  edf$cell_id <- cellFromXY(env_agg, as.matrix(edf[, c("x","y")]))

  # ── 3. Presence-absence matrix ───────────────────────────────
  spp <- sort(unique(occ_df$species[!is.na(occ_df$species) & occ_df$species != ""]))
  pa  <- matrix(0L, nrow=nrow(edf), ncol=length(spp), dimnames=list(NULL, spp))

  for (sp in spp) {
    pts <- occ_df[occ_df$species == sp,
                  c("decimalLongitude","decimalLatitude"), drop=FALSE]
    pts <- pts[complete.cases(pts), , drop=FALSE]
    if (!nrow(pts)) next
    # Map occurrence points into the (possibly coarser) site grid
    occ_cells <- cellFromXY(env_agg, as.matrix(pts))
    occ_cells <- unique(occ_cells[!is.na(occ_cells)])
    if (!length(occ_cells)) next
    hit_rows  <- which(edf$cell_id %in% occ_cells)
    if (length(hit_rows)) pa[hit_rows, sp] <- 1L
  }

  # Drop species with zero presences
  n_pres  <- colSums(pa)
  keep_sp <- names(n_pres[n_pres > 0])
  if (!length(keep_sp)) stop("No species have any presences in the site grid.")
  dropped_sp <- setdiff(spp, keep_sp)
  if (length(dropped_sp))
    message("Dropping ", length(dropped_sp), " species with 0 presences: ",
            paste(head(dropped_sp, 5), collapse=", "),
            if (length(dropped_sp)>5) "..." else "")
  pa  <- pa[, keep_sp, drop=FALSE]
  spp <- keep_sp

  n_sites   <- nrow(edf)
  site_desc <- if (use_custom_grid)
    sprintf("%.0f m custom grid (%d sites)", site_size_m, n_sites)
  else
    sprintf("native env resolution (%d sites)", n_sites)
  message("Site grid: ", site_desc)

  list(pa        = pa,
       env       = as.matrix(edf[, env_vars, drop=FALSE]),
       coords    = edf[, c("x","y")],
       env_vars  = env_vars,
       species   = spp,
       cell_id   = edf$cell_id,
       site_desc = site_desc)
}

run_jsdm_model <- function(jd, n_iter, n_burnin, n_thin, n_latent) {
  # Scale env matrix and strip ALL attributes that confuse jSDM
  env_sc  <- scale(jd$env)
  env_df  <- as.data.frame(env_sc)
  # make.names again in case scale() altered anything
  names(env_df) <- make.names(names(env_df), unique=TRUE)

  # Verify PA matrix is integer with no NA
  pa <- jd$pa
  storage.mode(pa) <- "integer"
  if (any(is.na(pa))) pa[is.na(pa)] <- 0L

  # Remove any sites that are all-NA in env (safety net)
  ok_sites <- complete.cases(env_df)
  if (!all(ok_sites)) {
    message("Removing ", sum(!ok_sites), " sites with NA env values before jSDM.")
    env_df <- env_df[ok_sites, , drop=FALSE]
    pa     <- pa[ok_sites,   , drop=FALSE]
  }

  jSDM::jSDM_binomial_probit(
    presence_data = pa,
    site_formula  = ~ .,
    site_data     = env_df,
    burnin        = n_burnin,
    mcmc          = n_iter,
    thin          = n_thin,
    n_latent      = n_latent,
    site_effect   = "random",
    verbose       = 0)
}

gg_dark <- function(bs=11) {
  theme_minimal(base_size=bs, base_family="JetBrains Mono") +
    theme(plot.background  = element_rect(fill=CARD, color=NA),
          panel.background = element_rect(fill=CARD, color=NA),
          panel.grid.major = element_line(color=BRD, linewidth=.3),
          panel.grid.minor = element_blank(),
          text             = element_text(color=TXT),
          axis.text        = element_text(color=MUT, size=9),
          axis.title       = element_text(color=MUT, size=9),
          plot.title       = element_text(color=TXT, size=11, family="Space Grotesk", face="bold"),
          plot.subtitle    = element_text(color=MUT, size=8),
          legend.background= element_rect(fill=CARD, color=NA),
          legend.text      = element_text(color=MUT, size=8),
          legend.title     = element_text(color=MUT, size=8))
}

sdiv <- function(lbl) div(class="sdiv", tags$span(lbl))

badge_status <- function(st) {
  switch(st,
    idle    = tags$span(class="badge badge-muted", "No data"),
    running = tags$span(class="badge badge-amber", "\u29d7 Running\u2026"),
    done    = tags$span(class="badge badge-green", "\u2713 Done"),
    error   = tags$span(class="badge badge-red",   "\u2715 Error"))
}

# ══════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════
ui <- page_navbar(
  title="BioHex Studio", window_title="BioHex Studio", fillable=TRUE,
  theme=bs_theme(version=5, bg=D, fg=TXT, primary=ACC, secondary=PANEL,
                 base_font=font_google("JetBrains Mono"),
                 heading_font=font_google("Space Grotesk")),
  useShinyjs(),
  tags$head(
    tags$style(HTML(APP_CSS)),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('log', function(m){
        var b=document.getElementById(m.id);
        if(b){b.innerHTML+=m.html+'<br>';b.scrollTop=b.scrollHeight;}
      });
      Shiny.addCustomMessageHandler('clearLog', function(m){
        var b=document.getElementById(m.id); if(b) b.innerHTML='';
      });
    "))
  ),

  # ── TAB 1: Data ───────────────────────────────────────────
  nav_panel("01 \u00b7 Data", icon=bs_icon("cloud-download"),
    tags$style(HTML("
      .scroll-legend {
        max-height: 250px;
        overflow-y: auto;
      }
    ")),
    layout_sidebar(fillable=TRUE,
      sidebar=sidebar(width=310, bg=PANEL,

        sdiv("Taxon query"),
        textAreaInput("taxon_query", "Species / higher taxon",
          value="Testudines\nCaudata\nSquamata", rows=5,
          placeholder="One per line or comma-separated\nHigher taxa auto-expand to species"),

        sdiv("Geographic scope"),
        radioButtons("geo_mode", NULL,
          choices=c("Countries"="country","Custom WKT"="wkt"), selected="country", inline=TRUE),
        conditionalPanel("input.geo_mode=='country'",
          selectizeInput("countries", NULL, multiple=TRUE, selected="Italy",
            choices=sort(tryCatch(ne_countries(scale="medium",returnclass="sf")$name, error=function(e) "Italy")),
            options=list(placeholder="Select countries\u2026"))),
        conditionalPanel("input.geo_mode=='wkt'",
          textAreaInput("wkt_input", NULL, rows=3, placeholder="POLYGON((\u2026))")),

        sdiv("Filters"),
        fluidRow(
          column(6, numericInput("yr_min","Year from",2000,1800,2026)),
          column(6, numericInput("yr_max","Year to",  2024,1800,2026))),
        numericInput("max_unc","Max coord. uncertainty (m)",1500,100,50000,100),
        checkboxInput("run_cc","Run CoordinateCleaner",TRUE),

        sdiv("GBIF credentials"),
        tags$small(style=paste0("color:",MUT), "Or set GBIF_USER/PWD/EMAIL in .Renviron"),
        br(),
        textInput("gbif_user","User",   Sys.getenv("GBIF_USER")),
        passwordInput("gbif_pwd","Password",Sys.getenv("GBIF_PWD")),
        textInput("gbif_email","Email", Sys.getenv("GBIF_EMAIL")),
        br(),
        actionButton("btn_fetch","Fetch from GBIF",class="btn-cyan w-100",icon=icon("download")),

        sdiv("Or upload CSV"),
        fileInput("csv_upload","Upload CSV file",accept=".csv",
                  buttonLabel="Browse\u2026", placeholder="No file selected"),
        textInput("csv_sp",  "Species column name",    "species"),
        textInput("csv_lon", "Longitude column name", "decimalLongitude"),
        textInput("csv_lat", "Latitude column name",  "decimalLatitude"),
        actionButton("btn_load_csv","Load CSV",class="btn-ghost w-100",icon=icon("file-csv"))
      ),

      layout_columns(col_widths=c(4,4,4),
        uiOutput("kpi_records"), uiOutput("kpi_species"), uiOutput("kpi_status")),
      card(card_header(bs_icon("terminal")," Activity log"),
           div(id="data_log", class="log-box", "Waiting\u2026")),
      card(full_screen=TRUE,
           card_header(
             class="d-flex justify-content-between align-items-center",
             span(bs_icon("map")," Occurrence preview"),
             div(style="display:flex;align-items:center;gap:8px;",
               radioButtons("occ_color_by","",
                 choices=c("Species"="species","Genus"="genus"),
                 selected="species",inline=TRUE),
               downloadButton("dl_occ_map","",class="btn-ghost btn-sm",
                 icon=icon("download"),title="Download map as PNG"))
           ),
           withSpinner(leafletOutput("occ_map",height="350px"),color=ACC,type=6),
           tags$style(HTML(".leaflet-control-layers,.leaflet .legend{max-height:220px;overflow-y:auto;}"))
      )
    )
  ),

  # ── TAB 2: Diversity ──────────────────────────────────────
  nav_panel("02 \u00b7 Diversity", icon=bs_icon("hexagon"),
    layout_sidebar(fillable=TRUE,
      sidebar=sidebar(width=285, bg=PANEL,
        sdiv("H3 settings"),
        sliderInput("hex_res","Resolution",1,9,5,1,ticks=FALSE),
        tags$small(style=paste0("color:",MUT),"1=large hexagons \u00b7 9=fine hexagons"),
        br(), br(),
        numericInput("esn","ES estimator n",50,1),
        sdiv("Metric"),
        selectInput("estimator",NULL,choices=ESTIMATOR_CH),
        sdiv("Style"),
        selectInput("tile","Base map",choices=TILE_CH),
        selectInput("palette","Palette",choices=PALETTE_CH),
        sliderInput("fill_opacity","Opacity",0.1,1,0.75,0.05,ticks=FALSE),
        checkboxInput("show_borders","Hex borders",TRUE),
        checkboxInput("show_effort","Show effort in popup",TRUE),
        br(),
        actionButton("btn_build_hex","Build hex map",class="btn-green w-100",icon=icon("play")),
        br(),br(),
        uiOutput("hex_status_ui")
      ),
      layout_columns(col_widths=c(8,4),
        card(full_screen=TRUE,
             card_header(class="d-flex justify-content-between align-items-center",
               span(bs_icon("hexagon")," H3 diversity map"),
               div(style="display:flex;align-items:center;gap:8px;",
                 uiOutput("hex_metric_badge"),
                 downloadButton("dl_hex_map","",class="btn-ghost btn-sm",
                   icon=icon("download"),title="Download map as PNG"))),
             withSpinner(leafletOutput("hex_map",height="510px"),color=ACC,type=6)),
        card(
          card_header(bs_icon("list-ul")," Species in selected hexagon"),
          div(style=paste0("color:",MUT,";font-size:.78rem;padding:.4rem .6rem;"),
              "Click any hexagon on the map to see which species occur in it."),
          uiOutput("hex_species_panel")
        )
      ),
      layout_columns(col_widths=c(6,6),
        card(card_header("Richness distribution"),
             withSpinner(plotOutput("plot_hist",height="195px"),color=ACC,type=6)),
        card(card_header("Top 10 hexagons"),
             withSpinner(plotOutput("plot_top10",height="195px"),color=ACC,type=6)))
    )
  ),

  # ── TAB 3: jSDM ──────────────────────────────────────────
  nav_panel("03 \u00b7 jSDM", icon=bs_icon("cpu"),
    layout_sidebar(fillable=TRUE,
      sidebar=sidebar(width=320, bg=PANEL,

        # Step 1 ─────────────────────────────────────────────
        sdiv("Step 1 \u00b7 Choose layers"),
        uiOutput("env_layer_picker_ui"),
        uiOutput("corine_upload_ui"),
        # Step 2 ─────────────────────────────────────────────
        sdiv("Step 2 \u00b7 Download & VIF filter"),
        numericInput("vif_thr","VIF threshold",10,2,50),
        actionButton("btn_get_env","Download & run VIF",
                     class="btn-ghost w-100",icon=icon("globe")),
        br(),
        uiOutput("env_download_status_ui"),
        uiOutput("vif_result_ui"),
        # Step 3 ─────────────────────────────────────────────
        uiOutput("env_sel_ui"),

        sdiv("Site size"),
        numericInput("site_size_m", "Site size (metres)",
                     value=0, min=0, max=500000, step=500),
        div(style=paste0("font-size:.73rem;color:",MUT,";margin-bottom:4px;"),
          "0 = use native env. raster resolution (~9 km at 5′). ",
          "Set a larger value (e.g. 20 000 m) to aggregate occurrences into coarser sites, ",
          "increasing co-occurrence signal for sparse data. ",
          "Recommended range for reptile atlas data: 5 000 – 50 000 m."
        ),
        uiOutput("site_size_info_ui"),

        sdiv("MCMC settings"),
        fluidRow(
          column(6,numericInput("n_iter",  "Iterations",10000,1000)),
          column(6,numericInput("n_burnin","Burn-in",   2000, 500))),
        fluidRow(
          column(6,numericInput("n_thin",  "Thinning",  5,1)),
          column(6,numericInput("n_latent","Latent vars",2,1,10))),
        br(),
        actionButton("btn_run_jsdm","Run jSDM model",class="btn-violet w-100",icon=icon("play")),
        br(),br(),
        uiOutput("jsdm_status_ui"),
        br(),
        div(id="jsdm_log",class="log-box","Waiting\u2026")
      ),

      # KPIs — styled for readability
      layout_columns(col_widths=c(4,4,4),
        uiOutput("jkpi_sites"), uiOutput("jkpi_spp"), uiOutput("jkpi_vars")),

      navset_card_tab(

        nav_panel("Convergence",
          layout_columns(col_widths=c(3,9),
            card(card_header("Select"),
                 selectInput("conv_sp",  "Species",   choices=NULL),
                 selectInput("conv_par", "Parameter", choices=NULL)),
            layout_columns(col_widths=c(12),
              card(full_screen=TRUE,
                card_header(class="d-flex justify-content-between align-items-center",
                  span("Trace"),
                  downloadButton("dl_plot_trace","",class="btn-ghost btn-sm",icon=icon("download"))),
                withSpinner(plotOutput("plot_trace",  height="200px"),color=ACC,type=6)),
              card(full_screen=TRUE,
                card_header(class="d-flex justify-content-between align-items-center",
                  span("Posterior density"),
                  downloadButton("dl_plot_density","",class="btn-ghost btn-sm",icon=icon("download"))),
                withSpinner(plotOutput("plot_density",height="200px"),color=ACC,type=6)))
          )
        ),

        nav_panel("Residual correlations",
          card(full_screen=TRUE,
            card_header(class="d-flex justify-content-between align-items-center",
              span(bs_icon("grid")," Residual correlation matrix"),
              div(tags$small(style=paste0("color:",MUT,";margin-right:8px;"),"⛶ expand for detail"),
                  downloadButton("dl_plot_rescor","",class="btn-ghost btn-sm",icon=icon("download")))),
            withSpinner(plotOutput("plot_res_cor",height="600px"),color=ACC,type=6))
        ),

        nav_panel("Species responses",
          layout_columns(col_widths=c(3,9),
            card(card_header("Select species"),
                 selectInput("resp_sp","Species",choices=NULL)),
            card(full_screen=TRUE,
              card_header(class="d-flex justify-content-between align-items-center",
                span("Beta coefficients ± 95% CI"),
                div(tags$small(style=paste0("color:",MUT,";margin-right:8px;"),"⛶ expand"),
                    downloadButton("dl_plot_resp","",class="btn-ghost btn-sm",icon=icon("download")))),
              withSpinner(plotOutput("plot_sp_resp",height="460px"),color=ACC,type=6)))
        ),

        nav_panel("Predicted theta",
          card(full_screen=TRUE,
            card_header(class="d-flex justify-content-between align-items-center",
              span("Predicted occupancy θ"),
              downloadButton("dl_plot_theta","",class="btn-ghost btn-sm",icon=icon("download"))),
            withSpinner(plotOutput("plot_theta",height="380px"),color=ACC,type=6))
        ),

        nav_panel("Latent variables",
          card(full_screen=TRUE,
            card_header(class="d-flex justify-content-between align-items-center",
              span("Spatial latent variable scores"),
              div(tags$small(style=paste0("color:",MUT,";margin-right:8px;"),"⛶ expand"),
                  downloadButton("dl_plot_latent","",class="btn-ghost btn-sm",icon=icon("download")))),
            withSpinner(plotOutput("plot_latent",height="500px"),color=ACC,type=6))
        ),

        nav_panel("Deviance",
          card(full_screen=TRUE,
            card_header(class="d-flex justify-content-between align-items-center",
              span("Deviance trace"),
              downloadButton("dl_plot_dev","",class="btn-ghost btn-sm",icon=icon("download"))),
            withSpinner(plotOutput("plot_deviance",height="360px"),color=ACC,type=6))
        ),

        nav_panel("Species biplot",
          card(full_screen=TRUE,
            card_header(class="d-flex justify-content-between align-items-center",
              span("Species latent factor loadings (λ₁ vs λ₂)"),
              div(tags$small(style=paste0("color:",MUT,";margin-right:8px;"),"⛶ expand"),
                  downloadButton("dl_plot_biplot","",class="btn-ghost btn-sm",icon=icon("download")))),
            div(style=paste0("color:",MUT,";font-size:.76rem;padding:.3rem .8rem 0;"),
              "Same direction + far from origin = positively correlated beyond environment."),
            withSpinner(plotOutput("plot_sp_biplot",height="600px"),color=ACC,type=6))
        )
      )
    )
  ),

  # ── TAB 4: Data ────────────────────────────────────────────
  nav_panel("04 \u00b7 Data", icon=bs_icon("file-earmark-arrow-down"),
    layout_columns(col_widths=c(12),
      card(
        card_header(bs_icon("file-earmark-arrow-down")," Downloads"),
        card_body(
          tags$p(style=paste0("color:",MUT,";font-size:.82rem;"),
            "Download your occurrence data. ",
            "All map and plot downloads are available via the \u2913 buttons ",
            "directly on each figure in the tabs above."),
          layout_columns(col_widths=c(4,4,4),
            div(
              tags$b(style=paste0("color:",TXT,";font-size:.78rem;display:block;margin-bottom:6px;"),
                     "Occurrences"),
              downloadButton("dl_occ","Download CSV",class="btn-green w-100")
            ),
            div(
              tags$b(style=paste0("color:",TXT,";font-size:.78rem;display:block;margin-bottom:6px;"),
                     "jSDM model object"),
              downloadButton("dl_jsdm","Download RDS",class="btn-ghost w-100")
            ),
            div(
              tags$b(style=paste0("color:",TXT,";font-size:.78rem;display:block;margin-bottom:6px;"),
                     "Hex diversity table"),
              downloadButton("dl_hex_csv","Download CSV",class="btn-ghost w-100")
            )
          )
        )
      )
    )
  ),

  # ── TAB 5: Help ───────────────────────────────────────────
  nav_panel("Help", icon=bs_icon("question-circle"),
    layout_columns(col_widths=c(6,6),
      card(card_header("Workflow"),
        card_body(tags$ol(style="line-height:2;padding-left:1.2rem;",
          tags$li(tags$b("01 \u00b7 Data")," — Enter species or higher-taxon names. ",
            "Higher taxa (e.g. Serpentes) auto-expand via GBIF backbone. ",
            "Choose countries or WKT, apply filters, then click Fetch. ",
            "Or upload a CSV directly with the file browser."),
          tags$li(tags$b("02 \u00b7 Diversity")," — Choose H3 resolution and metric. Build to render hexagonal diversity polygons."),
          tags$li(tags$b("03 \u00b7 jSDM")," — Download WorldClim + land-cover layers. ",
            "Optional extras: NDVI, CORINE (EU only). ",
            "VIF filtering removes collinear predictors; retained/dropped variables are shown. ",
            "Manually include/exclude predictors before running the MCMC model."),
          tags$li(tags$b("04 \u00b7 Export")," — Preview and download the PNG map; download CSV, GeoJSON, or jSDM RDS.")))
      ),
      card(card_header("Reference"),
        card_body(tags$dl(
          tags$dt("Species richness"),   tags$dd("Unique species per hexagon."),
          tags$dt("Shannon H"),          tags$dd("Entropy; sensitive to rare species."),
          tags$dt("Simpson D"),          tags$dd("Probability two individuals differ."),
          tags$dt("ES / Hurlbert"),      tags$dd("Expected species in n rarefied records."),
          tags$dt("Hill 1/2/inf"),       tags$dd("q=1 exp(H), q=2 1/D, q=inf 1/maxp."),
          tags$dt("jSDM latent factors"),tags$dd("Shared axes capturing residual co-occurrence beyond environment."),
          tags$dt("Beta coefficients"),  tags$dd("Posterior mean effect of each predictor on occupancy.")
        ))
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════
# Increase max upload to 500 MB (needed for large rasters like CORINE)
options(shiny.maxRequestSize = 500 * 1024^2)

server <- function(input, output, session) {

  rv <- reactiveValues(
    occ_raw=NULL, occ_clean=NULL,
    polygons=NULL, diversity=NULL,
    env_rast_all=NULL,   # all downloaded layers
    env_rast_sel=NULL,   # user-selected subset
    vif_retained=NULL, vif_dropped=NULL, vif_table=NULL,
    jsdm_data=NULL, jsdm_mod=NULL,
    wkt=NULL,
    data_st="idle", hex_st="idle", jsdm_st="idle", env_st="idle"
  )

  # Log helpers
  ts  <- function() format(Sys.time(),"%H:%M:%S")
  log_to <- function(id, msg, col=GRN) {
    html <- paste0("<span style='color:",MUT,"'>[",ts(),"]</span> ",
                   "<span style='color:",col,"'>",msg,"</span>")
    session$sendCustomMessage("log", list(id=id, html=html))
  }
  logd <- function(m, col=GRN) log_to("data_log", m, col)
  logj <- function(m, col=GRN) log_to("jsdm_log", m, col)

  get_wkt <- reactive({
    if (input$geo_mode=="wkt") { req(nchar(trimws(input$wkt_input))>0); trimws(input$wkt_input) }
    else { req(length(input$countries)>0); countries_to_wkt(input$countries) }
  })

  observe({
    if (nchar(input$gbif_user) >0) Sys.setenv(GBIF_USER =input$gbif_user)
    if (nchar(input$gbif_pwd)  >0) Sys.setenv(GBIF_PWD  =input$gbif_pwd)
    if (nchar(input$gbif_email)>0) Sys.setenv(GBIF_EMAIL=input$gbif_email)
  })

  # ── GBIF fetch ────────────────────────────────────────────
  observeEvent(input$btn_fetch, {
    req(input$taxon_query); rv$data_st <- "running"
    session$sendCustomMessage("clearLog",list(id="data_log"))
    withProgress(message="Fetching from GBIF\u2026", value=0, {
      tryCatch({
        logd("Resolving taxon names\u2026")
        logd("Resolving taxon names\u2026")
        taxa_resolved <- resolve_taxa(input$taxon_query)
        if (!length(taxa_resolved$keys)) stop("No taxa could be resolved from query.")
        logd(paste0("Resolved ", length(taxa_resolved$keys), " GBIF key(s): ",
                    paste(taxa_resolved$labels, collapse=", ")))
        wkt <- get_wkt(); rv$wkt <- wkt
        incProgress(0.1)
        logd("Submitting occ_download to GBIF (may take a few minutes)\u2026", col=AMB)
        df <- download_gbif(taxa_resolved, wkt, input$yr_min, input$yr_max, input$max_unc)
        incProgress(0.55)
        if (is.null(df)||!nrow(df)){logd("No records.",col=RED);rv$data_st<-"error";return()}
        logd(paste0("Downloaded ",nrow(df)," raw records."))
        if (input$run_cc){
          logd("Running CoordinateCleaner\u2026")
          df2 <- clean_occ(df)
          logd(paste0("Retained ",nrow(df2)," clean records."))
        } else {
          df2 <- df %>% select(species,decimalLongitude,decimalLatitude,year,basisOfRecord) %>%
            filter(!is.na(decimalLongitude),!is.na(decimalLatitude))
        }
        rv$occ_raw <- df; rv$occ_clean <- df2; rv$data_st <- "done"
        logd(paste0("\u2713 ",n_distinct(df2$species)," spp / ",nrow(df2)," records."))
        incProgress(0.35)
      }, error=function(e){rv$data_st<-"error";logd(paste0("\u2715 ",e$message),col=RED)
                           showNotification(e$message,type="error")})
    })
  })

  # ── CSV upload — always visible button, no conditionalPanel ──
  observeEvent(input$btn_load_csv, {
    req(input$csv_upload)
    tryCatch({
      df <- read.csv(input$csv_upload$datapath, stringsAsFactors=FALSE)
      # rename requested columns
      sp_col  <- input$csv_sp;  lon_col <- input$csv_lon; lat_col <- input$csv_lat
      if (!all(c(sp_col,lon_col,lat_col) %in% names(df)))
        stop(paste("Columns not found:",paste(setdiff(c(sp_col,lon_col,lat_col),names(df)),collapse=", ")))
      names(df)[names(df)==sp_col]  <- "species"
      names(df)[names(df)==lon_col] <- "decimalLongitude"
      names(df)[names(df)==lat_col] <- "decimalLatitude"
      df <- df %>% filter(!is.na(decimalLongitude),!is.na(decimalLatitude),
                          !is.na(species),species!="")
      if (!"year"         %in% names(df)) df$year          <- NA_integer_
      if (!"basisOfRecord"%in% names(df)) df$basisOfRecord <- NA_character_
      rv$occ_raw <- df; rv$occ_clean <- df; rv$data_st <- "done"
      rv$wkt <- tryCatch(get_wkt(),error=function(e) NULL)
      logd(paste0("\u2713 CSV loaded: ",n_distinct(df$species)," spp / ",nrow(df)," rows."))
      showNotification(paste0("\u2713 ",nrow(df)," records loaded."),type="message")
    }, error=function(e) showNotification(paste("CSV error:",e$message),type="error"))
  })

  # ── KPIs ──────────────────────────────────────────────────
  # KPI helper — icon wrapped in a coloured circle for visibility
  kpi_icon <- function(icon_name, bg=ACC) {
    div(style=paste0(
          "width:42px;height:42px;border-radius:50%;",
          "background:", bg, ";display:flex;",
          "align-items:center;justify-content:center;"),
      bs_icon(icon_name, size="1.2em",
              style=paste0("color:", D, ";"))
    )
  }

  output$kpi_records <- renderUI({
    n <- if (!is.null(rv$occ_clean)) nrow(rv$occ_clean) else "—"
    value_box("Records", n,
              showcase=kpi_icon("pin-map-fill", ACC), theme="primary")
  })
  output$kpi_species <- renderUI({
    n <- if (!is.null(rv$occ_clean)) n_distinct(rv$occ_clean$species) else "—"
    value_box("Species", n,
              showcase=kpi_icon("bug-fill", GRN), theme="primary")
  })
  output$kpi_status <- renderUI({
    value_box("Status", badge_status(rv$data_st),
              showcase=kpi_icon("database-check", VIO), theme="primary")
  })

  # ── Occurrence map ────────────────────────────────────────
  output$occ_map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% setView(20,20,zoom=2)
  })
  observe({
    req(rv$occ_clean)
    df   <- rv$occ_clean
    mode <- if (!is.null(input$occ_color_by)) input$occ_color_by else "species"

    # Derive grouping variable
    df$group <- if (mode == "genus") {
      # First word of species name = genus
      sapply(strsplit(df$species, " "), `[`, 1)
    } else {
      df$species
    }

    grps <- unique(df$group)
    pal  <- colorFactor(viridis(min(length(grps), 256), option="turbo"), domain=grps)

    leafletProxy("occ_map") %>% clearMarkers() %>% clearControls() %>%
      addCircleMarkers(data=df, lng=~decimalLongitude, lat=~decimalLatitude,
        color=~pal(group), radius=3, weight=1, opacity=.9, fillOpacity=.65,
        popup=~paste0("<b>",species,"</b><br>",
                      round(decimalLongitude,4),", ",round(decimalLatitude,4))) %>%
      addLegend(
        pal      = pal,
        values   = grps,
        title    = if(mode=="genus") "Genus" else "Species",
        position = "bottomright",
        opacity  = .85,
        labFormat = labelFormat()
      ) %>%
      fitBounds(min(df$decimalLongitude,na.rm=TRUE), min(df$decimalLatitude,na.rm=TRUE),
                max(df$decimalLongitude,na.rm=TRUE), max(df$decimalLatitude,na.rm=TRUE))
  })

  # Download occurrence map as PNG
  output$dl_occ_map <- downloadHandler(
    filename = function() paste0("occurrence_map_",
                                 if(!is.null(input$occ_color_by)) input$occ_color_by else "species",
                                 ".png"),
    content = function(file) {
      req(rv$occ_clean)
      df   <- rv$occ_clean
      mode <- if (!is.null(input$occ_color_by)) input$occ_color_by else "species"
      df$group <- if (mode=="genus") sapply(strsplit(df$species," "),`[`,1) else df$species
      grps <- sort(unique(df$group))

      world <- ne_countries(scale="medium", returnclass="sf")
      bbox  <- c(xmin=min(df$decimalLongitude,na.rm=TRUE)-1,
                 xmax=max(df$decimalLongitude,na.rm=TRUE)+1,
                 ymin=min(df$decimalLatitude, na.rm=TRUE)-1,
                 ymax=max(df$decimalLatitude, na.rm=TRUE)+1)

      # Cap legend to 30 items to keep PNG readable
      show_legend <- length(grps) <= 30
      n_col <- min(length(grps), 256)
      cols  <- setNames(viridis(n_col, option="turbo")[
                  as.integer(cut(seq_along(grps), n_col, labels=FALSE))],
                grps)

      p <- ggplot() +
        geom_sf(data=world, fill="#111827", color="#1f2d3d", linewidth=.2) +
        geom_point(data=df, aes(x=decimalLongitude, y=decimalLatitude,
                                color=group), size=.8, alpha=.75) +
        scale_color_manual(values=cols, name=if(mode=="genus") "Genus" else "Species",
                           guide=if(show_legend) guide_legend(ncol=2, override.aes=list(size=3))
                                 else "none") +
        coord_sf(xlim=c(bbox["xmin"],bbox["xmax"]), ylim=c(bbox["ymin"],bbox["ymax"])) +
        labs(title=paste("Occurrence map —", if(mode=="genus") "by genus" else "by species"),
             x="Longitude", y="Latitude") +
        gg_dark() +
        theme(legend.text=element_text(size=6), legend.title=element_text(size=7),
              legend.key.size=unit(0.35,"cm"))
      if (!show_legend)
        p <- p + labs(caption=paste0(length(grps)," groups — legend hidden (>30 items)"))

      ggsave(file, p, width=12, height=8, dpi=250, bg=D)
    }
  )

  # ── H3 diversity ──────────────────────────────────────────
  observeEvent(input$btn_build_hex,{
    req(rv$occ_clean); rv$hex_st <- "running"
    withProgress(message="Building hexagons\u2026",value=0.1,{
      tryCatch({
        df <- rv$occ_clean
        cells <- df %>%
          mutate(cell=geo_to_h3(data.frame(lat=decimalLatitude,lng=decimalLongitude),
                                res=as.integer(input$hex_res))) %>%
          filter(!is.na(species),species!="") %>%
          group_by(cell,species) %>% summarise(records=n(),.groups="drop")
        incProgress(0.35,detail="Computing diversity\u2026")
        cd <- obisindicators::calc_indicators(cells,esn=input$esn)
        rv$diversity <- cd
        incProgress(0.35,detail="Building polygons\u2026")
        rv$polygons <- build_h3_polys(cd); rv$hex_st <- "done"
        incProgress(0.2)
      },error=function(e){rv$hex_st<-"error"
        showNotification(paste("Hex error:",e$message),type="error")})
    })
  })

  output$hex_status_ui   <- renderUI(badge_status(rv$hex_st))

  output$dl_hex_map <- downloadHandler(
    filename = function() paste0("hex_map_", input$estimator, ".png"),
    content  = function(file) {
      req(rv$polygons)
      polys <- st_transform(rv$polygons, 3857)
      bbox  <- st_bbox(polys)
      est   <- input$estimator
      pal_fn <- scale_fill_viridis_c(name=LEG[[est]], option=input$palette)
      world <- ne_countries(scale="medium", returnclass="sf") %>% st_transform(3857)
      p <- ggplot() +
        geom_sf(data=world, fill="#111827", color="#1f2d3d", linewidth=.2) +
        geom_sf(data=polys, aes(fill=.data[[est]]), color="black", linewidth=.05) +
        pal_fn +
        coord_sf(xlim=c(bbox["xmin"],bbox["xmax"]), ylim=c(bbox["ymin"],bbox["ymax"])) +
        labs(title=paste("H3 diversity map —", LEG[[est]]),
             subtitle=paste0("H3 resolution ", input$hex_res)) +
        theme_void() +
        theme(plot.background=element_rect(fill=D,color=NA),
              plot.title  =element_text(color=TXT,family="Space Grotesk",face="bold",size=12),
              plot.subtitle=element_text(color=MUT,family="JetBrains Mono",size=9),
              legend.background=element_rect(fill=CARD,color=NA),
              legend.text =element_text(color=TXT,family="JetBrains Mono",size=8),
              legend.title=element_text(color=TXT,family="Space Grotesk",face="bold",size=9))
      ggsave(file, p, width=12, height=8, dpi=250, bg=D)
    }
  )
  output$hex_metric_badge <- renderUI({
    req(rv$polygons)
    tags$span(class="badge badge-cyan", LEG[[input$estimator]])
  })

  output$hex_map <- renderLeaflet(
    leaflet() %>% addProviderTiles(providers[[input$tile]]) %>% setView(20,38,zoom=5))

  observe({
    req(rv$polygons); polys <- rv$polygons; est <- input$estimator; vals <- polys[[est]]
    pal <- colorNumeric(viridis(100,option=input$palette),domain=vals,na.color="transparent")
    brd <- if(isTRUE(input$show_borders)) "white" else NA
    pop <- if(isTRUE(input$show_effort))
      ~paste0("<b>",LEG[[est]],":</b> ",round(get(est),3),"<br><b>Records:</b> ",survey_effort)
    else ~paste0("<b>",LEG[[est]],":</b> ",round(get(est),3))
    leafletProxy("hex_map",data=polys) %>% clearShapes() %>% clearControls() %>%
      addProviderTiles(providers[[input$tile]]) %>%
      addPolygons(fillColor=~pal(get(est)),color=brd,weight=0.6,opacity=.9,
        fillOpacity=input$fill_opacity,popup=pop,
        highlight=highlightOptions(weight=2,color="#fff",
          fillOpacity=min(input$fill_opacity+.15,1),bringToFront=TRUE)) %>%
      addLegend(pal=pal,values=vals,title=LEG[[est]],opacity=.9,position="bottomright") %>%
      fitBounds(st_bbox(polys)["xmin"],st_bbox(polys)["ymin"],
                st_bbox(polys)["xmax"],st_bbox(polys)["ymax"])
  })

  # ── Hex click: show species in that cell ──────────────────
  rv$clicked_cell <- NULL

  observeEvent(input$hex_map_shape_click, {
    click <- input$hex_map_shape_click
    req(click, rv$occ_clean, rv$polygons)
    # Find which polygon was clicked by proximity of click centre to polygon centroids
    polys_wgs <- tryCatch(st_transform(rv$polygons, 4326), error=function(e) NULL)
    req(polys_wgs)
    click_pt <- st_sfc(st_point(c(click$lng, click$lat)), crs=4326)
    dists    <- st_distance(st_centroid(st_geometry(polys_wgs)), click_pt)
    idx      <- which.min(dists)
    cell_id  <- rv$polygons$cell[idx]
    rv$clicked_cell <- cell_id
  })

  output$hex_species_panel <- renderUI({
    req(rv$clicked_cell, rv$occ_clean)
    df  <- rv$occ_clean
    res <- as.integer(input$hex_res)
    # Assign H3 cell to each occurrence
    df2 <- df %>%
      mutate(cell = h3::geo_to_h3(
        data.frame(lat=decimalLatitude, lng=decimalLongitude), res=res)) %>%
      filter(cell == rv$clicked_cell)
    spp <- sort(unique(df2$species))
    n_rec <- nrow(df2)
    if (!length(spp)) {
      return(div(style=paste0("color:",MUT,";font-size:.8rem;padding:.6rem;"),
                 "No occurrences mapped to this hexagon at current resolution."))
    }
    tagList(
      div(style=paste0("background:",BRD,";border-radius:6px;padding:6px 10px;",
                       "margin:.4rem;font-size:.75rem;color:",TXT,";"),
        tags$b(length(spp), " species  ·  ", n_rec, " records"),
        br(),
        tags$span(style=paste0("color:",MUT,";font-size:.68rem;"), rv$clicked_cell)
      ),
      div(style=paste0("padding:.3rem .5rem;max-height:380px;overflow-y:auto;"),
        lapply(spp, function(sp) {
          n <- nrow(df2[df2$species==sp,])
          div(style=paste0("display:flex;justify-content:space-between;",
                           "padding:4px 6px;border-bottom:1px solid ",BRD,";",
                           "font-size:.78rem;"),
            tags$span(style=paste0("color:",TXT,";font-style:italic;"), sp),
            tags$span(style=paste0("color:",ACC,";font-weight:600;"), n, " rec.")
          )
        })
      )
    )
  })

  output$plot_hist <- renderPlot({
    req(rv$polygons); df <- as.data.frame(rv$polygons)
    ggplot(df,aes(x=richness))+geom_histogram(binwidth=1,fill=ACC,color=D,alpha=.85)+
      labs(x="Species richness",y="Hexagons")+gg_dark()
  },bg="transparent")

  output$plot_top10 <- renderPlot({
    req(rv$polygons)
    df <- as.data.frame(rv$polygons)
    top10 <- df %>% arrange(desc(richness)) %>% slice_head(n=10) %>%
      mutate(rank=paste0("#",row_number()),
             label=paste0(rank,": ",richness," spp
(",survey_effort," rec.)"))
    ggplot(top10, aes(x=reorder(rank, richness), y=richness)) +
      geom_segment(aes(xend=reorder(rank,richness), y=0, yend=richness),
                   color=BRD, linewidth=3) +
      geom_point(aes(size=survey_effort, color=shannon_diversity), alpha=.9) +
      geom_text(aes(label=richness), color=TXT, size=3, hjust=-0.5) +
      scale_color_viridis_c(option="magma", name="Shannon H") +
      scale_size_continuous(name="Records", range=c(3,9)) +
      coord_flip() +
      labs(title="Top 10 richest hexagons",
           subtitle="Dot size = records  ·  colour = Shannon diversity",
           x=NULL, y="Species richness") +
      gg_dark() +
      theme(axis.text.y=element_text(size=9, color=TXT))
  }, bg="transparent")

  # ── jSDM: layer picker with cache status ──────────────────
  output$env_layer_picker_ui <- renderUI({
    cached <- layers_cached()
    make_label <- function(key, label) {
      status <- if (key %in% cached)
        tags$span(style=paste0("color:",GRN,";font-size:.7rem;"), " ✓cached")
      else
        tags$span(style=paste0("color:",MUT,";font-size:.7rem;"), " ↳download")
      tagList(label, status)
    }
    tagList(
      checkboxGroupInput("base_env_sel", "Base layers",
        choiceValues  = c("bio","elev","built","grassland","trees","footprint","water","wetland"),
        choiceNames   = list(
          make_label("bio",       "Bioclim (19 vars)"),
          make_label("elev",      "Elevation"),
          make_label("built",     "Built area"),
          make_label("grassland", "Grassland"),
          make_label("trees",     "Tree cover"),
          make_label("footprint", "Human footprint"),
          make_label("water",     "Surface water fraction"),
          make_label("wetland",   "Wetland fraction")),
        selected = c("bio","elev","built","grassland","trees","footprint","water","wetland")),
      checkboxGroupInput("extra_env", "Optional layers",
        choiceValues = c("ndvi","corine"),
        choiceNames  = list(
          make_label("ndvi",   "NDVI proxy (bare-soil, global)"),
          make_label("corine", tags$span("CORINE ⚠ EU only"))),
        selected = NULL)
    )
  })

  # ── jSDM: CORINE upload UI ─────────────────────────────────
  output$corine_upload_ui <- renderUI({
    req("corine" %in% input$extra_env)
    tagList(
      tags$small(style=paste0("color:",AMB),
        "CORINE cannot be auto-downloaded. Upload a GeoTIFF for your study area:"),
      fileInput("corine_file", NULL, accept=c(".tif",".tiff"),
                buttonLabel="Browse...", placeholder="No CORINE file selected")
    )
  })

  output$env_download_status_ui <- renderUI(badge_status(rv$env_st))

  # ── jSDM: env layers ───────────────────────────────────────
  observeEvent(input$btn_get_env, {
    wkt <- tryCatch(rv$wkt %||% get_wkt(), error=function(e) NULL)
    req(wkt)
    v       <- vect(wkt, crs="EPSG:4326")
    ext_obj <- ext(v)

    base_sel  <- if (length(input$base_env_sel)  > 0) input$base_env_sel
                 else c("bio","elev","built","grassland","trees","footprint","water","wetland")
    extra_sel <- if (length(input$extra_env) > 0) input$extra_env else character(0)

    logj(paste0("Requested: ", paste(c(base_sel, extra_sel), collapse=", ")), col=AMB)
    rv$env_st <- "running"

    withProgress(message="Downloading env. layers...", value=0.05, {
      tryCatch({
        # Pass extra (non-corine) to get_env_stack
        env <- get_env_stack(ext_obj,
                             extra = extra_sel[extra_sel != "corine"],
                             wdir  = tempdir())

        # Optionally add CORINE from uploaded file
        if ("corine" %in% extra_sel && !is.null(input$corine_file)) {
          logj("Adding CORINE from uploaded file...")
          corine_r <- tryCatch({
            r <- rast(input$corine_file$datapath)
            # Reproject to WGS84 if needed
            if (!same.crs(r, env[[1]])) {
              logj("  Reprojecting CORINE to WGS84...")
              r <- project(r, crs(env[[1]]), method="near")
            }
            # Compute intersection of CORINE and study extent
            ce <- ext(r); se <- ext_obj
            ix1 <- max(ce$xmin, se$xmin); ix2 <- min(ce$xmax, se$xmax)
            iy1 <- max(ce$ymin, se$ymin); iy2 <- min(ce$ymax, se$ymax)
            if (ix1 >= ix2 || iy1 >= iy2) {
              logj("  CORINE does not overlap study area — skipping.", col=AMB)
              NULL
            } else {
              r_crop <- crop(r, ext(ix1, ix2, iy1, iy2))
              resample(r_crop, env[[1]], method="near")
            }
          }, error=function(e) { logj(paste0("CORINE failed: ", e$message), col=AMB); NULL })
          if (!is.null(corine_r)) {
            names(corine_r) <- "corine"
            env <- c(env, corine_r)
            logj("CORINE added (NA outside coverage area).")
            tryCatch(writeRaster(corine_r, layer_cache_files[["corine"]], overwrite=TRUE),
                     error=function(e) NULL)
          }
        }

        incProgress(0.5, detail="VIF filtering...")
        logj("Running VIF filtering...")
        vr <- run_vif(env, thr=input$vif_thr)

        rv$env_rast_all <- env
        rv$env_rast_sel <- vr$rast
        rv$vif_retained <- vr$retained
        rv$vif_dropped  <- vr$dropped
        rv$vif_table    <- vr$vif_table
        rv$env_st       <- "done"

        logj(paste0("\u2713 ", nlyr(env), " layers total | ",
                    length(vr$retained), " retained | ",
                    length(vr$dropped),  " dropped by VIF."))
        showNotification(paste0("\u2713 ", nlyr(env), " layers ready, ",
                                length(vr$retained), " pass VIF."),
                         type="message", duration=6)
        incProgress(0.45)

      }, error=function(e) {
        rv$env_st <- "error"
        logj(paste0("\u2715 ", e$message), col=RED)
        showNotification(paste("Env error:", e$message), type="error")
      })
    })
  })


  # VIF result display
  output$vif_result_ui <- renderUI({
    req(rv$vif_retained)
    ret_html <- paste(sapply(rv$vif_retained,
      function(v) paste0("<span class='vif-retained'>",v,"</span>")),collapse=" ")
    drp_html <- if (length(rv$vif_dropped) > 0) {
      paste(sapply(rv$vif_dropped, function(v) {
        paste0("<span class='vif-dropped'>", v, "</span>")
      }), collapse = " ")
    } else {
      paste0("<em style='color:", MUT, "'>none</em>")
    }
    tagList(
      div(style=paste0("background:",D,";border:1px solid ",BRD,
                       ";border-radius:7px;padding:8px 10px;margin:6px 0;font-size:.75rem;"),
        tags$p(style="margin:0 0 4px;",
               tags$span(style=paste0("color:",GRN,";font-family:'Space Grotesk';font-weight:700;"),
                         "\u2713 Retained: "),
               HTML(ret_html)),
        tags$p(style="margin:0;",
               tags$span(style=paste0("color:",RED,";font-family:'Space Grotesk';font-weight:700;"),
                         "\u2715 Dropped: "),
               HTML(drp_html))
      )
    )
  })

  # Env variable selector (Step 3 — after VIF)
  output$env_sel_ui <- renderUI({
    req(rv$vif_retained, rv$env_rast_all)
    all_vars <- names(rv$env_rast_all)
    # Build labelled choices with VIF status indicator
    choice_labels <- setNames(all_vars, sapply(all_vars, function(v) {
      if (v %in% rv$vif_retained) paste0("[OK] ", v)
      else                        paste0("[DROP] ", v)
    }))
    tagList(
      sdiv("Step 3 \u00b7 Manual predictor selection"),
      div(style=paste0("font-size:.75rem;color:",MUT,";margin-bottom:6px;"),
          tags$span(style=paste0("color:",GRN,";font-weight:700;"), "[OK]"),
          " = retained by VIF   ",
          tags$span(style=paste0("color:",RED,";font-weight:700;"), "[DROP]"),
          " = flagged as collinear. You can override either."),
      checkboxGroupInput("env_var_check", "",
        choices  = choice_labels,
        selected = rv$vif_retained)
    )
  })

  # ── jSDM KPIs ────────────────────────────────────────────
  output$site_size_info_ui <- renderUI({
    sz <- if (!is.null(input$site_size_m)) input$site_size_m else 0
    if (sz > 0) {
      # estimate approx degree size at 40N (middle of typical European study)
      deg <- round(sz / 111000, 3)
      div(style=paste0("background:",BRD,";border-radius:6px;padding:5px 9px;",
                       "font-size:.72rem;color:",TXT,";margin-bottom:4px;"),
        tags$b(style=paste0("color:",ACC,";"), "Custom grid active: "),
        paste0(sz, " m ≈ ", deg, "° (",
               round(sz/1000, 1), " km)")
      )
    } else {
      div(style=paste0("font-size:.72rem;color:",MUT,";margin-bottom:4px;"),
        "Native resolution: ~9 km (5′ arc-min)")
    }
  })

  output$jkpi_sites <- renderUI({
    n <- if (!is.null(rv$jsdm_data)) nrow(rv$jsdm_data$pa) else
         if (!is.null(rv$env_rast_sel)) terra::ncell(rv$env_rast_sel[[1]]) else "—"
    value_box("Sites",    as.character(n),
              showcase=kpi_icon("grid-3x3", ACC), theme="primary")
  })
  output$jkpi_spp <- renderUI({
    n <- if (!is.null(rv$jsdm_data)) length(rv$jsdm_data$species) else
         if (!is.null(rv$occ_clean)) n_distinct(rv$occ_clean$species) else "—"
    value_box("Species",  as.character(n),
              showcase=kpi_icon("bug-fill", GRN), theme="primary")
  })
  output$jkpi_vars <- renderUI({
    n <- if (!is.null(rv$jsdm_data)) length(rv$jsdm_data$env_vars) else
         if (!is.null(rv$env_rast_sel)) nlyr(rv$env_rast_sel) else "—"
    value_box("Env vars", as.character(n),
              showcase=kpi_icon("bar-chart-fill", AMB), theme="primary")
  })
  output$jsdm_status_ui <- renderUI(badge_status(rv$jsdm_st))

  # ── jSDM run ──────────────────────────────────────────────
  observeEvent(input$btn_run_jsdm,{
    req(rv$occ_clean, rv$env_rast_all)
    rv$jsdm_st <- "running"
    session$sendCustomMessage("clearLog",list(id="jsdm_log"))
    withProgress(message="Fitting jSDM\u2026",value=0.05,{
      tryCatch({
        # Build env raster from user selection
        sel <- if(!is.null(input$env_var_check)&&length(input$env_var_check)>0)
                 intersect(input$env_var_check,names(rv$env_rast_all))
               else rv$vif_retained
        env_use <- rv$env_rast_all[[sel]]
        logj(paste0("Using ",nlyr(env_use)," predictors: ",paste(sel,collapse=", ")))
        incProgress(0.1,detail="Building PA matrix\u2026")
        site_sz <- if (!is.null(input$site_size_m) && input$site_size_m > 0)
                      input$site_size_m else NULL
        if (!is.null(site_sz))
          logj(paste0("Custom site size: ", site_sz, " m"), col=AMB)
        else
          logj("Using native env raster resolution as site grid.", col=MUT)
        jd <- prepare_jsdm(rv$occ_clean, env_use, site_size_m=site_sz)
        rv$jsdm_data <- jd
        logj(paste0("PA matrix: ",nrow(jd$pa)," sites x ",ncol(jd$pa)," species."))
        logj(paste0("Site grid: ", jd$site_desc))

        # Populate species dropdowns BEFORE running model
        spp_names <- jd$species
        updateSelectInput(session,"conv_sp",  choices=spp_names, selected=spp_names[1])
        updateSelectInput(session,"resp_sp",  choices=spp_names, selected=spp_names[1])

        logj(paste0("MCMC: iter=",input$n_iter," burnin=",input$n_burnin,
                    " thin=",input$n_thin," latent=",input$n_latent,"\u2026"),col=AMB)
        incProgress(0.1,detail="Running MCMC\u2026")
        # capture.output suppresses jSDM's interactive progress that
        # triggers "Selection:" prompts in RStudio's console
        invisible(capture.output(
          mod <- run_jsdm_model(jd,input$n_iter,input$n_burnin,input$n_thin,input$n_latent)
        ))
        rv$jsdm_mod <- mod; rv$jsdm_st <- "done"

        # Populate parameter selector from actual model output
        if(!is.null(mod$mcmc.sp)&&length(mod$mcmc.sp)>0){
          pars <- colnames(mod$mcmc.sp[[1]])
          updateSelectInput(session,"conv_par",choices=pars,selected=pars[1])
        }
        logj("\u2713 jSDM fitted successfully.")
        showNotification("\u2713 jSDM done!",type="message")
        incProgress(0.8)
      },error=function(e){rv$jsdm_st<-"error"
        logj(paste0("\u2715 ",e$message),col=RED)
        showNotification(paste("jSDM error:",e$message),type="error")})
    })
  })

  # ── Convergence: trace ────────────────────────────────────
  # Key fix: use isolate + reactive on input$conv_sp to force rerender
  output$plot_trace <- renderPlot({
    req(rv$jsdm_mod)
    sp  <- input$conv_sp;  par <- input$conv_par
    req(sp, par)
    mod <- rv$jsdm_mod
    idx <- which(names(mod$mcmc.sp)==sp)
    if(!length(idx)) idx <- 1
    ch <- tryCatch(as.numeric(mod$mcmc.sp[[idx]][,par]),error=function(e) NULL)
    req(!is.null(ch)&&length(ch)>0)
    df <- data.frame(iter=seq_along(ch),value=ch)
    ggplot(df,aes(iter,value))+geom_line(color=ACC,linewidth=.35)+
      labs(title=paste("Trace —",par,"—",sp),x="Iteration",y="Value")+gg_dark()
  },bg="transparent")

  output$plot_density <- renderPlot({
    req(rv$jsdm_mod)
    sp <- input$conv_sp; par <- input$conv_par
    req(sp, par)
    mod <- rv$jsdm_mod
    idx <- which(names(mod$mcmc.sp)==sp)
    if(!length(idx)) idx <- 1
    ch <- tryCatch(as.numeric(mod$mcmc.sp[[idx]][,par]),error=function(e) NULL)
    req(!is.null(ch)&&length(ch)>0)
    df <- data.frame(value=ch)
    ggplot(df,aes(value))+
      geom_density(fill=VIO,color=NA,alpha=.75)+
      geom_vline(xintercept=mean(ch),color=AMB,linetype="dashed",linewidth=.8)+
      labs(title=paste("Posterior density —",par),x="Value",y="Density")+gg_dark()
  },bg="transparent")

  # ── Residual correlations ─────────────────────────────────
  output$plot_res_cor <- renderPlot({
    req(rv$jsdm_mod)
    old <- par(bg=CARD,col.axis=MUT,col.lab=MUT,col.main=TXT,fg=BRD)
    on.exit(par(old))
    tryCatch(jSDM::plot_residual_cor(rv$jsdm_mod,tl.cex=1),
             error=function(e){plot.new();text(0.5,0.5,paste("Error:",e$message),col=TXT)})
  },bg=CARD)

  # ── Species responses — reacts to resp_sp input ───────────
  output$plot_sp_resp <- renderPlot({
    req(rv$jsdm_mod, rv$jsdm_data)
    sp <- input$resp_sp; req(sp)
    mod <- rv$jsdm_mod
    idx <- which(names(mod$mcmc.sp)==sp)
    if(!length(idx)) idx <- 1
    n_env  <- length(rv$jsdm_data$env_vars)
    b_cols <- seq_len(n_env+1)
    betas <- tryCatch(colMeans(mod$mcmc.sp[[idx]][,b_cols,drop=FALSE]),error=function(e) NULL)
    req(betas)
    ci <- tryCatch(apply(mod$mcmc.sp[[idx]][,b_cols,drop=FALSE],2,
                         function(x) quantile(x,c(.025,.975))),error=function(e) NULL)
    df <- tibble(param=names(betas),mean=as.numeric(betas),
                 lo=if(!is.null(ci)) ci[1,] else NA_real_,
                 hi=if(!is.null(ci)) ci[2,] else NA_real_,
                 dir=ifelse(mean>0,"pos","neg"))
    p <- ggplot(df,aes(x=reorder(param,mean),y=mean,fill=dir))+
      geom_col(alpha=.85)+
      scale_fill_manual(values=c(pos=GRN,neg=RED),guide="none")+
      coord_flip()+
      labs(title=paste("Beta coefficients —",sp),subtitle="Posterior mean ± 95% CI",
           x=NULL,y="Posterior mean")+gg_dark()
    if(!anyNA(df$lo))
      p <- p+geom_errorbar(aes(ymin=lo,ymax=hi),width=.25,color=TXT,linewidth=.5)
    p
  },bg="transparent")

  # ── Predicted theta ───────────────────────────────────────
  output$plot_theta <- renderPlot({
    req(rv$jsdm_mod)
    theta <- tryCatch(as.vector(rv$jsdm_mod$theta_latent),error=function(e) NULL)
    req(!is.null(theta)&&length(theta)>0)
    df <- data.frame(theta=theta)
    ggplot(df,aes(theta))+
      geom_histogram(bins=60,fill=ACC,color=D,alpha=.85)+
      labs(title="Predicted occurrence probability \u03b8",x="\u03b8",y="Count")+gg_dark()
  },bg="transparent")

  # ── Latent variable site scores ──────────────────────────
  # ── Latent variable spatial maps ──────────────────────────
  # "Sites" here = raster grid cells (not GBIF points).
  # jsdm_data$coords holds the lon/lat of every cell used in
  # the PA matrix.  Mapping W1/W2 back onto those coordinates
  # shows WHERE the unexplained environmental gradient is
  # strongest — the only spatially meaningful display.
  output$plot_latent <- renderPlot({
    req(rv$jsdm_mod, rv$jsdm_data)
    mod     <- rv$jsdm_mod
    coords  <- rv$jsdm_data$coords          # data.frame: x, y (WGS84)

    lv1_mat <- tryCatch(as.matrix(mod$mcmc.latent[["lv_1"]]), error=function(e) NULL)
    lv2_mat <- tryCatch(as.matrix(mod$mcmc.latent[["lv_2"]]), error=function(e) NULL)

    if (is.null(lv1_mat) || ncol(lv1_mat) == 0) {
      return(ggplot() +
        annotate("text", x=0.5, y=0.5,
                 label="Latent variable data unavailable.\nRun jSDM model first.",
                 color=TXT, size=4, hjust=0.5) +
        theme_void() + theme(plot.background=element_rect(fill=CARD, color=NA)))
    }

    # Number of sites must match number of columns in mcmc.latent
    W1 <- colMeans(lv1_mat)
    if (length(W1) != nrow(coords)) {
      return(ggplot() +
        annotate("text", x=0.5, y=0.5,
                 label=paste0("Dimension mismatch: ", length(W1),
                              " site scores vs ", nrow(coords), " grid cells."),
                 color=TXT, size=3.5, hjust=0.5) +
        theme_void() + theme(plot.background=element_rect(fill=CARD, color=NA)))
    }

    df <- data.frame(lon=coords$x, lat=coords$y, W1=W1)

    if (!is.null(lv2_mat) && ncol(lv2_mat) == length(W1)) {
      # Two latent variables: side-by-side spatial maps
      W2 <- colMeans(lv2_mat)
      df$W2 <- W2
      df_long <- tidyr::pivot_longer(df, cols=c("W1","W2"),
                                     names_to="LV", values_to="score")
      ggplot(df_long, aes(lon, lat, fill=score)) +
        geom_tile() +
        facet_wrap(~LV, labeller=labeller(LV=c(
          W1="W₁ — Latent axis 1",
          W2="W₂ — Latent axis 2"))) +
        scale_fill_viridis_c(option="magma", name="Score") +
        coord_equal() +
        labs(title="Spatial distribution of latent variable site scores",
             subtitle=paste0(
               "Sites = ", nrow(coords), " raster grid cells. ",
               "High absolute scores indicate cells with strong unexplained\n",
               "co-occurrence signal not captured by the environmental predictors."),
             x="Longitude", y="Latitude") +
        gg_dark() +
        theme(strip.text=element_text(color=TXT, family="Space Grotesk",
                                      face="bold", size=9),
              strip.background=element_rect(fill=PANEL, color=BRD),
              panel.spacing=unit(1,"lines"))
    } else {
      # Single latent variable: one spatial map
      ggplot(df, aes(lon, lat, fill=W1)) +
        geom_tile() +
        scale_fill_viridis_c(option="magma", name="W₁") +
        coord_equal() +
        labs(title="Spatial distribution of latent variable W₁",
             subtitle=paste0("Sites = ", nrow(coords), " raster grid cells."),
             x="Longitude", y="Latitude") +
        gg_dark()
    }
  }, bg="transparent")

  # ── Species biplot (lambda loadings) ──────────────────────
  # In jSDM, mcmc.sp[[j]] has columns: intercept, betas, lambda_1, lambda_2, ...
  # Rows = MCMC samples. Posterior mean lambda = colMeans of those columns.
  output$plot_sp_biplot <- renderPlot({
    req(rv$jsdm_mod, rv$jsdm_data)
    mod   <- rv$jsdm_mod
    spp   <- rv$jsdm_data$species
    n_sp  <- length(spp)
    # +1 for intercept
    n_beta <- length(rv$jsdm_data$env_vars) + 1

    extract_lambda <- function(col_offset) {
      tryCatch(
        sapply(seq_len(n_sp), function(j) {
          mat <- as.matrix(mod$mcmc.sp[[j]])
          col_idx <- n_beta + col_offset
          if (ncol(mat) < col_idx) return(NA_real_)
          mean(mat[, col_idx])
        }),
        error=function(e) NULL)
    }

    L1 <- extract_lambda(1)
    L2 <- extract_lambda(2)

    if (is.null(L1) || all(is.na(L1)) || is.null(L2) || all(is.na(L2))) {
      return(ggplot() +
        annotate("text", x=0.5, y=0.5,
                 label="Need >= 2 latent variables for species biplot.
Set 'Latent vars' >= 2 before running jSDM.",
                 color=TXT, size=4, hjust=0.5) +
        theme_void() + theme(plot.background=element_rect(fill=CARD, color=NA)))
    }

    df <- data.frame(species=spp, L1=L1, L2=L2,
                     dist=sqrt(L1^2 + L2^2)) %>%
      filter(!is.na(L1), !is.na(L2))

    # Symmetric axis limits with 20% padding for labels
    lim <- max(abs(c(df$L1, df$L2)), na.rm=TRUE) * 1.3

    ggplot(df) +
      # Reference cross at origin
      geom_hline(yintercept=0, color=BRD, linewidth=.5, linetype="dashed") +
      geom_vline(xintercept=0, color=BRD, linewidth=.5, linetype="dashed") +
      # Arrow per species
      geom_segment(aes(x=0, y=0, xend=L1, yend=L2, color=dist),
                   arrow=arrow(length=unit(0.2,"cm"), type="closed"),
                   linewidth=0.75, alpha=0.85) +
      # Species labels slightly beyond arrow tip
      geom_text(aes(x=L1*1.18, y=L2*1.18, label=species, color=dist),
                size=5, fontface="italic", hjust=0.5, lineheight=.9) +
      # Origin dot
      annotate("point", x=0, y=0, color=TXT, size=2) +
      scale_color_viridis_c(option="plasma", guide="none") +
      coord_equal(xlim=c(-lim, lim), ylim=c(-lim, lim)) +
      labs(title="Species biplot — latent factor loadings (λ)",
           subtitle=paste0("Arrows from origin (0,0) = correct. ",
                           "Same direction + far from centre = positively correlated."),
           x="λ₁ (affinity for latent axis 1)",
           y="λ₂ (affinity for latent axis 2)") +
      gg_dark() +
      theme(plot.subtitle=element_text(color=MUT, size=7.5))
  }, bg="transparent")

  # ── Deviance ──────────────────────────────────────────────
  output$plot_deviance <- renderPlot({
    req(rv$jsdm_mod)
    dev <- tryCatch({
      m <- as.matrix(rv$jsdm_mod$mcmc.Deviance)
      as.numeric(m[, 1])       # single column: Deviance
    }, error=function(e) NULL)
    if (is.null(dev) || !length(dev)) {
      return(ggplot() +
        annotate("text", x=0.5, y=0.5,
                 label="Deviance data unavailable.",
                 color=TXT, size=4) +
        theme_void() + theme(plot.background=element_rect(fill=CARD, color=NA)))
    }
    df <- data.frame(iter=seq_along(dev), deviance=dev)
    ggplot(df, aes(iter, deviance)) +
      geom_line(color=AMB, linewidth=.4) +
      geom_smooth(method="loess", formula=y~x,
                  color=ACC, se=FALSE, linewidth=.8) +
      labs(title="Deviance trace (loess trend in cyan)",
           x="MCMC sample", y="Deviance") + gg_dark()
  }, bg="transparent")

  # ══════════════════════════════════════════════════════════
  # DATA DOWNLOADS
  # ══════════════════════════════════════════════════════════

  output$dl_occ <- downloadHandler(
    filename = function() "biohex_occurrences.csv",
    content  = function(file) { req(rv$occ_clean); write.csv(rv$occ_clean,file,row.names=FALSE) })

  output$dl_hex_csv <- downloadHandler(
    filename = function() "biohex_diversity.csv",
    content  = function(file) {
      req(rv$polygons)
      as.data.frame(rv$polygons) %>% select(-geometry) %>% write.csv(file,row.names=FALSE) })

  output$dl_jsdm <- downloadHandler(
    filename = function() "biohex_jsdm_model.rds",
    content  = function(file) { req(rv$jsdm_mod); saveRDS(rv$jsdm_mod, file) })

  # ── Per-plot download handlers ─────────────────────────────
  save_gg <- function(file, p, w=10, h=7, dpi=250) {
    if (is.null(p)) return()
    ggplot2::ggsave(file, p, width=w, height=h, dpi=dpi, bg=CARD)
  }

  output$dl_plot_trace <- downloadHandler(
    filename=function() "jsdm_trace.png",
    content=function(file) {
      req(rv$jsdm_mod, input$conv_sp, input$conv_par)
      mod <- rv$jsdm_mod
      idx <- which(names(mod$mcmc.sp)==input$conv_sp); if(!length(idx)) idx <- 1
      ch  <- as.numeric(mod$mcmc.sp[[idx]][,input$conv_par])
      df  <- data.frame(iter=seq_along(ch), value=ch)
      save_gg(file,
        ggplot(df,aes(iter,value))+geom_line(color=ACC,linewidth=.35)+
          labs(title=paste("Trace —",input$conv_par,"—",input$conv_sp),
               x="Iteration",y="Value")+gg_dark(), h=5)
    })

  output$dl_plot_density <- downloadHandler(
    filename=function() "jsdm_posterior_density.png",
    content=function(file) {
      req(rv$jsdm_mod, input$conv_sp, input$conv_par)
      mod <- rv$jsdm_mod
      idx <- which(names(mod$mcmc.sp)==input$conv_sp); if(!length(idx)) idx <- 1
      ch  <- as.numeric(mod$mcmc.sp[[idx]][,input$conv_par])
      df  <- data.frame(value=ch)
      save_gg(file,
        ggplot(df,aes(value))+geom_density(fill=VIO,color=NA,alpha=.75)+
          geom_vline(xintercept=mean(ch),color=AMB,linetype="dashed",linewidth=.8)+
          labs(title=paste("Posterior density —",input$conv_par),x="Value",y="Density")+
          gg_dark(), h=5)
    })

  output$dl_plot_rescor <- downloadHandler(
    filename=function() "jsdm_residual_correlations.png",
    content=function(file) {
      req(rv$jsdm_mod)
      n_sp <- length(rv$jsdm_data$species)
      sz   <- max(8, min(24, n_sp * 0.35))
      png(file, width=sz*100, height=sz*100, res=150, bg=CARD)
      old <- par(bg=CARD, col.axis=MUT, col.lab=MUT, col.main=TXT, fg=BRD)
      tryCatch(jSDM::plot_residual_cor(rv$jsdm_mod, tl.cex=max(0.4, 0.9-n_sp*0.01)),
               error=function(e) { plot.new(); text(0.5,0.5,"Error",col=TXT) })
      par(old); dev.off()
    })

  output$dl_plot_resp <- downloadHandler(
    filename=function() paste0("jsdm_response_",
                                gsub(" ","_",input$resp_sp %||% "species"),".png"),
    content=function(file) {
      req(rv$jsdm_mod, rv$jsdm_data, input$resp_sp)
      mod <- rv$jsdm_mod; sp <- input$resp_sp
      idx <- which(names(mod$mcmc.sp)==sp); if(!length(idx)) idx <- 1
      n_e <- length(rv$jsdm_data$env_vars)
      bc  <- seq_len(n_e+1)
      betas <- colMeans(as.matrix(mod$mcmc.sp[[idx]])[,bc,drop=FALSE])
      ci    <- apply(as.matrix(mod$mcmc.sp[[idx]])[,bc,drop=FALSE],2,
                     function(x) quantile(x,c(.025,.975)))
      df <- tibble::tibble(param=names(betas),mean=as.numeric(betas),
                           lo=ci[1,],hi=ci[2,],dir=ifelse(mean>0,"pos","neg"))
      save_gg(file,
        ggplot(df,aes(x=reorder(param,mean),y=mean,fill=dir))+
          geom_col(alpha=.85)+
          scale_fill_manual(values=c(pos=GRN,neg=RED),guide="none")+
          geom_errorbar(aes(ymin=lo,ymax=hi),width=.25,color=TXT,linewidth=.5)+
          coord_flip()+
          labs(title=paste("Beta coefficients —",sp),
               subtitle="Posterior mean ± 95% CI",x=NULL,y="Posterior mean")+
          gg_dark(),
        h=max(5, n_e*0.32))
    })

  output$dl_plot_theta <- downloadHandler(
    filename=function() "jsdm_theta.png",
    content=function(file) {
      req(rv$jsdm_mod)
      df <- data.frame(theta=as.vector(rv$jsdm_mod$theta_latent))
      save_gg(file,
        ggplot(df,aes(theta))+geom_histogram(bins=60,fill=ACC,color=D,alpha=.85)+
          labs(title="Predicted occupancy θ",x="θ",y="Count")+gg_dark(), h=5)
    })

  output$dl_plot_latent <- downloadHandler(
    filename=function() "jsdm_latent_spatial.png",
    content=function(file) {
      req(rv$jsdm_mod, rv$jsdm_data)
      mod <- rv$jsdm_mod; coords <- rv$jsdm_data$coords
      W1  <- colMeans(as.matrix(mod$mcmc.latent[["lv_1"]]))
      lv2 <- tryCatch(as.matrix(mod$mcmc.latent[["lv_2"]]),error=function(e) NULL)
      df  <- data.frame(lon=coords$x, lat=coords$y, W1=W1)
      if (!is.null(lv2)) df$W2 <- colMeans(lv2)
      p <- if (!is.null(lv2)) {
        dl <- tidyr::pivot_longer(df,c("W1","W2"),names_to="LV",values_to="score")
        ggplot(dl,aes(lon,lat,fill=score))+geom_tile()+facet_wrap(~LV)+
          scale_fill_viridis_c(option="magma",name="Score")+
          coord_equal()+labs(x="Longitude",y="Latitude")+gg_dark()
      } else {
        ggplot(df,aes(lon,lat,fill=W1))+geom_tile()+
          scale_fill_viridis_c(option="magma",name="W₁")+
          coord_equal()+labs(x="Longitude",y="Latitude")+gg_dark()
      }
      save_gg(file, p, w=12, h=6)
    })

  output$dl_plot_dev <- downloadHandler(
    filename=function() "jsdm_deviance.png",
    content=function(file) {
      req(rv$jsdm_mod)
      dev <- as.numeric(as.matrix(rv$jsdm_mod$mcmc.Deviance)[,1])
      df  <- data.frame(iter=seq_along(dev), deviance=dev)
      save_gg(file,
        ggplot(df,aes(iter,deviance))+geom_line(color=AMB,linewidth=.4)+
          geom_smooth(method="loess",formula=y~x,color=ACC,se=FALSE,linewidth=.8)+
          labs(title="Deviance trace",x="MCMC sample",y="Deviance")+gg_dark(), h=5)
    })

  output$dl_plot_biplot <- downloadHandler(
    filename=function() "jsdm_species_biplot.png",
    content=function(file) {
      req(rv$jsdm_mod, rv$jsdm_data)
      mod <- rv$jsdm_mod; spp <- rv$jsdm_data$species
      nb  <- length(rv$jsdm_data$env_vars)+1
      L1  <- sapply(seq_along(spp),function(j) mean(as.matrix(mod$mcmc.sp[[j]])[,nb+1]))
      L2  <- sapply(seq_along(spp),function(j) mean(as.matrix(mod$mcmc.sp[[j]])[,nb+2]))
      df  <- data.frame(species=spp,L1=L1,L2=L2,dist=sqrt(L1^2+L2^2))
      lim <- max(abs(c(df$L1,df$L2)),na.rm=TRUE)*1.3
      save_gg(file,
        ggplot(df)+
          geom_hline(yintercept=0,color=BRD,linewidth=.5,linetype="dashed")+
          geom_vline(xintercept=0,color=BRD,linewidth=.5,linetype="dashed")+
          geom_segment(aes(x=0,y=0,xend=L1,yend=L2,color=dist),
                       arrow=arrow(length=unit(0.2,"cm"),type="closed"),
                       linewidth=0.75,alpha=0.85)+
          geom_text(aes(x=L1*1.18,y=L2*1.18,label=species,color=dist),
                    size=2.8,fontface="italic",hjust=0.5)+
          annotate("point",x=0,y=0,color=TXT,size=2)+
          scale_color_viridis_c(option="plasma",guide="none")+
          coord_equal(xlim=c(-lim,lim),ylim=c(-lim,lim))+
          labs(title="Species biplot — latent factor loadings",
               x="λ₁",y="λ₂")+gg_dark(),
        w=10, h=10)
    })
}

shinyApp(ui, server)
