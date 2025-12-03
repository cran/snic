## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.retina = 1,
  quality = 0.3,
  dpi = 50
)
library(snic)

## ----load-imagery-------------------------------------------------------------
library(terra)

data_dir <- system.file("demo-geotiff", package = "snic", mustWork = TRUE)
band_files <- file.path(
  data_dir,
  c(
    "S2_20LMR_B02_20220630.tif",
    "S2_20LMR_B04_20220630.tif",
    "S2_20LMR_B08_20220630.tif",
    "S2_20LMR_B12_20220630.tif"
  )
)

s2 <- terra::rast(band_files)

names(s2) <- c("B02", "B04", "B08", "B12")
s2

## ----plot-s2, fig.width = 8, fig.height = 6, fig.cap = "Figure 1 - A false-color composite of a Sentinel-2 scene."----
snic_plot(s2, r = "B08", g = "B04", b = "B02")

## ----seeds--------------------------------------------------------------------
seeds_rect <- snic_grid(s2, type = "rectangular", spacing = 24, padding = 2)

head(seeds_rect)

## ----run-snic-----------------------------------------------------------------
seg_rect <- snic(s2, seeds_rect, compactness = 0.3)
seg_rect

## ----plot-seg, fig.width = 8, fig.height = 6, fig.cap = "Figure 2 - The Sentinel-2 false-color composite with superpixel segmentation boundaries overlaid in yellow and seed points marked with cyan crosses."----
snic_plot(
  s2,
  r = "B08", g = "B04", b = "B02",
  stretch = "lin",
  seeds = seeds_rect,
  seeds_plot_args = list(pch = 3, col = "#00FFFF", cex = 0.6),
  seg = seg_rect,
  seg_plot_args = list(border = "#FFD700", col = NA, lwd = 0.5)
)

## ----grid-types---------------------------------------------------------------
seeds_rect <- snic_grid(s2, type = "rectangular", spacing = 26)
seeds_diam <- snic_grid(s2, type = "diamond", spacing = 26)
seeds_hex  <- snic_grid(s2, type = "hexagonal", spacing = 26)

set.seed(42)
seeds_rand <- snic_grid(s2, type = "random", spacing = 26)

grids <- list(
  seeds_rect, seeds_diam,
  seeds_hex, seeds_rand
)

results <- lapply(grids, function(seeds) {
  list(
    snic(s2, seeds, compactness = 0.1),
    snic(s2, seeds, compactness = 0.4)
  )
})

## ----grid-types-plot, fig.width = 8, fig.height = 12, fig.cap = "Figure 3 - Superpixel segmentation results on the Sentinel-2 scene. Rows correspond to different seed grid types (rectangular, diamond, hexagonal, random) and columns correspond to different compactness values (0.1 and 0.4)."----
op <- par(mfrow = c(4, 2), oma = c(2, 2, 2, 0))

for (res in results) {
  snic_plot(
    s2,
    r = "B08", g = "B04", b = "B02",
    seg = res[[1]],
    seg_plot_args = list(border = "#FFFFFF", col = NA, lwd = 0.4),
    mar = c(1, 0, 0, 0)
  )
  snic_plot(
    s2,
    r = "B08", g = "B04", b = "B02",
    stretch = "lin",
    seg = res[[2]],
    seg_plot_args = list(border = "#FFFFFF", col = NA, lwd = 0.4),
    mar = c(1, 0, 0, 0)
  )
}

mtext("Compactness = 0.1", side = 3, outer = TRUE, at = 0.25, line = 0.5)
mtext("Compactness = 0.4", side = 3, outer = TRUE, at = 0.75, line = 0.5)
mtext("Rectangular", side = 2, outer = TRUE, at = 3.5 / 4, line = 0.5, las = 3)
mtext("Diamond", side = 2, outer = TRUE, at = 2.5 / 4, line = 0.5, las = 3)
mtext("Hexagonal", side = 2, outer = TRUE, at = 1.5 / 4, line = 0.5, las = 3)
mtext("Random", side = 2, outer = TRUE, at = 0.5 / 4, line = 0.5, las = 3)

par(op)

## ----hex-seeds-res------------------------------------------------------------
seeds_hex <- snic_grid(
  s2,
  type = "hexagonal",
  spacing = 50,
  padding = 5
)

## ----hex-seeds-res-plot, fig.width = 8, fig.height = 6.5, fig.cap = "Figure 4 - Superpixel segmentation results on the Sentinel-2 scene at different spatial resolutions (20 m, 40 m, 80 m, 160 m)."----

op <- par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))

agg_facts <- c(1L, 2L, 4L, 8L)
for (fact in agg_facts) {
  s2_agg <- terra::aggregate(s2, fact = fact, fun = "mean", na.rm = TRUE)
  seg_agg <- snic(s2_agg, seeds_hex, compactness = 0.2)
  snic_plot(
    s2_agg,
    r = "B08", g = "B04", b = "B02",
    seg = seg_agg,
    seg_plot_args = list(border = "white", col = NA, lwd = 0.4),
    main = sprintf("resolution %d m", fact * 20)
  )
}

par(op)

