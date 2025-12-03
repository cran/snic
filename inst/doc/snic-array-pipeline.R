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
if (!requireNamespace("jpeg", quietly = TRUE)) {
  stop("Install the 'jpeg' package to run this vignette.")
}

# Example RGB image shipped with the package (values in [0, 1])
img_path <- system.file("demo-jpeg/clownfish.jpeg", package = "snic",
  mustWork = TRUE)
rgb <- jpeg::readJPEG(img_path)
dim(rgb) # rows, cols, channels

## ----array-image-rgb, fig.width = 8, fig.height = 6, fig.cap = "Figure 1 - An RGB image of a clownfish."----
snic_plot(rgb, r = 1, g = 2, b = 3)

## ----rgb-to-lab---------------------------------------------------------------
dims <- dim(rgb)
dim(rgb) <- c(dims[1] * dims[2], dims[3])

lab <- convertColor(rgb, from = "sRGB", to = "Lab", scale.out = 1 / 255)

# Back to canonical dimensions
dim(rgb) <- dims
dim(lab) <- dims

## ----array-image-lab-rgb, fig.width = 7, fig.height = 4, fig.cap = "Figure 2 - Comparison of the L, a, b channels (Lab color space) with the R, G, B channels (RGB color space)."----

# Grayscale palette
gray <- grDevices::gray.colors(256)

# Prepare figure layout
op <- par(mfrow = c(2, 3))

# Plot Lab channels
snic_plot(lab, band = 1, col = gray, main = "L")
snic_plot(lab, band = 2, col = gray, main = "a")
snic_plot(lab, band = 3, col = gray, main = "b")

# Plot RGB channels
snic_plot(rgb, band = 1, col = gray, main = "R")
snic_plot(rgb, band = 2, col = gray, main = "G")
snic_plot(rgb, band = 3, col = gray, main = "B")

par(op)

## ----pipeline-step-1----------------------------------------------------------
seeds <- snic_grid(lab, type = "rectangular", spacing = 22L)

## ----pipeline-step-2----------------------------------------------------------
segs <- snic(lab, seeds, compactness = 0.25)

## ----array-image-seg, fig.width = 8, fig.height = 6, fig.cap = "Figure 3 - The clownfish image with superpixel segmentation boundaries overlaid."----
snic_plot(rgb, r = 1, g = 2, b = 3, seg = segs)

## ----segs---------------------------------------------------------------------
dim(segs)

## ----grid-types---------------------------------------------------------------

seeds_rect <- snic_grid(rgb, type = "rectangular", spacing = 22L)
seeds_diam <- snic_grid(rgb, type = "diamond", spacing = 22L)
seeds_hex  <- snic_grid(rgb, type = "hexagonal", spacing = 22L)

# Set seed for reproducibility
set.seed(42)
seeds_rand <- snic_grid(rgb, type = "random", spacing = 22L)

## ----grid-types-segments------------------------------------------------------
grids <- list(
  seeds_rect, seeds_diam, seeds_hex, seeds_rand
)

results <- lapply(grids, function(seeds) {
  list(
    snic(rgb, seeds, compactness = 0.1),
    snic(rgb, seeds, compactness = 0.5)
  )
})

## ----array-image-grid-types, fig.width = 8, fig.height = 12, fig.cap = "Figure 4 - Superpixel segmentation results on the clownfish image. Rows correspond to different seed grid types (rectangular, diamond, hexagonal, random) and columns to different compactness values (0.1 and 0.5)."----

# Prepare figure layout
op <- par(mfrow = c(4, 2), oma = c(2, 2, 2, 0))

# Plot results
for (res in results) {
  snic_plot(rgb, r = 1, g = 2, b = 3, seg = res[[1]], mar = c(1, 0, 0, 0))
  snic_plot(rgb, r = 1, g = 2, b = 3, seg = res[[2]], mar = c(1, 0, 0, 0))
}

mtext("Compactness = 0.1", side = 3, outer = TRUE, at = 0.25, line = 0.5)
mtext("Compactness = 0.5", side = 3, outer = TRUE, at = 0.75, line = 0.5)
mtext("Rectangular", side = 2, outer = TRUE, at = 3.5 / 4, line = 0.5, las = 3)
mtext("Diamond", side = 2, outer = TRUE, at = 2.5 / 4, line = 0.5, las = 3)
mtext("Hexagonal", side = 2, outer = TRUE, at = 1.5 / 4, line = 0.5, las = 3)
mtext("Random", side = 2, outer = TRUE, at = 0.5 / 4, line = 0.5, las = 3)

par(op)

