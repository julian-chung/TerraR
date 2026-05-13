# =============================================================
# TerraR: Export Web Assets
# Script 04: Generate README thumbnails and website thumbnail
# =============================================================
#
# Produces downscaled and cropped versions of the rendered
# spike maps for use in the GitHub README, devlog writeup,
# and on julianchung.com.
#
# PREREQUISITES:
#   Run 02_render_maps.R first to generate the full-res PNGs.
#   Requires the magick package: install.packages("magick")
#
# OUTPUTS:
#   assets/readme-{state}.png        ~1400px wide — GitHub README hero images (3 states)
#   assets/devlog-{state}.png        ~1200px wide — all 8 states for devlog writeup
#   assets/thumbnail-victoria.png    1200x630px   — website OG image / thumbnail
# =============================================================

library(magick)

dir.create("assets", showWarnings = FALSE)

# --- Settings ---

README_WIDTH  <- 1400L   # px — GitHub content column max
DEVLOG_WIDTH  <- 1200L   # px — Quarto devlog embed width
THUMB_W       <- 1200L   # px — OG / website thumbnail width
THUMB_H       <- 630L    # px — OG / website thumbnail height (16:9-ish)

# Victoria crop: Melbourne sits in the lower-centre of the state map.
# These offsets are proportions of the full image (0–1), applied before
# the final thumbnail resize. Adjust if the crop needs reframing.
# (0, 0) is top-left; crop box is (left, top, right, bottom) as proportions.
VIC_CROP <- list(left = 0.25, top = 0.40, right = 0.85, bottom = 0.95)

# --- State file lookup ---
# Maps each state name to its rendered PNG slug and a short output label.

all_states <- list(
  list(name = "New South Wales",             slug = "New-South-Wales",            label = "nsw"),
  list(name = "Victoria",                    slug = "Victoria",                   label = "victoria"),
  list(name = "Queensland",                  slug = "Queensland",                 label = "qld"),
  list(name = "South Australia",             slug = "South-Australia",            label = "sa"),
  list(name = "Western Australia",           slug = "Western-Australia",          label = "wa"),
  list(name = "Tasmania",                    slug = "Tasmania",                   label = "tasmania"),
  list(name = "Northern Territory",          slug = "Northern-Territory",         label = "nt"),
  list(name = "Australian Capital Territory",slug = "Australian-Capital-Territory",label = "act")
)

# =============================================================
# README hero images — 3 most striking states, 1400px wide
# =============================================================

hero_labels <- c("nsw", "victoria", "tasmania")

message("--- README hero images ---")
for (state in all_states) {
  if (!state$label %in% hero_labels) next

  infile  <- file.path("outputs/images", paste0("state-spike-map-", state$slug, ".png"))
  outfile <- file.path("assets", paste0("readme-", state$label, ".png"))

  message("Resizing: ", state$name)
  img <- magick::image_read(infile)
  img <- magick::image_resize(img, magick::geometry_size_pixels(width = README_WIDTH))
  magick::image_write(img, path = outfile, format = "png")
  message("  -> ", outfile, " (", magick::image_info(img)$width, "x", magick::image_info(img)$height, ")")
}

# =============================================================
# Devlog images — all 8 states, 1200px wide
# =============================================================

message("--- Devlog images ---")
for (state in all_states) {
  infile  <- file.path("outputs/images", paste0("state-spike-map-", state$slug, ".png"))
  outfile <- file.path("assets", paste0("devlog-", state$label, ".png"))

  message("Resizing: ", state$name)
  img <- magick::image_read(infile)
  img <- magick::image_resize(img, magick::geometry_size_pixels(width = DEVLOG_WIDTH))
  magick::image_write(img, path = outfile, format = "png")
  message("  -> ", outfile, " (", magick::image_info(img)$width, "x", magick::image_info(img)$height, ")")
}

# =============================================================
# Victoria website thumbnail — crop to Melbourne region then resize
# =============================================================

message("--- Website thumbnail ---")

vic_full <- magick::image_read("outputs/images/state-spike-map-Victoria.png")
info     <- magick::image_info(vic_full)
W        <- info$width
H        <- info$height

crop_x <- round(VIC_CROP$left  * W)
crop_y <- round(VIC_CROP$top   * H)
crop_w <- round((VIC_CROP$right  - VIC_CROP$left) * W)
crop_h <- round((VIC_CROP$bottom - VIC_CROP$top)  * H)

message("  Crop region: ", crop_w, "x", crop_h, " at (", crop_x, ",", crop_y, ")")

vic_crop  <- magick::image_crop(vic_full,
  magick::geometry_area(width = crop_w, height = crop_h, x_off = crop_x, y_off = crop_y))
vic_thumb <- magick::image_resize(vic_crop,
  magick::geometry_size_pixels(width = THUMB_W, height = THUMB_H, preserve_aspect = FALSE))

magick::image_write(vic_thumb, path = "assets/thumbnail-victoria.png", format = "png")
message("  -> assets/thumbnail-victoria.png (", THUMB_W, "x", THUMB_H, ")")

message("Done. All assets saved to assets/")
