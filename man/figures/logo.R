# This script draws man/figures/logo.png; run from the package root. The text is
# set in Linux Libertine, an open font. Before drawing, install Linux Libertine
# or its fork Libertinus Serif.
#
# A Roman aqueduct: round arches, and infrastructure. Each arch spans the
# numbers that round to the whole number under its keystone, and the piers
# stand on the ties in between, where rounding has to pick a side. The sun sets
# behind the middle arch.

sky_top <- "#1f3a5f"
sky_mid <- "#b8607a"
sky_low <- "#f0a35e"
stone <- "#dcb88a"
stone_dark <- "#b98f60"
mortar <- "#8a6440"
water <- "#2b5876"
border <- "#1f3a5f"

pts <- function(x, y) paste(sprintf("%.1f,%.1f", x, y), collapse = " ")

# Arches are s apart, keystones on the whole numbers. The opening between two
# piers has radius r, and the ring of voussoirs around it is t thick.
s <- 150
pier <- 34
r <- (s - pier) / 2
t <- 18
keys <- 260 + s * -2:2
y_spring <- 350
y_water <- 470
y_deck <- y_spring - r - t - 28
font_size <- 88

opening <- function(x) {
  sprintf(
    "M %g,%g V %g A %g,%g 0 0 1 %g,%g V %g Z",
    x - r,
    y_water,
    y_spring,
    r,
    r,
    x + r,
    y_spring,
    y_water
  )
}

wall_path <- paste(
  sprintf("M 0,%g H 520 V %g H 0 Z", y_deck, y_water),
  paste(vapply(keys, opening, ""), collapse = " ")
)

# Courses of stone, each joint offset from the one below.
courses <- local({
  y <- seq(y_deck + 18, y_water, by = 18)
  vertical <- unlist(lapply(seq_along(y), function(i) {
    x <- seq(-20, 540, by = 44) + (i %% 2) * 22
    sprintf('<line x1="%g" y1="%g" x2="%g" y2="%g"/>', x, y[i] - 18, x, y[i])
  }))
  c(sprintf('<line x1="0" y1="%g" x2="520" y2="%g"/>', y, y), vertical)
})

# Nine voussoirs per arch, so that one of them is the keystone. It is lighter
# and juts out a little.
voussoirs <- function(x) {
  a <- seq(pi, 2 * pi, length.out = 10)
  vapply(
    1:9,
    function(i) {
      key <- i == 5
      ro <- r + t + if (key) 8 else 0
      sprintf(
        paste(
          '<path d="M %.1f,%.1f L %.1f,%.1f A %g,%g 0 0 1 %.1f,%.1f',
          'L %.1f,%.1f A %g,%g 0 0 0 %.1f,%.1f Z" fill="%s"/>'
        ),
        x + r * cos(a[i]),
        y_spring + r * sin(a[i]),
        x + ro * cos(a[i]),
        y_spring + ro * sin(a[i]),
        ro,
        ro,
        x + ro * cos(a[i + 1]),
        y_spring + ro * sin(a[i + 1]),
        x + r * cos(a[i + 1]),
        y_spring + r * sin(a[i + 1]),
        r,
        r,
        x + r * cos(a[i]),
        y_spring + r * sin(a[i]),
        if (key) {
          "#f3dcb4"
        } else if (i %% 2) {
          stone
        } else {
          "#d2ac7c"
        }
      )
    },
    ""
  )
}

# Impost blocks where the arches spring from the piers.
imposts <- sprintf(
  '<rect x="%g" y="%g" width="%g" height="8"/>',
  keys[-1] - s / 2 - pier / 2 - 5,
  y_spring - 8,
  pier + 10
)

aqueduct <- c(
  sprintf(
    '<clipPath id="wall"><path d="%s" clip-rule="evenodd"/></clipPath>',
    wall_path
  ),
  sprintf('<path d="%s" fill="%s" fill-rule="evenodd"/>', wall_path, stone),
  sprintf(
    '<g clip-path="url(#wall)" stroke="%s" stroke-width="1.6" stroke-opacity="0.45">',
    mortar
  ),
  courses,
  "</g>",
  sprintf('<g stroke="%s" stroke-width="1.6">', mortar),
  unlist(lapply(keys, voussoirs)),
  "</g>",
  sprintf('<g fill="%s">', stone_dark),
  imposts,
  # the cornice along the top
  sprintf('<rect x="0" y="%g" width="520" height="12"/>', y_deck - 6),
  "</g>",
  sprintf(
    '<rect x="0" y="%g" width="520" height="4" fill="#f3dcb4"/>',
    y_deck - 6
  )
)

sun <- sprintf(
  '<circle cx="260" cy="%g" r="38" fill="url(#sun)"/>',
  y_water - 52
)

# The river under the aqueduct, with the sun's glitter on it.
river <- c(
  sprintf(
    '<rect x="0" y="%g" width="520" height="%g" fill="%s"/>',
    y_water,
    600 - y_water,
    water
  ),
  sprintf(
    paste(
      '<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="#f6c27a"',
      'stroke-width="3" stroke-linecap="round" stroke-opacity="%.2f"/>'
    ),
    260 - c(30, 22, 14, 8),
    y_water + c(10, 22, 34, 46),
    260 + c(30, 22, 14, 8),
    y_water + c(10, 22, 34, 46),
    c(0.8, 0.6, 0.45, 0.3)
  )
)

hexs <- pts(
  c(260, 511.1, 511.1, 260, 8.9, 8.9),
  c(10, 155, 445, 590, 445, 155)
)

svg <- c(
  paste(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 520 600"',
    'width="520" height="600">'
  ),
  "<defs>",
  sprintf(
    paste(
      '<linearGradient id="sky" x1="0" y1="0" x2="0" y2="1">',
      '<stop offset="0.25" stop-color="%s"/>',
      '<stop offset="0.55" stop-color="%s"/>',
      '<stop offset="0.8" stop-color="%s"/></linearGradient>'
    ),
    sky_top,
    sky_mid,
    sky_low
  ),
  paste(
    '<radialGradient id="sun"><stop offset="0" stop-color="#fff3c4"/>',
    '<stop offset="1" stop-color="#f7c45a"/></radialGradient>'
  ),
  sprintf('<clipPath id="hex"><polygon points="%s"/></clipPath>', hexs),
  "</defs>",
  '<g clip-path="url(#hex)">',
  '<rect width="520" height="600" fill="url(#sky)"/>',
  sun,
  river,
  aqueduct,
  # The word is centered on the twilight, where the blue starts to turn purple
  # (y = 172). Its letters reach 0.7 font sizes above the baseline.
  sprintf(
    paste(
      '<text x="260" y="%.1f" text-anchor="middle"',
      'font-family="Linux Libertine Display O, Linux Libertine O,',
      'Libertinus Serif, serif" font-size="%g" fill="#fbecd0">roundwork</text>'
    ),
    172 + 0.35 * font_size,
    font_size
  ),
  "</g>",
  sprintf(
    paste(
      '<polygon points="%s" fill="none" stroke="%s" stroke-width="16"',
      'stroke-linejoin="round"/>'
    ),
    hexs,
    border
  ),
  "</svg>"
)

# Write logo to disk
svg_path <- tempfile(fileext = ".svg")
writeLines(svg, svg_path)
rsvg::rsvg_png(svg_path, "man/figures/logo.png", width = 1040)
