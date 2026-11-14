# This script draws man/figures/logo.png; run from the package root. The text is
# set in Linux Libertine, an open font. Before drawing, install Linux Libertine
# or its fork Libertinus Serif.
#
# Unrounding as a magnifying glass. A number line runs across the hex in steps
# of 0.1. Under the lens, it is magnified to show what "2.5" may have been
# before rounding: anything from 2.45 to 2.55. That is unround("2.5"), which by
# default allows rounding up or down from 5, so both bounds are included.

paper <- "#f3ead8"
ink <- "#1f3a5f"
band <- "#e8955a"
border <- "#1f3a5f"

font <- 'font-family="Linux Libertine Display O, Linux Libertine O, Libertinus Serif, serif"'

cx <- 260 # the lens, centered on 2.5
cy <- 360
lr <- 122
zoom <- 2

# The number line around 2.5 at a given scale (pixels per 0.1), with ticks
# every `minor` and labels every `major`, both in tenths.
number_line <- function(scale, minor, major, label_digits, size) {
  at <- seq(-5, 5, by = minor) # in tenths away from 2.5, i.e. units of 0.1
  at <- at[abs(at) * scale <= 300]
  x <- cx + at * scale
  # labels only where they fit inside the hex
  is_major <- abs(at / major - round(at / major)) < 1e-9 & abs(at * scale) < 200
  c(
    sprintf(
      '<line x1="0" y1="%g" x2="520" y2="%g" stroke="%s" stroke-width="%g"/>',
      cy,
      cy,
      ink,
      size / 8
    ),
    sprintf(
      '<line x1="%.1f" y1="%g" x2="%.1f" y2="%g" stroke="%s" stroke-width="%g"/>',
      x,
      cy - ifelse(is_major, size * 0.5, size * 0.25),
      x,
      cy + ifelse(is_major, size * 0.5, size * 0.25),
      ink,
      size / 10
    ),
    sprintf(
      '<text x="%.1f" y="%g" text-anchor="middle" %s font-size="%g" fill="%s">%s</text>',
      x[is_major],
      cy + size * 1.5,
      font,
      size,
      ink,
      formatC(2.5 + at[is_major] / 10, format = "f", digits = label_digits)
    )
  )
}

# Outside the lens, the line is faint, so the eye goes to what the lens shows.
outer <- c('<g opacity="0.4">', number_line(80, 1, 1, 1, 22), "</g>")

# Under the lens: the interval [2.45, 2.55], closed on both ends.
s_in <- 80 * zoom
lo <- cx - 0.5 * s_in
hi <- cx + 0.5 * s_in
inner <- c(
  sprintf(
    '<rect x="%.1f" y="%g" width="%.1f" height="16" fill="%s"/>',
    lo,
    cy - 8,
    hi - lo,
    band
  ),
  number_line(s_in, 0.1, 0.5, 2, 26),
  sprintf('<circle cx="%.1f" cy="%g" r="9" fill="%s"/>', c(lo, hi), cy, ink)
)

# The magnifying glass is brass, with a rosewood handle. Metal and wood are
# shaded with gradients: the rim's runs diagonally across it, and the
# handle's runs across its width, so each part looks round.
stops <- function(offsets, colors) {
  paste(
    sprintf('<stop offset="%g" stop-color="%s"/>', offsets, colors),
    collapse = ""
  )
}
brass_bands <- c("#fbe9b0", "#d6a64a", "#8c611e", "#e6c06a", "#6a4512")
defs <- c(
  sprintf(
    paste0(
      '<linearGradient id="rim" gradientUnits="userSpaceOnUse" x1="%g" y1="%g" x2="%g" y2="%g">',
      "%s</linearGradient>"
    ),
    cx - lr,
    cy - lr,
    cx + lr,
    cy + lr,
    stops(c(0, 0.3, 0.5, 0.7, 1), brass_bands)
  ),
  # the inner lip of the rim catches light from the other side
  sprintf(
    paste0(
      '<linearGradient id="lip" gradientUnits="userSpaceOnUse" x1="%g" y1="%g" x2="%g" y2="%g">',
      "%s</linearGradient>"
    ),
    cx + lr,
    cy + lr,
    cx - lr,
    cy - lr,
    stops(c(0, 0.3, 0.5, 0.7, 1), brass_bands)
  ),
  sprintf(
    '<linearGradient id="brass" x1="0" y1="0" x2="0" y2="1">%s</linearGradient>',
    stops(
      c(0, 0.25, 0.45, 0.8, 1),
      c("#7a5418", "#f4dc95", "#d9a948", "#8a5f1c", "#5e3f10")
    )
  ),
  sprintf(
    '<linearGradient id="wood" x1="0" y1="0" x2="0" y2="1">%s</linearGradient>',
    stops(c(0, 0.3, 0.55, 1), c("#2b1810", "#8a5634", "#4a2a18", "#1e110a"))
  ),
  # the glass is a little darker toward its edge, where it is thinnest
  sprintf(
    paste0(
      '<radialGradient id="glass" gradientUnits="userSpaceOnUse" cx="%g" cy="%g" r="%g"',
      ' fx="%g" fy="%g">%s</radialGradient>'
    ),
    cx,
    cy,
    lr,
    cx - 30,
    cy - 30,
    stops(c(0, 0.75, 1), c("#f4f8f9", "#e1ecf0", "#c7d9e0"))
  ),
  '<filter id="shadow" x="-20%" y="-20%" width="140%" height="140%"><feGaussianBlur stdDeviation="6"/></filter>'
)

# The handle, drawn along the x-axis from the center and then turned to the
# lower right. A narrow brass neck leaves the rim, then a collar with two
# turned ridges, then the wood, which swells slightly.
x0 <- cx + lr + 42
handle_shape <- c(
  sprintf('<rect x="%g" y="%g" width="24" height="16"/>', cx + lr, cy - 8),
  sprintf(
    '<rect x="%g" y="%g" width="22" height="30" rx="4"/>',
    cx + lr + 20,
    cy - 15
  ),
  sprintf(
    paste(
      '<path d="M %g,%g C %g,%g %g,%g %g,%g L %g,%g C %g,%g %g,%g %g,%g Z"/>'
    ),
    x0,
    cy - 13,
    x0 + 40,
    cy - 19,
    x0 + 100,
    cy - 19,
    x0 + 160,
    cy - 16,
    x0 + 160,
    cy + 16,
    x0 + 100,
    cy + 19,
    x0 + 40,
    cy + 19,
    x0,
    cy + 13
  )
)
handle_shape <- sub("/>$", ' fill="url(#%s)"/>', handle_shape)
handle <- c(
  sprintf('<g transform="rotate(45 %g %g)">', cx, cy),
  sprintf(handle_shape, c("brass", "brass", "wood")),
  # ridges on the collar
  sprintf(
    '<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="%s" stroke-width="%g"/>',
    cx + lr + c(27, 28.5, 35, 36.5),
    cy - 15,
    cx + lr + c(27, 28.5, 35, 36.5),
    cy + 15,
    c("#5e3f10", "#fbe9b0", "#5e3f10", "#fbe9b0"),
    c(1.6, 1, 1.6, 1)
  ),
  # a thin brass ring where the wood begins
  sprintf(
    '<rect x="%g" y="%g" width="5" height="27" fill="url(#brass)"/>',
    x0 - 1,
    cy - 13.5
  ),
  "</g>"
)

# A soft shadow on the paper, cast down and to the right.
shadow <- c(
  '<g filter="url(#shadow)" fill="#3a2a10" fill-opacity="0.28" transform="translate(7,9)">',
  sprintf('<circle cx="%g" cy="%g" r="%g"/>', cx, cy, lr + 9),
  sprintf('<g transform="rotate(45 %g %g)">', cx, cy),
  gsub(' fill="url\\(#%s\\)"', "", handle_shape),
  "</g></g>"
)

# An arc of the circle of radius rr around the lens, from angle a1 to a2.
arc <- function(rr, a1, a2, style) {
  sprintf(
    '<path d="M %.1f,%.1f A %g,%g 0 0 1 %.1f,%.1f" fill="none" %s/>',
    cx + rr * cos(a1),
    cy + rr * sin(a1),
    rr,
    rr,
    cx + rr * cos(a2),
    cy + rr * sin(a2),
    style
  )
}

lens <- c(
  sprintf(
    '<clipPath id="lens"><circle cx="%g" cy="%g" r="%g"/></clipPath>',
    cx,
    cy,
    lr
  ),
  sprintf('<circle cx="%g" cy="%g" r="%g" fill="url(#glass)"/>', cx, cy, lr),
  '<g clip-path="url(#lens)">',
  inner,
  # the rim's shadow on the inside of the glass
  sprintf(
    '<circle cx="%g" cy="%g" r="%g" fill="none" stroke="#1f3a5f" stroke-opacity="0.1" stroke-width="12"/>',
    cx,
    cy,
    lr - 2
  ),
  # glints on the curved glass: a long one at the upper left, a short one
  # opposite
  arc(
    lr - 22,
    1.12 * pi,
    1.38 * pi,
    'stroke="#fff" stroke-width="7" stroke-linecap="round" stroke-opacity="0.75"'
  ),
  arc(
    lr - 22,
    1.42 * pi,
    1.46 * pi,
    'stroke="#fff" stroke-width="7" stroke-linecap="round" stroke-opacity="0.75"'
  ),
  arc(
    lr - 18,
    0.15 * pi,
    0.3 * pi,
    'stroke="#fff" stroke-width="4" stroke-linecap="round" stroke-opacity="0.4"'
  ),
  "</g>",
  # the rim: a broad band, a stepped lip inside it, and fine dark edges
  sprintf(
    '<circle cx="%g" cy="%g" r="%g" fill="none" stroke="url(#rim)" stroke-width="16"/>',
    cx,
    cy,
    lr + 2
  ),
  sprintf(
    '<circle cx="%g" cy="%g" r="%g" fill="none" stroke="url(#lip)" stroke-width="4"/>',
    cx,
    cy,
    lr - 6
  ),
  sprintf(
    '<circle cx="%g" cy="%g" r="%g" fill="none" stroke="#4a300c" stroke-width="1.2" stroke-opacity="0.7"/>',
    cx,
    cy,
    c(lr + 10, lr - 4, lr - 8)
  ),
  # light running along the top of the rim
  arc(
    lr + 5,
    1.05 * pi,
    1.55 * pi,
    'stroke="#fff8dc" stroke-width="2" stroke-linecap="round" stroke-opacity="0.7"'
  )
)

pts <- function(x, y) paste(sprintf("%.1f,%.1f", x, y), collapse = " ")
hexs <- pts(c(260, 511.1, 511.1, 260, 8.9, 8.9), c(10, 155, 445, 590, 445, 155))

svg <- c(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 520 600" width="520" height="600">',
  "<defs>",
  defs,
  "</defs>",
  sprintf('<clipPath id="hex"><polygon points="%s"/></clipPath>', hexs),
  '<g clip-path="url(#hex)">',
  sprintf('<rect width="520" height="600" fill="%s"/>', paper),
  outer,
  shadow,
  handle,
  lens,
  sprintf(
    '<text x="260" y="200" text-anchor="middle" %s font-size="76" fill="%s">roundwork</text>',
    font,
    ink
  ),
  "</g>",
  sprintf(
    '<polygon points="%s" fill="none" stroke="%s" stroke-width="16" stroke-linejoin="round"/>',
    hexs,
    border
  ),
  "</svg>"
)

# Write logo to disk
svg_path <- tempfile(fileext = ".svg")
writeLines(svg, svg_path)
rsvg::rsvg_png(svg_path, "man/figures/logo.png", width = 1040)
