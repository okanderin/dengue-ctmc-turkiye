# Gerekli paketler
required_packages <- c("DiagrammeR", "DiagrammeRsvg", "rsvg", "Cairo",
                       "png", "here")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "),
       ". Install them before running this script.", call. = FALSE)
}

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(Cairo)


stokastik_gecis_diyagram <- grViz("
digraph ctmc_model {
  graph [layout = dot, rankdir = LR]

  node [shape = circle, style = filled, fontname = Times, fontsize = 14]

   edge  [fontname=\"DejaVu Sans\"]

  // Absorban durum sol üstte
  I0 [label = 'I = 0\\nAbsorbing', fillcolor = '#FAD7D7', color = '#CC0000', pos='0,2!']

  // Diğer durumlar
  I1 [label = 'I = 1', fillcolor = '#D1E8E2', color = '#2E8B57']
  I2 [label = 'I = 2', fillcolor = '#D1E8E2', color = '#2E8B57']
  In [label = 'I = n)', fillcolor = '#D1E8E2', color = '#2E8B57']
  Itau [label = 'I ≥ τ\\nAutochthonous \nestablishment', fillcolor = '#D9EAD3', color = '#2E8B57', shape = doublecircle]

  start [shape = point, width = 0, label = '']

  // Başlangıç
  start -> I1 [label = '(t=0)', color = '#1F4E79']

  // Geçişler oklar üzerinde denklemlerle
  I1 -> I0 [label = 'μ₁ ', color = '#CC0000']
  I1 -> I2 [label = 'λ₁ ', color = '#2E8B57']
  I2 -> I1 [label = 'μ₂', color = '#CC0000']
  I2 -> In [label = 'λ₂ ', color = '#2E8B57']
  In -> Itau [label = 'λₙ', color = '#2E8B57']
  In -> I2 [label = 'μₙ ', color = '#CC0000']

  // Interstate döngüler
  I1 -> I1 [label = 'hold ~ Exp(q₁)', color = '#9A9A9A', style=dashed]
  I2 -> I2 [label = 'hold ~ Exp(q₂)', color = '#9A9A9A', style=dashed]
  In -> In [label = 'hold ~ Exp(qₙ)', color = '#9A9A9A', style=dashed]
}
")

stokastik_gecis_diyagram





svg_data <- export_svg(stokastik_gecis_diyagram)
bitmap_data <- rsvg(charToRaw(svg_data), width = 6000)
out_png <- here::here("figures_standalone", "stokastik_gecis_diyagram_final.png")
png::writePNG(bitmap_data, target = out_png, dpi = 300)
