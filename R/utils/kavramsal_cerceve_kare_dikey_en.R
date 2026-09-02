# ==========================================================
# Conceptual Framework Diagram — VERSION 3 (English)
# ----------------------------------------------------------
# READABILITY STRATEGY
#   Effective point size = fontsize x (target width / natural width)
#   Increasing fontsize also enlarges the boxes; the ratio is unchanged.
#   For this reason LABELS WERE SHORTENED: ~22 characters per line.
#   Natural width ~6.5 in -> no shrinkage at a 16 cm page width
#   -> fontsize=13 renders at nearly a true 13 pt.
#   Formula details were moved to the figure caption.
# ==========================================================

req_pkgs <- c("DiagrammeR", "DiagrammeRsvg", "rsvg", "xml2", "here")
missing_pkgs <- req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1),
                                 quietly = TRUE)]
if (length(missing_pkgs) > 0L) {
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "),
       ". Install them before running this script.", call. = FALSE)
}
library(DiagrammeR); library(DiagrammeRsvg); library(rsvg)

out_dir <- here::here("figures_standalone")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_svg <- file.path(out_dir, "kavramsal_cerceve_kare_en.svg")
out_png <- file.path(out_dir, "kavramsal_cerceve_kare_en.png")

FS_NODE  <- 13   # inside box
FS_EDGE  <- 11   # edge label
FS_CLUST <- 13   # cluster title

framework <- grViz(sprintf("
digraph spark {
  graph [layout=dot, rankdir=TB, nodesep=0.30, ranksep=0.55, newrank=true]
  node  [shape=rectangle, style=\"filled,rounded\", fontname=\"Times-Roman\",
         fontsize=%d, penwidth=2.2, margin=\"0.16,0.10\"]
  edge  [fontname=\"Times-Roman\", fontsize=%d, color=\"#404040\",
         penwidth=1.5, fontcolor=\"#303030\", arrowsize=0.8]

  // ---------- TOP: Inputs ----------
  subgraph cluster_inputs {
    label=\"Input and Parameter Pool\"; labelloc=t;
    style=dashed; color=gray55; margin=10;
    fontname=\"Times-Roman\"; fontsize=%d;

    import  [label=\"Importation Pressure\\nΛ_import,month\", fillcolor=\"#D1E8E2\"]
    traits  [label=\"Biological Traits\\na(T), EIP(T), μ_v, β, m\", fillcolor=\"#FFF2CC\"]
    climate [label=\"Climate and Demography\\nT, RH, N_h\", fillcolor=\"#D1E8E2\"]
    {rank=same; import; traits; climate}
  }

  // ---------- MIDDLE: Stochastic core ----------
  subgraph cluster_core {
    label=\"Stochastic Core\"; labelloc=t;
    style=rounded; color=\"#8EC07C\"; penwidth=2; margin=10;
    fontname=\"Times-Roman\"; fontsize=%d;

    pois [label=\"Importation Process\\nN ~ Poisson(Λ)\", fillcolor=\"#E6F5F3\"]
    pest [label=\"Local Establishment (CTMC)\\nλ̄_local = E[λ_local(EIP)]\\nP_est = P_est(λ̄_local),  I = 30\", fillcolor=\"#D9EAD3\"]
    {rank=same; pois; pest}
  }




  // ---------- BOTTOM: Risk chain ----------
  pmonth  [label=\"Monthly Threshold-Exceedance Probability\\nP_month = 1 − e^(−Λ_import·P_est)\",
           fillcolor=\"#CCE3F5\", penwidth=3]
  yearly  [label=\"Annual Threshold-Exceedance Probability\\nP_year = 1 − Π_month (1 − P_month)\", fillcolor=\"#E1D5E7\"]
  horizon [label=\"Horizon Risk\\nP_horizon (2025–2075)\\n51 years · 612 months\",
           fillcolor=\"#E1D5E7\", penwidth=3]

  // ---------- EDGES ----------
  climate -> traits [style=dotted, label=\"parameterization\"]
  climate -> pest   [style=dotted, label=\"T, RH\"]
  climate -> import [style=dotted, label=\"M_climate(t)\", color=\"#999999\", fontcolor=\"#777777\"]
  import  -> pois
  traits  -> pest   [label=\"λ_local, γ\", color=\"#E67E22\", fontcolor=\"#E67E22\"]

  pois -> pmonth [label=\"Poisson\\nthinning\", color=\"#1F6FB2\", fontcolor=\"#1F6FB2\"]
  pest -> pmonth [color=\"#1F6FB2\"]

  pmonth  -> yearly  [label=\"monthly → annual\"]
  yearly  -> horizon [label=\"51-year accumulation\"]
}
", FS_NODE, FS_EDGE, FS_CLUST, FS_CLUST))

svg_txt <- export_svg(framework)
rsvg_svg(charToRaw(svg_txt), file = out_svg)
rsvg_png(charToRaw(svg_txt), file = out_png, width = 3000)

# ---- Natural size and effective point-size report ----
d <- rsvg::rsvg_dim(charToRaw(svg_txt))
w_in <- d$width / 300; h_in <- d$height / 300
target_cm <- 16; target_in <- target_cm / 2.54
scale <- min(1, target_in / w_in)

cat(sprintf(
  "\n--- SIZE REPORT ---
Natural size    : %.2f x %.2f in  (%.1f x %.1f cm)
Target width    : %.1f cm
Scale           : %.2f
Effective size  : box %.1f pt | edge %.1f pt
%s\n",
  w_in, h_in, w_in*2.54, h_in*2.54, target_cm, scale,
  FS_NODE*scale, FS_EDGE*scale,
  if (FS_NODE*scale >= 10) "STATUS: legible (>=10 pt)"
  else "STATUS: small — shorten labels further or stack inputs vertically"))

framework
