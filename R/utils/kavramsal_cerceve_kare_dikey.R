# ==========================================================
# Kavramsal Çerçeve Diyagramı — SÜRÜM 3
# ----------------------------------------------------------
# OKUNURLUK STRATEJİSİ
#   Etkin punto = fontsize × (hedef genişlik / doğal genişlik)
#   fontsize'ı büyütmek kutuları da büyütür; oran değişmez.
#   Bu nedenle ETIKETLER KISALTILDI: satır başına ~22 karakter.
#   Doğal genişlik ~6,5 inç -> 16 cm sayfada küçültme yok
#   -> fontsize=13 neredeyse tam 13 punto olarak görünür.
#   Formül ayrıntıları şekil altı açıklamaya taşındı.
# ==========================================================

req_pkgs <- c("DiagrammeR", "DiagrammeRsvg", "rsvg", "xml2")
for (p in req_pkgs)
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(DiagrammeR); library(DiagrammeRsvg); library(rsvg)

out_svg <- "kavramsal_cerceve_kare.svg"
out_png <- "kavramsal_cerceve_kare.png"

FS_NODE  <- 13   # kutu içi
FS_EDGE  <- 11   # ok etiketi
FS_CLUST <- 13   # küme başlığı

kavram <- grViz(sprintf("
digraph spark {
  graph [layout=dot, rankdir=TB, nodesep=0.30, ranksep=0.55, newrank=true]
  node  [shape=rectangle, style=\"filled,rounded\", fontname=\"Times-Roman\",
         fontsize=%d, penwidth=2.2, margin=\"0.16,0.10\"]
  edge  [fontname=\"Times-Roman\", fontsize=%d, color=\"#404040\",
         penwidth=1.5, fontcolor=\"#303030\", arrowsize=0.8]

  // ---------- ÜST: Girdiler ----------
  subgraph cluster_inputs {
    label=\"Girdi ve Parametre Havuzu\"; labelloc=t;
    style=dashed; color=gray55; margin=10;
    fontname=\"Times-Roman\"; fontsize=%d;

    import  [label=\"İthalat Baskısı\\nΛ_ithal,ay\", fillcolor=\"#D1E8E2\"]
    traits  [label=\"Biyolojik Özellikler\\na(T), EIP(T), μ_v, β, m\", fillcolor=\"#FFF2CC\"]
    climate [label=\"İklim ve Demografi\\nT, RH, N_h\", fillcolor=\"#D1E8E2\"]
    {rank=same; import; traits; climate}
  }

  // ---------- ORTA: Stokastik çekirdek ----------
  subgraph cluster_core {
    label=\"Stokastik Çekirdek\"; labelloc=t;
    style=rounded; color=\"#8EC07C\"; penwidth=2; margin=10;
    fontname=\"Times-Roman\"; fontsize=%d;

    pois [label=\"İthalat Süreci\\nN ~ Poisson(Λ)\", fillcolor=\"#E6F5F3\"]
    pest [label=\"Yerel Tutunma (CTMC)\\nλ̄_local = E[λ_local(EIP)]\nP_est = P_est(λ̄_local),  I = 30\", fillcolor=\"#D9EAD3\"]
    {rank=same; pois; pest}
  }
  
  
  

  // ---------- ALT: Risk zinciri ----------
  pmonth  [label=\"Aylık Majör Olasılık\\np_ay,major = 1 − e^(−Λ_ithal·P_est)\",
           fillcolor=\"#CCE3F5\", penwidth=3]
  yearly  [label=\"Yıllık Eşik-Aşımı Olasılığ\\np_yıl = 1 − Π_ay (1 − p_ay)\", fillcolor=\"#E1D5E7\"]
  horizon [label=\"Ufuk Riski\\nP_ufuk (2025–2075)\\n51 yıl · 612 ay\",
           fillcolor=\"#E1D5E7\", penwidth=3]

  // ---------- OKLAR ----------
  climate -> traits [style=dotted, label=\"parametreleme\"]
  climate -> pest   [style=dotted, label=\"T, RH\"]
  climate -> import [style=dotted, label=\"M_iklim(t)\", color=\"#999999\", fontcolor=\"#777777\"]
  import  -> pois
  traits  -> pest   [label=\"λ_yerel, γ\", color=\"#E67E22\", fontcolor=\"#E67E22\"]

  pois -> pmonth [label=\"Poisson\\ninceltmesi\", color=\"#1F6FB2\", fontcolor=\"#1F6FB2\"]
  pest -> pmonth [color=\"#1F6FB2\"]

  pmonth  -> yearly  [label=\"aylık → yıllık\"]
  yearly  -> horizon [label=\"51 yıl birikim\"]
}
", FS_NODE, FS_EDGE, FS_CLUST, FS_CLUST))

svg_txt <- export_svg(kavram)
rsvg_svg(charToRaw(svg_txt), file = out_svg)
rsvg_png(charToRaw(svg_txt), file = out_png, width = 3000)

# ---- Doğal boyut ve etkin punto raporu ----
d <- rsvg::rsvg_dim(charToRaw(svg_txt))
w_in <- d$width / 96; h_in <- d$height / 96
hedef_cm <- 16; hedef_in <- hedef_cm / 2.54
olcek <- min(1, hedef_in / w_in)

cat(sprintf(
  "\n--- BOYUT RAPORU ---
Doğal boyut   : %.2f x %.2f inç  (%.1f x %.1f cm)
Hedef genişlik: %.1f cm
Ölçek         : %.2f
Etkin punto   : kutu %.1f pt | ok %.1f pt
%s\n",
  w_in, h_in, w_in*2.54, h_in*2.54, hedef_cm, olcek,
  FS_NODE*olcek, FS_EDGE*olcek,
  if (FS_NODE*olcek >= 10) "DURUM: okunur (>=10 pt)"
  else "DURUM: kucuk — etiketleri daha da kisalt veya girdileri dikey istifle"))

kavram