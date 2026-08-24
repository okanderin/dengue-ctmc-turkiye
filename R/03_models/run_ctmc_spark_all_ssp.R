# ============================================================
# run_ctmc_spark_all_ssp.R
# ctmc_spark.R (Jensen-DÜZELTMELİ referans koşum) 3 SSP için çalıştırır.
# Her SSP çıktısı: outputs/<ssp>/model_results/ctmc_spark_horizon_2025_2075.csv
# (rep-suffix'SİZ) — §6.1.2 Jensen sapma tablosunu besler.
#
# ÖNEMLİ:
#  - TEMİZ bir R oturumunda çalıştırın. Aynı oturumda daha önce
#    ctmc_spark_monte_carlo.R source EDİLMİŞSE, run_ctmc_spark() fonksiyon-adı
#    çakışması olabilir (baştaki 'vectbl' çökmesinin nedeni buydu).
#    Bu wrapper'ı taze oturumda, proje kökünden çalıştırın.
#  - ctmc_spark.R kendi içinde init.R + parameter_functions.R yükler ve
#    dosya sonundaki entrypoint'i (identical(environment(), globalenv()) TRUE
#    iken) otomatik koşar. Bu yüzden local=FALSE ile global'e source ediyoruz.
#
# Çalıştırma:
#   setwd("~/drive/000000_tezim/0000_tezim/r_project_tez")   # proje kökü
#   source("run_ctmc_spark_all_ssp.R", encoding = "UTF-8")
# ============================================================

SSP_LIST <- c("ssp126", "ssp245", "ssp585")

for (s in SSP_LIST) {
  Sys.setenv(SSP_SCENARIO = s)
  message("\n########## ctmc_spark.R (Jensen ref) — ", s, " — ",
          format(Sys.time(), "%H:%M"), " ##########")
  # local = FALSE: entrypoint koşulunun (globalenv) TRUE olması için şart
  source("R/03_models/ctmc_spark.R", encoding = "UTF-8", local = FALSE)
  message("########## ", s, " TAMAM — ", format(Sys.time(), "%H:%M"), " ##########")
}

message("\n===== ctmc_spark.R TÜM SSP'ler BİTTİ =====")
message("Çıktılar: outputs/<ssp>/model_results/ctmc_spark_horizon_2025_2075.csv")

# ---- (İsteğe bağlı) hızlı doğrulama: 3 SSP horizon dosyası oluştu mu? ----
for (s in SSP_LIST) {
  f <- file.path("outputs", s, "model_results", "ctmc_spark_horizon_2025_2075.csv")
  message(sprintf("  %s : %s", s, if (file.exists(f)) "✓ var" else "✗ YOK"))
}
