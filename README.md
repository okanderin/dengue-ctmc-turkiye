# Dengue threshold-crossing risk in Türkiye under CMIP6 scenarios — reproducibility package

This repository contains the R pipeline, processed inputs, canonical model outputs, and documentation for the dengue CTMC analysis reported in Okan Derin's 2026 doctoral thesis, *“Türkiye'de Aedes Kaynaklı Arbovirüs Hassasiyeti: İklim Değişikliği Senaryolarıyla Epidemiyolojik Bir İnceleme.”*

The study uses a continuous-time Markov chain (CTMC) spark-phase birth–death model to estimate the probability that a transmission chain initiated by one imported dengue virus infection reaches an operational threshold of \(\tau=30\) concurrent infectious humans before extinction. The analysis covers five climatically distinct sentinel districts in Türkiye under CMIP6 SSP1-2.6, SSP2-4.5, and SSP5-8.5 scenarios over 2025–2075 (51 calendar years; 612 months).

Reaching the threshold is interpreted as attainment of a prespecified operational event, not as proof of endemic persistence.

Sentinel climate profiles and representative species assignments:

- **Kartal/İstanbul:** *Aedes albopictus*
- **Fethiye/Muğla:** *Aedes albopictus*
- **Hopa/Artvin:** *Aedes aegypti*
- **Zonguldak Merkez:** *Aedes aegypti* Black Sea corridor scenario; not a confirmed district record
- **Eğirdir/Isparta:** *Aedes albopictus*

Species assignments determine the species-specific thermal-performance parameters used by the model; they do not establish district-level dominance or exclude sympatry.

## Repository layout

```text
.
├── R/
│   ├── 00_maintenance/    # consistency, release, and maintenance utilities
│   ├── 01_setup/          # package, path, and global-option configuration
│   ├── 02_data/           # climate, population, importation, and trait processing
│   ├── 03_models/         # CTMC, Monte Carlo, parameter, and sensitivity functions
│   ├── 04_results/        # result generation, validation, regression, tables, and figures
│   ├── data_build_once_run.R
│   └── run_all.R          # end-to-end workflow driver
├── data_raw/              # redistributable source inputs and source-query documentation
├── data_processed/        # processed inputs used by the analytical pipeline
├── outputs/
│   ├── _canonical/        # canonical outputs used for reporting
│   ├── cross_scenario/    # cross-scenario outputs
│   └── tables/            # derived result tables
├── figures_standalone/    # publication and conceptual figures
├── reports/               # report-generation files
├── shiny_dengue_app/      # companion interactive application
├── CITATION.cff
├── LICENSE
└── README.md
```

## Model summary

- **Spark-phase CTMC:** \(I\in\{0,\ldots,\tau\}\), with \(I=0\) and \(I=\tau=30\) treated as absorbing extinction and operational threshold states, respectively. The finite-threshold probability is obtained from the birth–death gambler's-ruin solution.
- **Local transmission:** the per-infectious-individual rate \(\lambda_1(T,RH)\) follows a reduced Ross–Macdonald-type formulation, with \(R_0=\lambda_1/\gamma\) and \(\gamma=0.20\ \mathrm{day}^{-1}\).
- **Climate-sensitive vector biology:** biting, development, lifespan, and vector survival through the extrinsic incubation period (EIP) vary with temperature; vector mortality is additionally modified by vapour-pressure deficit.
- **EIP heterogeneity:** the main estimator averages EIP-dependent local-transmission rates over 2,000 inner draws and then evaluates the finite-threshold probability at the averaged rate. A separate Jensen-reference estimator calculates the average of the draw-specific threshold probabilities to quantify nonlinear-averaging sensitivity.
- **Importation pressure:** source-country incidence, province-level foreign-arrival volume, travel seasonality, effective exposure duration, and the fraction contributing viraemic exposure are combined in a time-varying Poisson importation process.
- **Risk aggregation:** Poisson thinning combines importation intensity with the single-import threshold-crossing probability to obtain monthly and 2025–2075 horizon risks.
- **Uncertainty and sensitivity:** the 1,000 outer repetitions reduce Monte Carlo error and are not a parametric uncertainty distribution. Parameter sensitivity is assessed separately using one-at-a-time analyses and LHS–PRCC.

The complete mathematical formulation, assumptions, and parameter definitions are provided in the doctoral thesis. The thesis document and manuscript working files are intentionally excluded from this code-and-data repository.

## Reproducing the analysis

### 1. Clone the repository

```bash
git clone https://github.com/okanderin/dengue-ctmc-turkiye.git
cd dengue-ctmc-turkiye
```

Open `r_project_tez.Rproj` in RStudio or start R from this directory.

### 2. Install the required R packages

R version 4.3 or later is recommended. Required packages are declared centrally in `R/01_setup/packages.R`. The setup script reports any missing packages and provides an installation command.

Start by installing `here` if it is not already available:

```r
install.packages("here")
source("R/01_setup/init.R", encoding = "UTF-8")
```

> **Environment note:** package versions are not currently pinned because this release does not contain an `renv.lock` file. For a versioned archival release, adding and validating an `renv.lock` file is recommended.

### 3. Run the pipeline

From a clean R session at the repository root:

```r
source("R/run_all.R", encoding = "UTF-8")
```

The default driver runs data preparation, all three SSP production runs, the Jensen-reference run in an isolated R process, result generation, and report rendering. Individual stages can be selected before sourcing the driver:

```r
STAGES <- c("04")
source("R/run_all.R", encoding = "UTF-8")
```

Available stages are `"02"` (data preparation), `"03"` (production and Jensen-reference model runs), `"04"` (tables, figures, and internal checks), and `"rmd"` (report rendering). A complete three-scenario model run is computationally intensive and may take approximately 45–60 hours on the reference system described during development. Running stage `"04"` alone requires the corresponding per-scenario model outputs to have been generated already.

## Verification and sensitivity

- **Finite-threshold numerical verification:** `R/04_results/core/01_generate_ssp_outputs.R` compares stored production estimates with the analytical finite-threshold birth–death expression evaluated from the corresponding stored mean local-transmission rates. This is an internal software check, not validation against observed dengue outcomes.
- **EIP estimator sensitivity:** `R/04_results/validation/stage2_eip_estimator_sensitivity.R` compares the primary estimator, \(P_\tau(E[\lambda(EIP)])\), with the Jensen-reference estimator, \(E[P_\tau(\lambda(EIP))]\), under EIP heterogeneity.
- **Poisson-thinning identity check:** `R/04_results/validation/ek_a4_poisson_bernoulli_check.R` checks the equivalence between the complement product of monthly non-occurrence probabilities and the cumulative-hazard expression.
- **VPD sensitivity:** `R/04_results/sensitivity/kvpd_sensitivity_check.R` evaluates alternative VPD–mortality coefficients.
- **Threshold sensitivity:** `R/04_results/sensitivity/10_tau_threshold_sensitivity.R` evaluates alternative operational thresholds.
- **OAT and LHS–PRCC:** scripts in `R/03_models/` and `R/04_results/sensitivity/` implement local and global sensitivity analyses.

Canonical reporting outputs are stored under `outputs/_canonical/`.

## Data availability

- Processed inputs used by the pipeline are provided in `data_processed/`.
- Canonical model outputs underlying the reported results are provided in `outputs/_canonical/`.
- Large raw CNRM-CM6-1-HR and ERA5-Land NetCDF files are not redistributed. Retrieval information and available query materials are provided under `data_raw/climate/`.
- Public demographic, tourism, geographic, and disease-burden inputs included in `data_raw/` remain subject to the terms and attribution requirements of their original providers.

A versioned archival release with a persistent identifier should be cited once available.

## Citation

Citation metadata are provided in `CITATION.cff`; its preferred citation is the 2026 doctoral thesis on which this repository is based.

## Licence

The analysis code is released under the MIT License. Processed data and outputs are released under CC BY 4.0 where redistribution is permitted. Raw or externally sourced data remain subject to the terms of their original providers. See `LICENSE` and the source-specific documentation for details.
