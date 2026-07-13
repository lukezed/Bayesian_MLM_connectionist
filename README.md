README: Belief-Driven or Structure-Determined?

Project Title: Belief-Driven or Structure-Determined? A Bayesian Multilevel Analysis of Connectionist Teaching Practices Among Mathematics Teachers in China.
Authors: Zhang, C., & Pampaka, M. (Under Review).

Project Structure Overview:
```
.
├── Bayesian_MLM_connectionist.Rproj  # R Project file
├── analysis
│   └── full_analysis.R               # Main R script for cleaning, modeling, and plotting
├── data
│   ├── MLM_data.xlsx                 # [IGNORED] Cleaned dataset for analysis
│   └── school_data_processing.xlsx    # [IGNORED] Raw data/pre-processing records
├── figures
│   ├── Fig1.png                      # Visualization of Belief-Practice relations
│   └── Fig2.png                      # Visualization of Housing Price effects
└── models
    ├── basic_mlm_multiple.rds        # Model 1 (Joint): Baseline
    ├── dim4_mlm_multiple.rds         # Model 2 (Joint): Structural predictors
    ├── final_mlm_multiple.rds        # Model 3 (Joint): Structural + Belief predictors
    ├── final2_mlm_multiple.rds       # Model 4 (Joint): Interaction (Belief x Attainment)
    ├── test_..._mlm.rds              # Parallel models using 'Test' data for validation
    └── ...sens..._rds                # Sensitivity analysis models (Priors comparison)
```
