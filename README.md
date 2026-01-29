Project Title: Belief-Driven or Structure-Determined? A Bayesian Multilevel Analysis of Connectionist Teaching Practices Among Mathematics Teachers in China. Authors: Zhang, C., & Pampaka, M. (Under Review).

### Project Structure Overview
Plaintext
├── Bayesian_MLM_connectionist.Rproj  # R Project file
├── analysis
│   └── full_analysis.R               # Main R script for data cleaning, modeling, and visualization
├── data
│   ├── MLM_data.xlsx                 # Cleaned dataset used for analysis
│   └── school_data_processing.xlsx    # Raw data / pre-processing records
├── figures
│   ├── Figure_1_PA.pdf               # Visualization of Belief-Practice relations (Model 3 & 4)
│   └── Figure_2_Housing_Price.pdf    # Visualization of Housing Price effects (Model 2 & 3)
└── models
    ├── basic_mlm_multiple.rds        # Model 1 (Joint): Baseline
    ├── dim4_mlm_multiple.rds         # Model 2 (Joint): Structural predictors
    ├── final_mlm_multiple.rds        # Model 3 (Joint): Structural + Belief predictors
    ├── final2_mlm_multiple.rds       # Model 4 (Joint): Interaction (Belief x Attainment)
    ├── test_..._mlm.rds              # Parallel models using only 'Test' data for validation
    └── ...sens..._rds                # Sensitivity analysis models (informative vs. non-informative priors)