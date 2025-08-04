#  Exit Poll Analysis Modeling 
**Joelle Gross | Summer 2025 Master's Practicum**

This project compiles, visualizes, and models U.S. presidential election exit poll data to provide state-level estimates of Democratic vote share, including in states without exit polls. It includes:
- A public-facing interactive dashboard
- A multilevel regression and poststratification (MRP) modeling pipeline
- A cleaned archive of historical exit poll data
- Evaluation of model accuracy and robustness

---

##  Project Overview

Exit polls offer a real-time glimpse into voter demographics and preferences, but they are only conducted in a subset of states. This project tackles the question:  
**Can we use historical data and demographic modeling to estimate how states without exit polls would have voted?**

To answer this, I compiled a cleaned archive of exit poll data, applied MRP using Census microdata and official election returns, and built a dashboard for exploring the results.

---

##  Dashboard Features
- Dynamic visualizations of exit poll responses  
- Filterable by year, state, party, and demographic group  
- Includes vote share estimates for all 50 states via MRP  
- Powered by historical data from 2008–2024

###  [Try the Dashboard](https://exit-poll-dashboard.onrender.com)

---

## Analysis Overview

### 1. Data Sources
- **Exit Poll Data**: [Roper Poll and NBC News archives, cleaned into CSV format (https://ropercenter.cornell.edu/elections-and-presidents/exit-polls)] 
- **Election Returns**: [MIT Election Data and Science Lab](https://electionlab.mit.edu/)
- **Census Microdata**: [American Community Survey (ACS 5-Year PUMS via `tidycensus`) (https://www.census.gov/programs-surveys/acs/microdata/access.html)] 

### 2. Modeling
- Multilevel logistic regression via `rstanarm`
- Poststratification using Census demographics
- Model trained on states with exit polls, generalized to those without

### 3. Evaluation
- Accuracy assessed with MAE, RMSE, and correct-winner classification
- Regression analysis of prediction error across years and states

---

##  Repo Structure

```
exit-poll-analysis/
├── data/                 # Exit poll CSVs, election returns, PUMS frames
├── code/    		          # Code files for running MRP \& and analysis
├── model/                # R scripts for MRP, poststratification, and evaluation
├── output/               # Model predictions and diagnostics
├── report/               # Paper and slides summarizing the project
├── report/    	          # Visualizations
└── README.md
```

---
## Key Findings

- MRP effectively predicts vote share in states with and without exit polls.
- Presence of an exit poll does **not** consistently reduce prediction error.
- Demographic predictors such as race, education, can skew modeling reuslts

---
---

##  Acknowledgments

Special thanks to:
- **Stephanie Perry**, NBC News
- **Dr. Marc Meredith**, University of Pennsylvania
- **Dr. Sharath Guntuku**, University of Pennsylvania

---
