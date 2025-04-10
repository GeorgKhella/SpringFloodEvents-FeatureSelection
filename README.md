# Local Causal Discovery of Spring Flood Events – Applied Statistics Project

**Author:** Georg Khella  
**Course:** Applied Statistics – MATH-516  
**Date:** March 2025

## 🌊 Objective

Investigate the **drivers of spring flood events** in Swiss catchments using **causal discovery** and **logistic classification**.  
Two hydrologically simulated catchments are analyzed (low vs. high elevation), with a binary target indicating if daily discharge exceeds the 90th percentile.

## 🧾 Dataset

- Daily data (1981–2016) for catchments 207 (low elevation) and 106 (high elevation)
- Variables: Precipitation, Temperature, Radiation, Snowmelt, Soil Moisture, Evapotranspiration, Discharge
- Lags: up to 3 days for all covariates

## 🧠 Methods

- **Logistic Regression (Full and LASSO):** Predict flood occurrence, select informative features
- **PC Algorithm:** Estimate Markov blanket (causal parents, children, spouses)
- **Mutual Information Ranking:** Approximate HITON-PC (local causal discovery)
- **Evaluation Metrics:** Accuracy, Precision, Recall, F1, AUC (on 2015–2016 test set)

## 📊 Results Summary

| Catchment | Model        | AUC   | F1 Score | Precision | Recall |
|-----------|--------------|-------|----------|-----------|--------|
| 207 (low) | LASSO / Full | 0.991 | 0.756    | 0.829     | 0.694  |
|           | PC Algorithm | 0.982 | 0.683    | 0.848     | 0.571  |
|           | MI Ranking   | 0.966 | 0.667    | 0.800     | 0.571  |
| 106 (high)| LASSO / Full | ~0.98 | ~0.74    | ~0.94     | ~0.62  |
|           | PC Algorithm | 0.981 | 0.661    | 0.925     | 0.514  |
|           | MI Ranking   | 0.960 | 0.604    | 0.941     | 0.444  |

## 🔍 Key Insights

- LASSO maintains high AUC while reducing feature count by ~40%
- Causal selection (PC, MI) offers **interpretable subsets** but lower recall
- Catchment 207 (low elevation) showed more stable and predictive discharge patterns

## 📁 Files

- `Khella_Project3_AppliedStatistics.pdf`: Full project report including analysis, figures, and causal graphs

## 📚 References

- Spirtes et al. (2000) – *Causation, Prediction, and Search*  
- Tibshirani (1996) – *LASSO Regression*  
- Pearl (2009) – *Causality: Models, Reasoning and Inference*  
- Aliferis et al. (2003) – *HITON-PC Feature Selection*
