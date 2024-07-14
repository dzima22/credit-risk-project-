# Description
- `Credit risk project` that involves generating and analyzing simulating financial data to assess credit risk.  

# Content
- *database* - raw dataset 
- *risk_code.R* - analysis, modeling and testing procedure stored as R script

# Methodology
- **Data Generation**:
 + Use the provided 00_database_generation.R script to generate loans.csv and clients.csv.
-Data Merging:
 + Merge the two datasets
- Feature Engineering:
 + Create explanatory variables for the Probability of Default (PD) model using the client data.
- Outlier Handling:
 + Verify if the explanatory variables contain outliers and adjust the data series accordingly using winsorization.
- Probability Estimation:
 + Estimate the default probability for each client using a logistic regression model and probit model.
- Contingency Table:
 + Create and interpret a confusion matrix to evaluate the classification performance.
- ROC Curve and AUC:
 + Plot the ROC curve and calculate the AUC and also interpret the results.
- Bootstrap Confidence Intervals:
 + Calculate bootstrap confidence intervals for the AUC with a 90% confidence level.
# Findings
