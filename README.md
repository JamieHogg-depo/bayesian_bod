# Improving the resolution of burden of disease measures with Bayesian models: assessing feasibility for government health agencies with an application in Western Australia

This repository contains R code for the analyses described in the manuscript "Improving the resolution of burden of disease measures with Bayesian models: assessing feasibility for government health agencies with an application in Western Australia" by James Hogg, Kerry Staples, Alisha Davis, Susanna Cramb, Candice Patterson, Jianguo Xiao and Wendy Sun. 

The modelled results are available in long-format in the dataset `ModelledEstimates.csv`. This dataset contains the following columns.

- `condition`: Asthma or coronary heart disease
- `metric`: Epidemiology metric
- `year`
- `lga_code_2016`
- `point`: Posterior median
- `lower`: Lower limit of 95% highest posterior density interval (HPDI)
- `upper`: Upper limit of 95% HPDI
- `se`: Posterior standard deviation
- `RSE`: Relative standard error (%)
- `EP`: Exceedance probability
- `ERP`: Estimated residential population 