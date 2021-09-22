# Gap Filling Method

This repository contains code and corresponding data for a gap-filling method developed by the [Transforming Drainage Project](https://transformingdrainage.org/) (TD) team to address the problem of missing measurements in subsurface drainage flow from artificially drained agricultural fields. It is implemented in three sequential phases and utilizes three measurements: on-site precipitation, soil temperature, and drain flow. Note that the method was developed with rainfed cropland in mind and was only applied to data collected at field scale. 

### Phase 1: Fill in Zero Flow 

[R script](./code/P1_01_fill_zero_flow.R)

During most winters in the northern states, the soil is frozen to the depth of the tile and no subsurface drain flow is expected. Such periods were identified based on expert judgment by researchers at each site relying on soil and air temperature information and local knowledge of the drainage system’s response to these conditions. When there were no drainage measurements available, and researchers determined the soil and drainage conditions were likely to have been frozen, we filled the gaps with zeros.


### Phase 2: Predict Using Replicate Plots

[R script](./code/P2_01_rep_regression.R)

Regression-based imputation from replicated plots or adjacent fields was used where available to replace missing data. Drain flow from replicated plots or adjacent fields tend to be correlated, so observed values at one location (plot) can be used as a predictor of missing values at another location. Due to the seasonal nature of subsurface drainage from croplands individual linear regression models were developed for each season: Winter (Jan, Feb, Mar), Spring (Apr, May, Jun), Summer (Jul, Aug, Sep), and Fall (Oct, Nov, Dev). 


### Phase 3: Impute Based on Precipitation and Observed Drain Flow Data

[R script](./code/P3_03_gap_filling.R)

The remaining missing daily drain flow data were filled based on the assumption that on a given day a drain can flow only if either (a) precipitation occurred on that day, or (b) the drain continues to flow from the day before. This required development of empirical relationships between on-site precipitation and peak flow, and recession characteristics of the drain flow.

For days with precipitation, a 2-day moving average was calculated in order to take into account time-lag between rainfall and resulting drain flow. A linear regression model was fitted to non-zero drain flow and 2-day moving average precipitation for each individual season, with the intercept of the model fixed to zero. We used these models to predict the missing drain flow data for days with non-zero precipitation. The predicted drain flow values were limited to the capacity of the drainage system by replacing any predictions greater than the site’s drainage coefficient (depth of water that drainage system can remove within 24 hours) with the value of the coefficient.

For days with zero precipitation, missing drain flow was calculated from the previous day’s observed flow using the 1st order recession equation 

$$
Q_i = Q_{i-1}e^k
$$

where $Q$ is daily drain flow, $k$ is the average recession coefficient of falling limbs calculated as a linear slope of $ln(Q)$, and i indicates day. The recession coefficient was calculated as a linear slope between the peak and inflection point of log-transformed daily drain flow data. The coefficient was calculated for all falling limbs of drain flow data and the average seasonal values were calculated as their arithmetic mean. 

It should be noted that the regression model between on-site precipitation and peak flow and recession equation were done using the original (pre-gap-filled) drain flow data. When the number of missing drainage days exceeded 152 (5 months) within a calendar year predictions were not made. Both the original and filled data are included in the published data. 
  
  
## Transforming Drainage Data

The data used in the gap-filling process (available on this repository) represents a fraction of the large dataset collected by the [Transforming Drainage Project](https://transformingdrainage.org/) from artificially drained agricultural fields across US Midwest and North Carolina. The complete dataset consist of 219 variables, including 90 field measurement and 129 management variables, and is published at the USDA National Agricultural Library Ag Data Commons repository.

When use this dataset, please cite: [![DOI](https://img.shields.io/badge/DOI-10.15482%2FUSDA.ADC%2F1521092-brightgreen)](https://doi.org/10.15482/USDA.ADC/1521092)

