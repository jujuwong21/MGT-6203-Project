# Team 7: UFC Fight Prediction and Analysis

**Team Members**: 
* Alejandro Forero (aeng61@gatech.edu)
* Samuel W. Wang (swang3068@gatech.edu)
* Andrew Berkowitz (aberkowitz8@gatech.edu)
* Juliette N. Wong (jwong88@gatech.edu)

With the rise in popularity in Ultimate Fighting Championship (UFC), there has been an increase in sports betting and pay-per-view (PPV) buys in recent years. Because of this, the overall goals of our project were to:
* analyze past fight statisitic data to understand what factors may increase a fighter's odds in winning and to predict the winner of a match
* use past fight statistic data and PPV data to predict PPV buys

## :chart_with_upwards_trend: Dataset

Our original dataset consisted of two files:

1. [UFC Fight Data from 1993 to 2023 (Kaggle)](https://www.kaggle.com/datasets/akshaysinghim/ufc-fight-data-1993-to-2023?resource=download): `raw_total_fight_data.csv, ufc-fight-data-1993-to-2023.csv`
2. [UFC PPV Sales (Kaggle)](https://www.kaggle.com/datasets/daverosenman/ufc-ppv-sales): `ufc_ppv_buys.csv`

## :clipboard: Project File Structure

This repository follows the following structure: 

* **Code**
  * **EDA_who_will_win.R**: R file containing EDA for the question "Who will win?" in a UFC match
  * **PPV_model_linreg.html**: HMTL file showing output of the PPV_model_linreg R notebook
  * **PPV_model_linreg.rmd**: R notebook file containing EDA and modeling for the linear regression model for PPV buys
  * **data_analysis_logistic.R**: R file containing EDA and modeling for the logistic regression model to predict UFC winner
  * **data_cleaning.R**: R file containing the script to clean our data 
  * **gbm_model.R**: R file containing EDA and modeling for the gradient boosting method model to predict UFC winner
  * **rf_model.ipynb**: Jupyter notebook containing Python code to predict UFC winner using random forest
* **Data**
  * **Cleaned**
    * **df_records.csv**: CSV file containing the win-loss records for a fighter before their match 
    * **fight_data.csv**: CSV file containing cleaned UFC fight data
    * **fighter_agg.csv**: CSV file containing the per-minute statistics of a fighter's previous matches 
    * **ppv_data.csv**: CSV file containing cleaned PPV data
  * **Raw**
    * **ufc_data_till_UFC_292.csv.csv**: CSV file containing raw (uncleaned) UFC fight data
    * **ufc_ppv_buys.csv**: CSV file containing raw PPV sales data
* **Final Report**
  * **team007finalreport.pdf**: PDF of our final report that we submitted to Canvas
* **Literature/Related Work**: Folder containing academic papers we conducted literature reviews on (see related works section below)
* **Progress Report**
  * **team007progressreport.pdf**: PDF of our progress report that we submitted to Canvas
* **Project Proposal**
  * **team007proposal.pdf**: PDF of our project proposal that we submitted to Canvas  
* **Visualizations**
  * **Correlation of variables.png**: PNG file showing the correlation of significant variables with the winner value for UFC fight prediction
  * **EDA.png**: PNG file showing 4 EDA plots for determining UFC winner
  * **EDA for PPV.png**: PNG file showing 4 EDA plots for PPV
  * **Linear Regression Model Comparison Table.png**: PNG file showing model fit metrics for the linear regression model
  * **Precision-recall curve (GBM).png**: PNG file showing the precision-recall curve for the GBM model to predict winner
  * **ROC Curve (GBM).png**: PNG file showing the ROC curve for the GBM model to predict winner
  * **logsitic_results.png**: PNG file showing the results (ROC curve, loss vs. threshold) for the logsitic regression model

## :book: Related Works

|  #  | Title                                                                                                                          | Author             | Type              |
| :-: | :----------------------------------------------------------------------------------------------------------------------------- | :----------------- | :---------------- |
|  1  | Applying Machine Learning Algorithms to Predict UFC Fight Outcomes                                                             | McQuaide, McKinley | Final Project     |
|  2  | Predicting UFC Fight Scoring - Multivariate Linear Regression | Wismer, David      | Bootcamp Project  |
|  3  | Machine learning in combat sports                                                                                              | Bartoš, Mikoláš    | Bachelor's Thesis |

## :floppy_disk: How to Run our Code

Note: these instructions assume that this repository has been cloned locally. The packages our code depends on are listed within the bullet points. If one does not have any of the packages, they can first run `install.packages("package-name")`

* To run the data_cleaning.R script, one can go to the Team-7 directory, and then run `Rscript Code/data_cleaning.R`
  * The data cleaning script requires the following packages: funr, usefun, tidyverse (dplyr, tidyr), fastDummies, writexl, runner
* To run the data_analysis_logistic.R script, one can go to the Team-7 directory, and then run `Rscript Code/data_analysis_logistic.R`
  * This script requires the pROC package
  * This script assumes that the data cleaning script already ran and the cleaned files exist in one's enviornment
* To run the EDA_who_will_win.R script, one can go to the Team-7 directory, then run `Rscript Code/EDA_who_will_win.R`
  * This script requires the following packages: ggplot2, fuzzyjoin, reshape2
  * This script assumes that the data cleaning script already ran and the cleaned files exist in one's enviornment
* To run the gbm_model.R script, one can go to the Team-7 directory, and then run `Rscript Code/gbm_model.R`
  * This script requires the following packages: caret, glmnet, gbm, xgboost, dplyr, lubridate, pROC
  * This script assumes that the data cleaning script already ran and the cleaned files exist in one's enviornment
* The PPV_model_linreg.rmd file is a R notebook file that can be opened in the RStudio application.
  * This file requires the following packages: tidyverse, corrplot, car
* The rf_model.ipynb is a jupyter notebook. If one has jupyter notebook set up, they can run this by typing `jupyter notebook` in the command line
  * This file requires the following libraries: pandas, numpy, pprint, sklearn
