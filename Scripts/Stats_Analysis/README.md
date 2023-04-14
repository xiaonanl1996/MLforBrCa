For analyses in 'Stats_Analysis': 

* 'Flowcharts/Flowchart.gv': produces Figure 1. Flowchart illustrating the selection process for our study population in manuscript. 

* 'JupyterNotebook': ML Analyses conducted in Python.
  1. `XGBoost-Cox.ipynb` implements XGBoost ML method.
  2. `HistGBM.ipynb` implements HistGBM ML method.

* 'Rmarkdown': Classical statistical analyses conducted in R. 
   1. `MLforBrCa_BaselineTable.Rmd`: produces baseline characteristics table.
   2. `Multiple_Imp.Rmd`: conducts multiple imputation for training and test data separately.
   3. `MLforBrCa_CoxTable.Rmd` fits baseline and augmented Cox models.
   4. `MLforBrCa_ModelPerformance.Rmd` computes Brier score and Harrell's C-index of two Cox models.
   5. `MLforBrCa_SA.Rmd`: documents sensitivity analyses.

* `JCfunctions.R`: stores generic functions used in analyses (e.g. tweaking p-value digits etc).
* `PieChart.R`: produces Figure 3 Pie chart in manuscript.
