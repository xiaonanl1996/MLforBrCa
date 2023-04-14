# MLforBrCa

This repository accompanies the paper: 

*Combining Machine Learning with Cox models for identifying risk factors for incident post-menopausal breast cancer in the UK Biobank*

The "Scripts" folder contains all scripts for data management and analyses. 

The `config.yml` file contains yaml entres for the values these scripts expect to find in the project config file. One can update or add the paths where appropriate. 

The `renv.lock` file saves the current versions of R packages used in the analyses. 

Scripts that generated figures and tables in the manuscript (see supplementary material for more info):
* Figure 1: See `Scripts/Data Management/Scripts_Processing/InitialExtraction.Rmd`.
* Figure 2: Generated using [whimsical](https://whimsical.com/).
* Table 1: See `Scripts/Stats_Analysis/RMarkdown/MLforBrCa_BaselineTable.Rmd`.
* Figure 3: See `Scripts/Stats_Analysis/PieChart.R`.
* Figure 4: See `Scripts/Stats_Analysis/JupyterNotebook/XGBoost.ipynb`.
* Figure 5: See `Scripts/Stats_Analysis/RMarkdown/MLforBrCa_CoxTable.Rmd`.