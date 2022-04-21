For data management (or derivation) in 'Data_Management' folder:

* 'Scripts_Cleaning': This should be the first folder to check. The order for viewing the script is: 
   1. `basic_functions.R`: is where we write and store generic functions used for variable derivation. 
   2. `derivation_objects.R`: is where we specify how we want to derive each variable. We can specify variable name, descriptions and which functions to use etc for deriving each variable.
       This script includes all the variables used in this study. 
   3.	`TEU_specifications.R`: is where we create a spec (i.e. a list) to gather all the variables required for specific project. (This is more like creating a shopping list in a way)
      The spec used for this study is the combination of `BrCa_PRS` and `HES_CaR` (as you will see in 'Data_Management/Scripts_Processing/InitialExtraction.Rmd').
      Variables were organised into different sub-specs for better presentation (otherwise the list would be way too long).
   4. The other 2 scripts (`DuckDB.R` and `dataset_generator.R`) are more for backend operation. We don’t usually need to edit them for deriving variables.

* 'Scripts_Processing': 
   1. `ATC_baseline.R` is the script for mapping UK Biobank (UKB) medications to The Anatomical Therapeutic Chemical (ATC) Classification System. 
   2. `InitialExtraction.Rmd` is the rmarkdown (Rmd) file for pre-processing data from data extraction to train-test split for machine learning (ML). Note that this Rmd file
   is not for knitting but for running interactively with a clear structure.