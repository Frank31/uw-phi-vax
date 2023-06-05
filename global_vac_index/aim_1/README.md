# uw-phi-vax
Repository to store code used in the Vaccine Improvement Project at the UW Population Health Initiative

Last updated by Francisco Rios Casas (frc2@uw.edu) on June 5, 2023.

## Main files included in this repo:
  
 * "01_set_up_R.R": lists important packages, file paths, functions and order of scripts to carry out analyses.
  
 * "02_prep_vaccination_trend_data.R": reads in vaccination coverage data, appends file name, and saves in prepped data folder.
  
 * "03_prep_sdi_data.R": reads in SDI data from past 20 years, merges onto national/subnational GBD hierarchy and also reshapes data to merge onto vaccination coverage.

 * "04_id_countries_low_sdi.R": create plots that allow identifying high-performing countris.
  
 * "05_run_extract_dhs_data.R": reads in DHS surveys from selected countries, and extracts relevant variables for MOV analysis (vaccination dates, demographic and household information).
  
 * "6_prep_dhs_data.R": re-codes, labels, and reformats variables as appropriate. Also calculates new variables such as indicator for missed opportunity for vaccination.
  
 * "07_mov_survival_analyses.R": creates tables and figures for missed opportunities and survival curves for time until vaccination.

 * "08_mov_results_tables.R": creates tables with the resuls of the MOV analyses. 

 * "09_prep_disease_data.R": creates a dataset using data on vaccine-preventable diseases. 

 * "10_merge_data_for_visuals.R": combines output from previous scripts to create a final dataset.

 * "11_compile_mov_results_output.R": compiles previous code and output into an R Markdown Document. 