This file contains information and instructions for the code associated with the paper titled 
"Climate policies that achieved major emission reductions: Global evidence from two decades" by Stechemesser et al.

Authors of this file and the code are: Annika Stechemesser, Ebba Mark, Moritz Schwarz, Patrick Klösel, Felix Pretis et al. 

Provided are files to reproduce the main analytical steps, including running the break detection models, preprocessing and coding the CAMPF data,
matching the break detection results with policy data, producing Fig. 1-4 as well as figures and tables for the supplementary information. 

The CAPMF data in this replication package was obtained in July 2023 before the official release by the OECD. The latest CAPMF version is 
available from the OECD Data Explorer (https://oe.cd/dx/capmf). The policy data for the US in this replication package were downloaded in 
March 2023 from the IPAC Dashboard from https://www.oecd.org/climate-action/ipac/dashboard and from the data visualisation tool from 
https://oecd-main.shinyapps.io/climate-actions-and-policies.

Overview of files: 

Name: 00_oecd_project_functions.R
Input: none
Output: none
Summary: File that holds auxiliary functions for the project including policy matching, analysis and plotting. It is loaded
in other files and used there. 

Name: 01_run_break_detection_models.R
Input: country_groupings.xslx, break_detection_regression_input.RDS
Output: Break_detection_results.RDS
Summary: Runs the break detection model  

Name: 02_preprocess_oecd_data.R
Input: CAPMF_Policy.csv, IPAC_policy_usa.csv, add_on_data.csv,
CAPMF_policies_names.csv, country_groupings.csv 
Output: OECD_data_preprocessed.csv, EU_policies_label_df.csv
Summary: Takes the raw OECD data, cleans and filters it, codes policy adoptions and tightenings, creates a label dataframe 
that indicates years with EU-policy controls for Fig. 2+3. 

Name: 03_policy_matching.R
Input: OECD_data_preprocessed.csv, Break_detection_results.RDS, 00_oecd_project_functions.R
Output: Policy_out.RDS, Policy_out_filtered.RDS
Summary: Matches breaks in each sector and country to policies that plausibly caused the break based on a 
confidence interval (statistical, 2-year, 3-year). Computes basic summary statistics on detected breaks. 

Name: 04_Fig_1.R
Input: 00_oecd_project_functions.R, OECD_data_preprocessed.csv, Logos (see folder), Policy_out_filtered.RDS, final_countries_in_analysis.csv 
Output: linechart.png (Fig. 1A), mix_plot_with_legend.png (Fig. 1B), Americas.png, Asia_Oceania.png, Europe.png, South_Africa.png (for Fig. 1C), 
Break_counts_bar_charts (for Fig. 1C, see folder), legend_break_map.png
Summary: Produces Fig. 1. Final assembly of Fig. 1 was manually as country-specific break counts are placed on maps manually.

Name: 05_Fig_2_3.R
Input: 00_oecd_project_functions.R, OECD_data_preprocessed.csv, Logos (see folder), Policy_out.RDS, EU_policies_label_df.csv
Output: Fig_2.png, Fig_3.png
Summary: Creates panel figures which show the true emissions, model fit, breaks, counterfactuals, matching and unmatched policies for each 
sector and country that had at least one break in the sector 

Name: 06_Fig_4.R
Input: 00_oecd_project_functions.R, OECD_data_preprocessed.csv, Logos (see folder), Policy_out_filtered.RDS, country_groupings.csv
Output: Fig_4.pdf, ven_diagrams.png, mean_effect_size_bars.png, mean_dfs_fig_4b.csv, Fig_4B_developed.csv, Fig_4B_developing.csv; 
Summary: Creates Fig. 4 which compares effect sizes for mixes and single policies, highlights the effect size for mixes with pricing, 
shows ven (euler) diagrams that visualise which policy types combine to successful mixes, 
counts how many breaks are matched by one policy or more to report in main text.

Name: 07_minimum_effect_size.R
Input: none
Output: Fig. S4
Summary: Visualizes the minimum detectable effect size.

Name: 08_parameter_and_control_variation.R
Input: country_groupings.xlsx, break_detection_regression_input.RDS, additional_control_variables.RDS
Output:main_model_result_parameter_variation.RDS, robustness_checks_control_variation.RDS
Summary: Runs robustness checks with parameter variations as well as robustness checks with control variations (section 7.2, 7.3 in the SI)

Name: 09_run_break_detection_adaptive_Lasso.R
Input: country_groupings.xlsx, break_detection_regression_input.RDS
Output: "20240515 Saving Overall LASSO FINAL.RData"
Summary: Runs break detection models with adaptive Lasso as selection algorithm (see SI section 7.2)

Name: 10_run_break_detection_threshold_Lasso.R
Input: 20240515 Saving Overall LASSO FINAL.RData
Output: 20240515 Final Lasso Models.RData -> also stored as RDS named final_lasso_models.RDS
Summary: Runs break detection models with threshold Lasso as selection algorithm (see SI section 7.2)

Name: 11_SI_retention_rate_lasso.R
Input: 00_oecd_project_functions.R, final_lasso_models.RDS
Output: Fig. S6-S13
Summary: Makes overview plots that visualize the retention rates for Lasso variations of selection algorithm (see SI section 7.2)

Name: 12_SI_fixed_effects_variation.R
Input: 00_oecd_project_functions.R, country_groupings.xlsx, break_detection_regression_input.RDS 
Output: Results_developing.RDS, Results_developing_continent.RDS, Results_developed.RDS, Results_developed_continent.RDS, Results_developed_continent_west_east_europe_split.RDS
Summary: Runs break detection models with continent-year fixed effects variations. 

Name: 13_robustness_checks_model_variation_plot_overview.R
Input: all_robustness_checks_combined.RDS, Results_developing_continent.RDS, Results_developed_continent_west_east_europe_split.RDS, Results_developing.RDS, Results_developed.RDS
Output: results_developing_compare_df.csv, Fig. S14-S21
Summary: Computes break retention rates and plots them for robustness of model specifications. 
 
Name: 14_SI_tables.R
Input: 00_oecd_project_functions.R, sector_codes_final_7_3_2023.RDS, break_detection_regression_input.RDS, main_model_result_parameter_variation.RDS, robustness_checks_control_variation.RDS, Results_developed_continent_west_east_europe_split.RDS, Results_developed_continent.RDS, Results_developing_continent.RDS, 20240511 Final Lasso Models.RData
Output: SI tables (see supplementary information)
Summary: The script creates a .txt file with a table of the relevant sectoral codes underlying sector-level emissions used in the analysis, summary statistics tables, and all regression tables included in the supplementary materials.

Name: 15_sensitivity.Rmd
Input: break_detection_regression_input.RDS
Output: Sensitivity analysis (Supplementary Information section 7.4)
Summary: Conducts a Cinelli et al. 2020 sensitivity analysis checking for unobserved confounding

Name: 16_SI_leave_one_out_variation.R
Input: break_detection_regression_input.RDS, country_groupings.xlsx
Output: developed_leave_one_out_save.csv, leave_one_out_developed_results_final.csv, leave_one_out_developing_results_final.csv
Summary: Conducts the sample composition robustness check (SI section 7.5)

Name: 17_forward_causal_analysis.R
Input: break_detection_regression_input.RDS, Forward_causal_step_shift_numbers.csv, OECD_data_preprocessed.csv 
Output: see SI section 7.7
Summary: Trials forward-causal step-shift selection as a robustness check. 
 
Name: 18_SCM_robustness_check.Rmd
Input: break_detection_regression_input.RDS, 00_oecd_project_functions.R
Output: see SI section 8
Summary: Re-estimate breaks found in the main specification with SCM using the gsynth package (estimate SCM for each break in each sector in each country as a robustness check)

Name: 19_SI_policy_detection_rate.R
Input: 00_oecd_project_functions.R, OECD_data_preprocessed.csv, Policy_out.RDS, policy_out_filtered.RDS
Output: Fig. S44 (see section 9 in SI)
Summary: Visualizes the share of how often a policy instrument was detected in an emission break

Name: 20_SI_pricing_mix_effect_comparison.R
Input: 00_oecd_project_functions.R, OECD_data_preprocessed.csv, Policy_out.RDS, policy_out_filtered.RDS, Logos (see folder)
Output: Fig. S46 (see section 9 SI)
Summary: Visualizes the comparison of mixes with an without pricing instruments. 

Name: 21_SI_blockmatrix.R
Input: 00_oecd_project_functions.R, OECD_data_preprocessed.csv, Policy_out.RDS, policy_out_filtered.RDS
Output: Fig. S47
Summary: Produces a block-matrix of emerging policy mixes. 



