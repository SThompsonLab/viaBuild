# viaBuild
R script to automate Alamar Blue data analysis

## Instructions:
This script is designed to take the 570 and 600 nM readings from an Alamar Blue assay using the plate reader from Janet Yother's lab. It will collate multiple read files into a single dataset, annotate the data by treatment per a plate.csv file, and generate various graphs and figures.

1. Create a directory per plate and add data files from that plate into the directory.
2. Fill out the plate.csv with the conditions in the 96-well plate. Place the plate.csv file in each directory made in the above step.
3. In RStudio, source the viaBluid.R file.
4. Run viaBuild, which has the following parameters:
  - **wd**: the working directory (Default = "./").
  - **time_list**: A list of times to use. If left to FALSE, viaBuild will pull the timepoints from the filename metadata (Default = F).
  - **long_or_wide**: Should the collated dataset be wide or long (Default = "long").
  - **nm_600_E**: Exctinction coefficient for Alamar Blue at 600 nM (Default = 117216).
  - **nm_570_E**: Exctinction coefficient for Alamar Blue at 570 nM (Default = 80586).
  - **norm_conc**: The label (per the plate.csv file) to normalize the conditions to (Default = "DMSO").
  - **add_time**: By default, if no time_list is given, the starting timepoint is 0. If you would like to add a number to this, set this parameter to that number (Default = 0).
  - **graph_it**: Graph the data (TRUE) or not (FALSE) (Default = TRUE).
  - **graphed_time**: Graph the data by "Time" or "Read_number" (Default = "Time").
  - **graph_type**: Graph the data by "relative" reduction (referencing the "norm_conc" variable) or "absolute" reduction (Default = "relative").
  - **plate_graph**: Generate plate layouts for each timepoint with the wells colored by absolute reduction. (Default = FALSE).
  
Additional calculations can be performed from the collated dataset, such as calculating the rate of reduction.
