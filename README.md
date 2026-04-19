# PAPE-ALMP-Paper

This repository contains the code and data for our group project concerned with the effect of a ALMPs on youth labour market outcomes.



Note: The datasets are too large to upload to GitHub. To run the code please download the appropriate dataset (ACS 2005-2019, all states, relevant variables) directly from IPUMS, insert it into the "data/raw" folder on your PC and rename the files according to the filenames in the 01\_load\_data.R file.



On the datasets:



data: Raw dataset imported from IPUMS

base: Restricted to 16-24 year old labour force participants and 2005-2019 samples; restricted to relevant variables



puma\_covs: PUMA level dataset (aggregated) of pre-treatment (2010) covariates for matching



nyc: Restricts to target population and matched PUMAs in New York, identifies treatment and control; ready for within-NY PUMA DiD

upstate: Restricts to target population and matched PUMAs on the East Coast, identifies treatment and control; ready for multi-state PUMA DiD

