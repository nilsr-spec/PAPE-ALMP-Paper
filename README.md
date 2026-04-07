# PAPE-ALMP-Paper

This repository contains the code and data for our group project concerned with the effect of a ALMPs on youth labour market outcomes.



Note: The datasets are too large to upload to GitHub. To run the code please download the appropriate dataset (ACS 2005-2019, all states, relevant variables) directly from IPUMS, insert it into the "data/raw" folder on your PC and rename the files according to the filenames in the 01\_load\_data.R file.



On the datasets:


data: Raw dataset imported from IPUMS

base: Restricted to labour force participants and 2005-2019 samples; restricted to relevant variables



base\_match: Extends base dataset by adding PUMA level covariates (needed for matching) and treatment indicator.



ny\_did: Restricts to target population and matched PUMAs in New York, identifies treatment and control; ready for within-NY PUMA DiD

eastcoast\_did: Restricts to target population and matched PUMAs on the East Coast, identifies treatment and control; ready for multi-state PUMA DiD



