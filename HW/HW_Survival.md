1. DPCA Study of Primary Biliary Cirrhosis (PBC dataset in DataRegressBook). To illustrate interpretation of Cox model results, we consider a cohort of 312 participants in a placebo-controlled clinical trial of D-penicillamine (DPCA) for primary biliary cirrhosis (PBC) (Dickson et al. 1989). PBC destroys bile ducts in the liver, causing bile to accumulate. Tissue damage is progressive and ultimately leads to liver failure. Time from diagnosis to end-stage liver disease ranges from a few months to 20 years. During the approximate 10-year follow-up period, 125 study participants died.
Predicting survival in PBC patients is important for clinical decision making. The investigators collected data on age as well as baseline laboratory values and clinical signs including serum bilirubin levels, enlargement of the liver (hepatomegaly), accumulation of water in the legs (edema), and visible veins in the chest and shoulders (spiders)—all signs of liver damage.  
For the PBC dataset (also in DataRegressBook), fit a model with cholesterol and bilirubin. Interpret the results, as you would in a paper, reporting the hazard ratios for a 100 mg/dL increase in cholesterol and a 10 mg/dL increase in bilirubin. Is the relationship between cholesterol and survival confounded by bilirubin?  
2. For the ACTG 019 data (ACTG019.csv), treatment rx is coded ZDV = 1 and placebo = 0. Estimate a Cox mode calculate the hazard ratios, CIs, likelihood ratio (LR), and Wald tests compare to the original coding? If any are different, how are they different?  