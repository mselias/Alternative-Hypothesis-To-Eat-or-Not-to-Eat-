# Alternative Hypothesis: To Eat or Not to Eat?
Armed with a data set containing a single string of information on the DNA structure of the Cytomegalovirus, our task was to search for abnormalities in the palindromes. In particular, we were looking for complementary DNA subsequences within the data. 

For this project, my role was to pose and analyze an additional hypothesis. The answer we were looking for was “Does the individual's overall health matter in combating the virus?”. To discover whether the person's underlying health played a key factor, we looked at a secondary data set in which patient information was held. We then began by running an Ordinary Least Squares (OLS) regression to determine significance levels of associated betas that were generated. After cleaning the data set and running two iterations of the OLS model in R, we were able to determine that the HBA1C reading (Patient's blood sugar levels), blood pressure, and gender played an important factor in the infectivity level of the patient. The overall conclusion in this case was that overall health did play an important role in the virus’ infectivity rate.
