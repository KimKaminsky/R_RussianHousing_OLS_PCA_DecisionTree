# R_RussianHousing_OLS_PCA_DecisionTree


This project used Russian Housing data from Kaggle from one of it's competitions. I participated in this competition early on in my data science program at Northwestern. My object was to practice some of the techniques I had learned in my regression and multivariate analysis class. Also, that course was taught using SAS so I wanted to spend some time learning R. 

I focused solely on using linear models for this competition. The dataset was very messy and it had a lot of variables so I spend a lot of time on data cleanup in order to improve my score. One problem was that there was a lot of missing data. If too much data was missing I just dropped the variable. For the macro economic data, I was able to find some of the missing data from the internet. If I was able to do this, I did it. If not, I imputed missing variables using decision trees with the Rattle library. Another helpful tool that improved my score was using Principal Component Analysis (PCA) to reduce dimensionality. 

My final score in the competition wasn't that great as I scored 2561 out of 3274 but I learned a lot about how to improve my score within the confines of using a linear model with a messy dataset with a lot of variables. If I had to do this again, I would try out using random forest as it works well with missing data and doesn't require as much data prep. I would be interested to see if this would improve the rmsle significantly without so much effort. At some point I will revisit this and then post the results in my github portfolio.
