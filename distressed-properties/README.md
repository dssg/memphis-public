Our goal in this study was to estimate the risk that each residential property in Memphis is distressed using administrative data collected by the City of Memphis and Shelby County. While University of Memphis Center for Community Building and Neighborhood Action conducted a survey of distressed properties from 2008-2010, the results of that survey are no longer reliable. Since the time and money required to conduct another survey are prohibitively high, we utilized data science to gain similar results through a faster and cheaper method.

Using the survey results along with data about each home in 2008 as our training set, we trained a random forest classifier to estimate the probability that each home in 2011, 2012, and 2012 were distressed based on data from those years. Once the same data is collected for 2014 and future years, we can run the the model to identify likely distressed properties in that year.

The workflow for training and running the model and creating the online application are as follows:

1. Acquire data for the training and test years using data-load/fetch-data.R
2. Train the random forest and predict distressed properties using analysis/random-forest.R
3. Load data for the online application using data-load/fetch-data-app.R
4. Run the online application using the files in distressed-properties-app/

Note: all data in this repository contain simulated data to help clarify the structure of each field. None of it reflects any actual information about properties in Memphis.
