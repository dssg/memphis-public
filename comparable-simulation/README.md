#Comparable Properties

We attempted to estimate the impact of rehabilitating a property on the appraised values of neighboring houses. This involved attempting to replicate the process that the tax assessor uses to construct appraised values from property characteristics and sales data. 

To prepare the app, first run get_frayser_hypo_sales.R. This will create a csv for each property of interest that contains all the properties for which it serves as a comparable. 

Then run create_fray_data.R. This takes the csvs created in the previous step, along with several new dataframes drawn from the database, and saves them in one big .Rdata file.

From there, the shiny app can be run. It consists of the ui.R and server.R scripts that are typical, plus a global.R one that allows importing the data so that it can be read by ui.R.

frayserCDC_comparables.R and frayserCDC_comparables_inclnew.R both attempt to assess the impact of specific renovations that Frayser CDC has done on surrounding property tax appraisals.

frayserCDC_comparables.R first computes the predicted values based on all sales that did happen and checks the correlation between those and the actual appraised values (about 0.74). It then evalutes two renovations that occured between 2010 and 2012, the years that are included in the most recent reappraisal.

frayserCDC_comparables_inclnew.R evalutes several renovations that occured in 2013 and 2014, which are not included in the most recent appraisal. It evalutes these as if they had occured in 2012. 



