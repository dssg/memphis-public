This is a project to evaluate the use of data to combat distressed properties and property
abandonment. It was initially undertaken using data from Memphis, Tennessee, but it is hoped
that the tools and analyses developed herein can be applied broadly.

The project consisted primarily of four components:

- A component to identify distressed properties, based on administrative data (in the `distressed-properties` folder)
- A component to simulate how rehabilitations effect change in property tax appraisals (in the `comparable-simulation` folder)
- A component to evaluate how rehabilitations are correlated with and have effected change in sale value of homes (in the `radial-binning` and `causal-inference` folders)
- A market value analysis, clustering Census tracts into areas with similar demographic and market characteristics, for policy development and use as a feature in other models (in the `MVA` folder)

We worked on a few other things as well:

- A comprehensive database about properties in Memphis, documented [here](https://www.github.com/dssg/memphis-public/wiki/DatabaseSchema)
- An evaluation of land productivity (tax per unit area less cost per unit area), in the `land-productivity` folder
- Scripts to load various types of data to our Postgres database (in the `data-load` folder)

This project was completed at the Eric and Wendy Schmidt
[Data Science for Social Good](http://dssg.uchicago.edu) summer
fellowship at the University of Chicago. The team consisted of
Alejandra Caro Rincon (Carnegie Mellon), Matthew Wigginton Conway (UC Santa Barbara),
Ben Green (Yale/Harvard), Robert Manduca (MIT/Harvard), and Tom Plagge (UChicago).
