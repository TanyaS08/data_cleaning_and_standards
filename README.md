# Assignment 1 (Data Cleaning and Standards)

## Assignment summary 

You will strategise on this assignment in groups, but submit the assignment individually.
In class, decide on the best order of functions to complete your task.  Then, either alone
or with other students, prepare a short R script that reads in the BWG database and applies
a sequence of steps to address one of the data cleaning tasks listed below(and/or any other
data cleaning task that your group sees fit). Write a brief description of the issues with
the data and the approach you developed to resolve it - even if you developed the code with
other students this part should be done individually.

## End product (Goal)

Two things (because I had too much fun...)

1. Find *potential* outliers in the bromeliad dataset with regards to number of leaves and
maximum water content and export as a .csv file

2. Add the higher-level taxonomic information for the bromeliad species and export
as a .csv file

## Overview of code functionality

The idea here is to flag potential outlying bromeliads based on either the number of leaves
recorded or maximum water content. The resulting .csv file (in `clean_data/`) merely lists 
bromeliads that are considered outliers, however the decision of if these are 'true' outliers
that need to be removed will be left up to the end users. A note here is that there are duplicates
bromeliads in the outlier .csv if they have both an outlying number of leaves and maximum water 
content - I _could_ have combined them and listed both variabels as outliers, however if you were
implementing this workflow for more variables it would get confusing. This slightly longer format
could also potentially allow the user to pivot the table wider if they wanted to have outlier
variables per a single bromeliad There are also some other checks to catch potentially erroneous
entries such as that all values are positive (> 0) and are indeed numeric.

The main aim of the second workflow is to try and create a pipeline that can can be run from
start to finish (and works) but will 'break' if new species were to be added and their taxonomy
could not be resolved (and would then have to be fixed). In short: something conceptually
similar to to assertr but tailored for constructing/filling in the taxonomic information for
bromeliad species. The resolved taxonomy (for all morpho species barring those listed as
unknown) for the current dataset can be found in `clean_data/`. As I suppressed user interaction
when a query has no/multiple matches to facilitate automation/reproducibility the check at the 
end will allow users to pick up if any species were missed and they can add them to the pipeline
where indicated in the code.

