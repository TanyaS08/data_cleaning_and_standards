# Assignment 1 (Data Cleaning and Standards)

## Assignment summary 

You will strategise on this assignment in groups, but submit the assignment individually.
In class, decide on the best order of functions to complete your task.  Then, either alone
or with other students, prepare a short R script that reads in the BWG database and applies
a sequence of steps to address one of the data cleaning tasks listed below(and/or any other
data cleaning task that your group sees fit). Write a brief description of the issues with
the data and the approach you developed to resolve it - even if you developed the code with
other students this part should be done individually.

## End product

Add the higher-level taxonomic information for the invertebrate morpho species and export
as a .csv file

## Overview of code functionality

The main aim of this workflow is to try and create a pipeline that can can be run from start
to finish (and works) but will 'break' if new species were to be added and their taxonomy
could not be resolved (and would then have to be fixed). In short: something conceptually
similar to to assertr but tailored for constructing/filling in the taxonomic information for
invertebrate species.

