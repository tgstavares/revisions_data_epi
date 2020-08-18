## Guide to codes of "Information and Behavioral Responses during a Pandemic: Evidence from Delays in COVID-19 Death Reports"

You can find here instructions to replicate all the results in the article "Information and Behavioral Responses during a Pandemic: Evidence from Delays in COVID-19 Death Reports" from Emilio Gutierrez, Adrian Rubli, Tiago Tavares. 

Before replicating the results in these codes, make sure you download the entire git repository.

### Replication epirical section on the survey

The results of these sections can be were tested using Stata 14.1. In the folder `Replication_covid_behavior/do/` you can find the related Stata do-file to generate the all the regression results and figures as shown in the paper. This file is self-explanatory in the sense that it is commented. The data used in this results can be find within the folder `analysis_treatments\raw\`.

### Replication of equilibrium model of infection

The results of this section were tested using `GNU Fortran 9.3.0`, and `gnuplot 5.2 patchlevel 8`. The main source file to compute the equilibrium can be find in `EqModel/main.f90` (you can use the makefile to compile it). Once this code is run, one can generate the paper figure using the gnuplot file `EqModel/figure_scripts/fig2.gp`.

### Contact
For more information, please contact [Tiago Tavares](mailto:tgstavares@gmail.com)