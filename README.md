## Guide to codes of "Delays in Death Reports and their Implications for Tracking the Evolution of COVID-19"

You can find here instructions to replicate all the results in the article "Delays in Death Reports and their Implications for Tracking the Evolution of COVID-19" from Emilio Gutierrez, Adrian Rubli, Tiago Tavares. 

Before replicating the results in these codes, make sure you download the entire git repository.

### Replication of sections "Descriptive Evidence for England and Mexico" and "Determinants of Reporting Delays"

The results of these sections can be were tested using Stata 14.1. In the folder `Estimation_delay/` you can find the related Stata do-files to generate the results. Here, we have a description of each do-files. These generate Figs. 1-5 in the paper. Each do-file contains more info on what is being done.
- SETTING UP DATA FOR MEXICO 
    - `gen_mex_data.do`: takes the raw data for Mexico and generates a cleaned up file with individual records (each observation is a person)
    - `gen_mex_covid_deaths.do`: takes the cleaned up file from the Mexico data and generates a dataset with observations at the municipality-date of death level detailing deaths reported at different dates
- SETTING UP DATA FOR ENGLAND
    - `gen_eng_covid_deaths.do`: takes the NHS England data and generates a dataset with observations at the trust-date of death level detailing deaths reported at different dates
- ANALYSIS
    - `desc_graphs.do`: a few descriptive graphs of how deaths have evolved over time (occurred vs reported)
    - `regs_deaths.do`: build info on delays, histograms of delays, regressions and plotting coefficients in different ways for comparison

### Replication of section "Implications for Epidemiological Modeling"
The results of this section were tested using `Python 3.7.4`, `GNU Fortran 9.3.0`, and `gnuplot 5.2 patchlevel 8`. Make sure your system has all required python libraries To replicate the main results:
1. To get results for Mexico move to the appropriate folder running `cd ModelEstimationMX/`
    1. Extract data from source files with `python export_data_to_fortran.py`
    1. Compile and run estimation code with `make main && ./main`
    1. (to get results using deaths data as reported or as occurred check the source code `main.f90` and uncomment where indicated)
    1. Convert the results to a csv format with `python export_data_to_python.py`
    1. (similarly you may need to comment/uncomment the source file as indicicated whether you're generating results using deaths as reported or deaths as occurred)
    1. Parameter results are summarized in the file `data/Res_00.txt` when using deaths as reported or `data/Res_13.txt` when using deaths as occurred
    1. Output files with simulations are in `data\exportfinal_002.csv` when using deaths as reported or `data\exportfinal_132.csv` when using deaths as occurred
1. To get results for England move to the appropriate folder running `cd ModelEstimationEN/` and repeat the analogous instructions of Mexico
1. At this stage one should have parameter estimates for both Mexico and England in `ModelEstimationMX/data/Res_00.txt`, `ModelEstimationMX/data/Res_13.txt`, `ModelEstimationEN/data/Res_00.txt`, and `ModelEstimationEN/data/Res_13.txt`; and simulation results in `ModelEstimationMX/data/exportfinal_002`, `ModelEstimationMX/data/exportfinal_132`, `ModelEstimationEN/data/exportfinal_002`, and `ModelEstimationEN/data/exportfinal_132`
1. Generate the paper figure moving to the appropriate folder with `cd Fig1/` and running `gnuplot fig1.gp` where the output is stored in the same folder under `fig_mx_en.eps`
1. Finally the simulation results can be analised using the jupyter notebook `Visualize_results_estimations.ipynb` in the root folder.
