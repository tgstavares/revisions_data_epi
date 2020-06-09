## Guide to codes of "Delays in Death Reports and their Implications for Tracking the Evolution of COVID-19"

You can find here instructions to replicate all the results in the article "Delays in Death Reports and their Implications for Tracking the Evolution of COVID-19" from Emilio Gutierrez, Adrian Rubli, Tiago Tavares. 

Before replicating the results in these codes, make sure you download the entire git repository.

### Replication of section "SIR model and estimation"
The results of this section were tested using `Python 3.7.4`, `GNU Fortran 9.3.0`, and `gnuplot 5.2 patchlevel 8`. Make sure your system has all required python libraries To replicate the main results:
1. To get results for Mexico move to the appropriate folder running `cd ModelEstimationMX/`
    1. Extract data from source files with `python export_data_to_fortran.py`
    1. Compile and run estimation code with `make main && ./main`
    1. (to get results using deaths data as reported or as occurred check the source code `main.f90` and uncomment where indicated)
    1. Convert the results to a csv format with `python export_data_to_python.py`
    1. (similarly you may need to comment/uncomment the source file as indicicated whether you're generating results using deaths as reported or deaths as occurred)
    1. Parameter results are summarized in the file `data/Res_00.txt` when using deaths as reported or `data/Res_13.txt` when using deaths as occurred
    1. Output files with simulations are in `data\exportfinal_002.csv` when using deaths as reported or `data\exportfinal_132.csv` when using deaths as occurred
1. To get results for England move to the appropriate folder running `cd ModelEstimationEN/` and repead the analougous instrutions of Mexico
1. At this stage one should have parameter estimates for both Mexico and England in `ModelEstimationMX/data/Res_00.txt`, `ModelEstimationMX/data/Res_13.txt`, `ModelEstimationEN/data/Res_00.txt`, and `ModelEstimationEN/data/Res_13.txt`; and simulation results in `ModelEstimationMX/data/exportfinal_002`, `ModelEstimationMX/data/exportfinal_132`, `ModelEstimationEN/data/exportfinal_002`, and `ModelEstimationEN/data/exportfinal_132`
1. Generate the paper figure moving to the appropriate folder with `cd Fig1/` and running `gnuplot fig1.gp` where the output is stored in the same folder under `fig_mx_en.eps`
1. Finally the simulation results can be analysed using the jupyter notebook `Visualize_results_estimations.ipynb` in the root folder.


    
