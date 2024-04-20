# AMG_base

AMB_base is a R version of the AMGv2 model (Clivot et al., 2019: https://doi.org/10.1016/j.envsoft.2019.04.004).
It is a simple model simulating topsoil organic carbon (SOC) stocks in arable cropping systems, at an annual time step.

This R code gives identical results than the C++ version "AMG_recherche".
However, it is a basic version because it does not include the calculation of C3 and C4 stocks, optimisation procedure, or graphical representation.

## Directories

- `inputs` includes the input files
- `scripts` includes the R code
- `outputs` includes the output files produced by the R code

## Model description

The full model description is available in the original publication of Clivot et al. (2019) at https://doi.org/10.1016/j.envsoft.2019.04.004 or https://hal.science/hal-02161566.

The input parameters included in the csv files `parameters_EOM`, `parameters_MINERALIZATION` and `parameters_PLANT` (`inputs` directory) are also available at https://doi.org/10.57745/MEQQIX. They are provided by the AMG Consortium.

## Requirements

This R script has been written and tested for R versions ≥ 4.0.0.

It requires the following packages: `readr`, `dplyr`, `tidyr` and `stringr`.

## Usage

The model can be run using the R file `AMG_script` in the `scripts` directory:
- All the inputs and the parameter files (.csv files) must be located in the `inputs` directory.
- The user must first fill in the csv files `inputs_MANAGEMENT`, `inputs_METEO` and `inputs_SOIL` to discribe the simulated cropping system(s) (an example is provided).
- The structure and the names of the input and parameter files should not be changed. Do not add columns with the same names as the existing ones.
- The output files (`outputs_C_SUPPLY` and `outputs_SOC_STOCKS`) are saved in the `outputs` directory.
- Input and output variable definitions, types and units are available in the csv file `AMG_base_variable_list`.

## Contact

Fabien Ferchaud <fabien.ferchaud@inrae.fr>
