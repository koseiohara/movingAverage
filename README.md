# movingAverage

Developer   : Kosei Ohara

Environment
    ifort 19.1.3.304 20200925
    GNU Make 3.82

This program is to compute moving avaraged field.  
This is available for various grid sizes.  

## Features
- This program is useful to compute moving avarage for various grid sizes.
- This can be used for 1, 2, and 3 dimension data.
- Only simple moving average can be computed (meaning that unavailable to compute weighted moving average)

## Namelists

You only need to edit namelists to execute this program.

### files.nml

Input and output files are setted in this file.  
'input_fname' and 'output_fname' are the input and the output files, respectively.

### filter.nml

'filterlen' is the length of the moving average filter.  
This parameter must be a odd number.  
'filtername' must be "simple" in the previous version.  
This parameter is to set wether the the moving average method is simple moving average or weighted moving average, 
"simple" is the filter without any weight.

### grid.nml

Grid size of input and output date is defined in this namelist.  
'nx', 'ny', and 'nz' need to be setted.  
The grid size is the product of these three.

### recinfo.nml

Number of parameters in the input file, The initial record to read, and the number of time steps are defined.  
Their name of variables are 'varnum', 'input_initialRecord', and 'tnum', respectively.

