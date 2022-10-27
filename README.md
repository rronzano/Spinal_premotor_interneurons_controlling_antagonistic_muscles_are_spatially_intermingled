# Spinal_premotor_interneurons_controlling_antagonistic_muscles_are_spatially_intermingled
 
R Marckdown code and all data that allow to generate all the sections available in the print "Spinal premotor interneurons controlling antagonistic muscles are spatially intermingled" in an html format.

This code was built to make an executable version of the paper available, fully transparent and to provide the reader the ability to modify the figures and the analysis if they want to analyse/visualise data differently. For users of MATLAB, all the sections and figures of the paper are also available as a live script with the following link: https://github.com/marcobeato/Spinal_premotor_interneurons_controlling_antagonistic_muscles_are_spatially_intermingled  


**Ronzano_et_al_2022.Rmd** file contains the code to generate the html version of the manuscript with the main figures. 
**Ronzano_et_al_2022_Supp.Rmd** file contains the code to generate the html version of the supplementary figures. 
**functions_intermingled.R** file contains all the functions used by the R markdown code to generate the figures.  

The variable "**R**" (chunk 2) sets the number of replicas for bootstrap analysis. By default, R is set to 100 replicas to run the code faster. To generate the figures of the manuscript, R was set at 5000 (see methods of the paper) and R was also set at 5000 to generate the file "Ronzano_et_al_2022.html" on this repository. 
 
The variable "**bin**" (chunk 2) sets the number of contour for the density plots
 
Through the code, the variable "**kernel**" sets the kernel size of the density estimate.  

Through the code, the variable "**density**" allows to trace the density estimate when sets to 1, if sets to 0 the code will generate plots with the actual distribution of premotor neurons. 

Through the code, the variable "**concatenate**" allows to pool the data from the different samples used to generate the figures when sets to 1, if sets to 0 the code will generate the distributions or contour plots independently for each sample used to generate the figure.

All the data used for this study are stored in the folder "**data**" and are classified by groups. The non-normalised data used to generate figure 3 - figure supplement 2 are stored in the folder named "**data_raw**". 

The folder "**figures**" contains all the images used to generate the figures. 

**install.R** and **runtime.txt** files are used by Binder to upload the packages needed to run the code.  
