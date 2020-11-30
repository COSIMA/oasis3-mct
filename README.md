This version is the last official version, OASIS3-MCT_4.0, of the OASIS3-MCT climate coupler. It is stored on the branch OASIS3-MCT_4.0. It will only contain some important bug fixes until the next official release.

Since the first developments of OASIS3-MCT, it includes:
- A new tutorial SPOC was added the 29/06/2020 in examples, linked to a Small Private Online Course on OASIS3-MCT
- A hybrid MPI+OpenMP parallelisation of the SCRIP library (previously fully sequential) leading to great reduction in the offline calculation time
 of the remapping weights; 
- A new communication method, using the remapping weights to define the intermediate mapping decomposition, offering a significant gain at run time
;
- New methods introduced in the global CONSERV operation reducing its costs by one order of magnitude while still ensuring an appropriate level of 
reproducibility;
- Support for bundle coupling fields;
- Automatic coupling restart writing;
- Memory and performance upgrades;
- A new LUCIA tool for load balancing analysis;
- New memory tracking tool (gilt);
- Improved error checking and error messages;
- Expanded test cases and testing automation;
- Testing at high resolution (> 1M gridpoints), high processor counts (32k pes), and with large variable counts (> 1k coupling fields);
- Bicubic and Second Order Conservative interpolations (with the gradients of the fields being provided by the models);
- Exchange of data on only a subdomain of the global grid;
- The specification of how time statistics are written out (variable TIMER_Debug) in the configuration file “namcouple”;

Bug fixes:
- Avoid false detection of coincident segments by re-initializing LCOINC to false when a tested cell is discarded (commit d2239c4f)
- Come back to id_var_nodim defined as IN in mod_oasis_var.F90 to compile again with NEMO_4.0 and NEMO trunk with intel (commit 28f4fe59)
- Fix the treatment of the periodicity of the grids (commits 20127fd9, 68021b13, facc08c1) 
- GAUSWGT remapping: exact calculation of average distance (commit 1df003a1) ; RT : https://cerfacs.fr/wp-content/uploads/2019/08/GlobC-TR-maisonnave-gaussian_interpolation-2_2019.pdf
- Second order conservative remapping using the option FRACNNEI: the second and third weights used in the second order conservative remapping were not correctly set when using FRACNNEI (svn revision 2535)
- For upward compatibility, declaration of var_actual_shape in oasis_def_var is back to var_actual_shape(2*id_var_nodims(1)) as in OASIS3-MCT_3.0
- Redefinition of var_nodim(2) to 1 in oasis_def_var if it is equal to 0 in the models, as it represents now the number of bundle fields

Please keep us informed of your progress with OASIS3-MCT and do not forget to cite the following latest reference in your paper describing your cou
pled model results:
A. Craig, S. Valcke, L. Coquart, 2017: Development and performance of a new version of the OASIS coupler, OASIS3-MCT_3.0, Geosci. Model Dev., 10, 3
297-3308,https://doi.org/10.5194/gmd-10-3297-2017, 2017.   

If you have problems or questions, please check the forum on the OASIS3-MCT web site (https://portal.enes.org/oasis) or contact us at oasishelp@cer
facs.fr .

