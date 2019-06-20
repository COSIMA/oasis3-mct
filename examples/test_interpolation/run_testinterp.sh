#!/bin/ksh
#set -x

host=`uname -n`
user=`whoami`

## - Define paths
srcdir=`pwd`
datadir=$srcdir/data_oasis3
casename=`basename $srcdir`

## - Define case
if [ $# -eq 0 ] ; then
   echo "Default usage: ./run_testinterp.sh 2_1_1 (i.e. nnodes=2, nprocs=1, nthreads=1)"
   echo "nnodes: total number of nodes fr the run"
   echo "nprocs: number of MPI tasks per node"
   echo "nthreads: number of OpenMP threads per MPI task"
   n_p_t=1
   nnode=2
   mpiprocs=1
   threads=1 
else
   n_p_t=$1
   nargs=`echo $n_p_t | awk -F _ '{print NF}'`
   if [ $nargs -ne 3 ] ; then
       echo "You can run this script without argument (default nnodes=2, nprocs=1, nthreads=1 will be used)"
       echo "or as ./run_testinterp.sh nnodes_ nprocs_nthreads where:"
       echo "nnodes: total number of nodes fr the run"
       echo "nprocs: number of MPI tasks per node"
       echo "nthreads: number of OpenMP threads per MPI task"
       exit
   else
       nnode=`echo $n_p_t | awk -F _ '{print $1}'`
       mpiprocs=`echo $n_p_t | awk -F _ '{print $2}'`
       threads=`echo $n_p_t | awk -F _ '{print $3}'`
   fi
fi

######################################################################
## - User's section
# Some examples of namcouples are given in data_oasis3
# Warning: If you add any extra lines in one of the namcouple given as examples you will have to
# change the definition of SRC_GRID_TYPE, SRC_GRID_PERIOD and SRC_GRID_OVERLAP in this script (see below lines 140-142)
## - Source grids (you have the choice between bggd, ssea, icos)
## bggd is an atmosphere structured (LR) grid
## ssea is an atmosphere gaussian reduced grid (D) : no conserv2nd remapping
## icos is an atmosphere unstructured grid (U) : no bili, no bicu nor conserv2nd remapping
SRC_GRID=bggd # bggd, ssea, icos
##
## - Target grid (the only grid supported in this environment is nogt)
## nogt is an ocean structured grid (LR)
TGT_GRID=nogt
##
## - Remapping (see restrictions above)
remap=conserv1st #distwgt, bicu, bili, conserv1st, conserv2nd

## - Verification source grid type and remapping
if [ ${SRC_GRID} == "ssea" ]; then
	if [ ${remap} == "conserv2nd" ]; then
		echo "Impossible to perform conserv2nd remapping from gaussian reduced grid ssea"
		exit
	fi
fi
if [ ${SRC_GRID} == "icos" ]; then
	if [ ${remap} == "conserv2nd" ] || [ ${remap} == "bicu" ] || [ ${remap} == "bili" ]; then
		echo "Impossible to perform ${remap} remapping from unstructured grid icos"
		exit
	fi
fi

arch=linux_gfortran_openmpi  # nemo_lenovo_intel_impi, nemo_lenovo_intel_impi_openmp or beaufix_intel_impi_openmp
                               # kraken_intel_impi, kraken_intel_impi_openmp, training_computer
			       # linux_gfortran_openmpi linux_gfortran_openmpi_openmp
# For arch=beaufix_intel_impi_openmp you must put in your .bashrc 
#module load intel
#module load intelmpi
#module load netcdf
#module load hdf5/1.8.16_par_thrsaf

if [ ${arch} == linux_gfortran_openmpi ] || [ ${arch} == linux_gfortran_openmpi_openmp ]; then
   rundir=/space/${user}/${casename}_${SRC_GRID}_${TGT_GRID}_${remap}/rundir_${nnode}_${mpiprocs}_${threads}
else
   rundir=$srcdir/${casename}_${SRC_GRID}_${TGT_GRID}_${remap}/rundir_${nnode}_${mpiprocs}_${threads}
fi

## - End of user's section
######################################################################

typeset -Z4 nodes
nodes=$nnode
typeset -Z2 mpiprocesses
mpiprocesses=$mpiprocs
typeset -Z2 nthreads
nthreads=$threads

## - Name of the executables
exe1=model1
exe2=model2

## - Define number of processes to run each executable
(( nproc = $nnode * $mpiprocs ))
(( nproc_exe2 = $nproc / 2 ))
(( nproc_exe1 = $nproc - $nproc_exe2 ))

echo ''
echo '*****************************************************************'
echo '*** '$casename' : '$run
echo ''
echo "Running test_interpolation with nnodes=$nnode nprocs=$mpiprocs nthreads=$threads"
echo '*****************************************************************'
echo 'Source grid :' $SRC_GRID
echo 'Target grid :' $TGT_GRID
echo 'Rundir       :' $rundir
echo 'Architecture :' $arch
echo 'Host         : '$host
echo 'User         : '$user
echo 'Grids        : '$SRC_GRID'-->'$TGT_GRID
echo 'Remap        : '$remap
echo 'Threads      : '$threads
echo ''
echo $exe1' runs on '$nproc_exe1 'processes'
echo $exe2' runs on '$nproc_exe2 'processes'
echo ''
echo ''

## - Copy everything needed into rundir
\rm -fr $rundir/*
mkdir -p $rundir

ln -sf $datadir/grids.nc  $rundir/grids.nc
ln -sf $datadir/masks.nc  $rundir/masks.nc
ln -sf $datadir/areas.nc  $rundir/areas.nc

ln -sf $srcdir/$exe1 $rundir/.
ln -sf $srcdir/$exe2 $rundir/.

cp -f $datadir/namcouple_${SRC_GRID}_${TGT_GRID}_${remap} $rundir/namcouple

## - Grid source characteristics 
# If you add any additional lines in the namcouples given as examples you will have
# to change the 3 lines below 
SRC_GRID_TYPE=`sed -n 20p $rundir/namcouple | tr -s ' ' | cut -d" " -f2` # source grid type
SRC_GRID_PERIOD=`sed -n 17p $rundir/namcouple | tr -s ' ' | cut -d" " -f1` # "P" for periodic, "R" for non-periodic
SRC_GRID_OVERLAP=`sed -n 17p $rundir/namcouple | tr -s ' ' | cut -d" " -f2` # Number of overlapping grid points for periodic grids

echo "SRC_GRID_TYPE : $SRC_GRID_TYPE"
echo "SRC_GRID_PERIOD : $SRC_GRID_PERIOD"
echo "SRC_GRID_OVERLAP : $SRC_GRID_OVERLAP"

## - Create name_grids.dat from namcouple informations
cat <<EOF >> $rundir/name_grids.dat
\$grid_source_characteristics
cl_grd_src='$SRC_GRID'
cl_remap='$remap'
cl_type_src='$SRC_GRID_TYPE'
cl_period_src='$SRC_GRID_PERIOD'
il_overlap_src=$SRC_GRID_OVERLAP
\$end
\$grid_target_characteristics
cl_grd_tgt='$TGT_GRID'
\$end
EOF
#
cd $rundir

######################################################################
## - Creation of configuration scripts

###---------------------------------------------------------------------
### BEAUFIX
###---------------------------------------------------------------------
if [ $arch == beaufix_intel_impi_openmp ] ; then
 ncore_per_node=40
 (( cpus_per_task = $ncore_per_node * 2 / $mpiprocs ))
 timreq=12:00:00
  cat <<EOF > $rundir/run_$casename.$arch
#!/bin/bash
#SBATCH --exclusive
#SBATCH --partition=normal64
#SBATCH --job-name ${remap}_${nthreads}
# Time limit for the job
#SBATCH --time=$timreq
#SBATCH -o $rundir/$casename.o
#SBATCH -e $rundir/$casename.e
# Number of nodes
#SBATCH --nodes=$nnode
# Number of MPI tasks per node
#SBATCH --ntasks-per-node=$mpiprocs
# Number of threads per MPI task ombre de thread openmp par proc MPI = nombre de coeur par proc
#SBATCH -c $cpus_per_task
ulimit -s unlimited
# rundir must be in the TMPDIR
cd \$TMPDIR
cp $rundir/* \$TMPDIR
#
export KMP_STACKSIZE=1GB
export I_MPI_WAIT_MODE=enable
(( map = $threads - 1 ))
affin="verbose,granularity=fine,proclist=[0"
for place in \$(seq \$map); do
  affin=\${affin}",\${place}"
  echo \$place
done
echo affin1 \$affin
affin=\${affin}"],explicit"
export KMP_AFFINITY=\$affin
echo KMP_AFFINITY \$KMP_AFFINITY
export OASIS_OMP_NUM_THREADS=$threads
export OMP_NUM_THREADS=$threads

    # Binding IntelMPI
    MAP_CPU="0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39"
    INTELMPI_BINDING="-env I_MPI_PIN_PROCESSOR_LIST \${MAP_CPU}"
    I_IMPI_BINDING="-env I_MPI_PERHOST \${mpiprocs} \${INTELMPI_BINDING}"
#
time mpirun -np ${nproc_exe1} ./$exe1 : -np ${nproc_exe2} ./$exe2
#
cp * $rundir 

EOF

###---------------------------------------------------------------------
### NEMO_LENOVO_INTEL_IMPI
###---------------------------------------------------------------------
elif [ ${arch} == nemo_lenovo_intel_impi ]; then

  (( nproc = $nproc_exe1 + $nproc_exe2 ))

  cat <<EOF > $rundir/run_$casename.$arch
#!/bin/bash -l
##SBATCH --partition debug
# Nom du job
#SBATCH --job-name scrip
# Temps limite du job
#SBATCH --time=00:02:00
#SBATCH --output=$rundir/$casename.o
#SBATCH --error=$rundir/$casename.e
# Nombre de noeuds et de processus
#SBATCH --nodes=$nnode --ntasks-per-node=$mpiprocs
#SBATCH --distribution cyclic

cd $rundir

ulimit -s unlimited
module purge
module -s load compiler/intel/2015.2.164 mkl/2015.2.164 mpi/intelmpi/5.0.3.048
#
#
time mpirun -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2 
#
EOF

###---------------------------------------------------------------------
### NEMO_LENOVO_INTEL_IMPI_OPENMP sur un noeud de la machine
###---------------------------------------------------------------------
elif [ ${arch} == nemo_lenovo_intel_impi_openmp ]; then

  timreq=00:03:00

  cat <<EOF > $rundir/run_$casename.$arch
#!/bin/bash -l
#Partition
#SBATCH --partition prod
# Nom du job
#SBATCH --job-name ${n_p_t}
# Time limit for the job
#SBATCH --time=$timreq
#SBATCH --output=$rundir/$casename.o
#SBATCH --error=$rundir/$casename.e
# Number of nodes
#SBATCH --nodes=$nnode
# Number of MPI tasks per node
#SBATCH --ntasks-per-node=$mpiprocs
# Number of OpenMP threads per MPI task
#SBATCH --cpus-per-task=24

cd $rundir

export KMP_STACKSIZE=1GB
export I_MPI_PIN_DOMAIN=omp
#export I_MPI_PIN_DOMAIN=socket
export I_MPI_WAIT_MODE=enable
export KMP_AFFINITY=verbose,granularity=fine,compact
export OASIS_OMP_NUM_THREADS=$threads

time mpirun -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2 
EOF

###---------------------------------------------------------------------
### KRAKEN_INTEL_IMPI
###---------------------------------------------------------------------
elif [ ${arch} == kraken_intel_impi ]; then

  (( nproc = $nproc_exe1 + $nproc_exe2 ))

  cat <<EOF > $rundir/run_$casename.$arch
#!/bin/bash -l
#SBATCH --partition prod
# Nom du job
#SBATCH --job-name scrip
# Temps limite du job
#SBATCH --time=02:00:00
#SBATCH --output=$rundir/$casename.o
#SBATCH --error=$rundir/$casename.e
# Nombre de noeuds et de processus
#SBATCH --nodes=$nnode --ntasks-per-node=$mpiprocs
#SBATCH --distribution cyclic

cd $rundir

ulimit -s unlimited
module purge
module load compiler/intel/18.0.1.163
module load mpi/intelmpi/2018.1.163
module load lib/netcdf-fortran/4.4.4_impi
module load lib/netcdf-c/4.6.1_impi
#
#
time mpirun -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2 
#
EOF


###---------------------------------------------------------------------
### KRAKEN_INTEL_IMPI_OPENMP 
###---------------------------------------------------------------------
elif [ ${arch} == kraken_intel_impi_openmp ]; then

  timreq=00:30:00

  cat <<EOF > $rundir/run_$casename.$arch
#!/bin/bash -l
#Partition
#SBATCH --partition prod
# Nom du job
#SBATCH --job-name ${n_p_t}
# Time limit for the job
#SBATCH --time=$timreq
#SBATCH --output=$rundir/$casename.o
#SBATCH --error=$rundir/$casename.e
# Number of nodes
#SBATCH --nodes=$nnode
# Number of MPI tasks per node
#SBATCH --ntasks-per-node=$mpiprocs
# Number of OpenMP threads per MPI task
#SBATCH --cpus-per-task=36

cd $rundir
module purge
module load compiler/intel/18.0.1.163
module load mpi/intelmpi/2018.1.163
module load lib/netcdf-fortran/4.4.4_impi
module load lib/netcdf-c/4.6.1_impi

export KMP_STACKSIZE=1GB
export I_MPI_PIN_DOMAIN=omp
export I_MPI_WAIT_MODE=enable
(( map = $threads - 1 ))
affin="verbose,granularity=fine,proclist=[0"
for place in \$(seq \$map); do
  affin=\${affin}",\${place}"
  echo \$place
done
echo affin1 \$affin
affin=\${affin}"],explicit"
export KMP_AFFINITY=\$affin
echo KMP_AFFINITY \$KMP_AFFINITY
export OASIS_OMP_NUM_THREADS=$threads
export OMP_NUM_THREADS=$threads

    # Binding IntelMPI
    MAP_CPU="0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35"
    INTELMPI_BINDING="-env I_MPI_PIN_PROCESSOR_LIST \${MAP_CPU}"
    I_IMPI_BINDING="-env I_MPI_PERHOST \${mpiprocs} \${INTELMPI_BINDING}"

time mpirun -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2 
EOF

fi 

######################################################################
### - Execute the model

if [ ${arch} == training_computer ]; then
    export OASIS_OMP_NUM_THREADS=$threads
    MPIRUN=/usr/local/intel/impi/2018.1.163/bin64/mpirun
    echo 'Executing the model using '$MPIRUN
    $MPIRUN -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2 > runjob.err
elif [ ${arch} == linux_gfortran_openmpi ] || [ ${arch} == linux_gfortran_openmpi_openmp ]; then
    export OASIS_OMP_NUM_THREADS=$threads
    MPIRUN=/usr/lib64/openmpi/bin/mpirun
    echo 'Executing the model using '$MPIRUN
    $MPIRUN -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2 > runjob.err
elif [ $arch == beaufix_intel_impi_openmp ]; then
    echo 'Submitting the job to queue using sbatch'
    sbatch $rundir/run_$casename.$arch
    squeue -u $user
elif [ $arch == nemo_lenovo_intel_impi_openmp ] || [ $arch == nemo_lenovo_intel_impi ]; then
    echo 'Submitting the job to queue using sbatch'
    sbatch $rundir/run_$casename.$arch
    squeue -u $USER
elif [ $arch == kraken_intel_impi_openmp ] || [ $arch == kraken_intel_impi ]; then
    echo 'Submitting the job to queue using sbatch'
    sbatch $rundir/run_$casename.$arch
    squeue -u $USER
fi
echo $casename 'is executed or submitted to queue.'
echo 'Results are found in rundir : '$rundir 

######################################################################

