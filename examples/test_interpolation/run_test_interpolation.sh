#!/bin/ksh
#set -x
######################################################################
#
host=`uname -n`
user=`whoami`
#
## - Define paths
srcdir=`pwd`
datadir=$srcdir/data_oasis3
datagrid=$srcdir/data_oasis3
casename=`basename $srcdir`
echo $datadir
echo $datagrid
#
############### User's section #######################################
# Source and target grids
SRC_GRID=torc # nogt ssea bggd icos
TGT_GRID=lmdz # nogt ssea bggd icos
# Interpolation = nn, bili, bicu, fracarea, fracnnei, fracarea2nd
interp=fracnnei
#
## - Define architecture and coupler 
arch=nemo_lenovo_gfortran_openmpi   
#
# - Define number of processes to run each executable 
#   The toy only runs in monoprocessor
    nproc_exe1=1
    nproc_exe2=1
#
## - Define rundir
if [ ${arch} == romulus_pgi_mpich ]; then
    MPIRUN=/usr/local/pgi/linux86-64/14.6/mpi/mpich/bin/mpirun
    rundir=/wkdir/globc/${user}/OASIS3-MCT/oasis3-mct/examples/${casename}/work_${casename}
elif [ ${arch} == training_computer ]; then
    MPIRUN=/opt/intel/impi/5.1.3.210/bin64/mpirun
    rundir=${HOME}/oasis3-mct/examples/${casename}/work_${casename}
elif [ ${arch} == nemo_lenovo_gfortran_openmpi ]; then
    MPIRUN=/data/softs/mpi/openmpi/1.8.4/bin/mpirun
    rundir=/scratch/globc/coquart/COMPARE_ERROR_TRUNK2187_BR_GABI/oasis3-mct_trunk_2187/examples/${casename}/work_${casename}_${SRC_GRID}_${TGT_GRID}_${interp}
elif [ ${arch} == nemo_lenovo_intel_impi ] || [ ${arch} == nemo_lenovo_intel_impi_openmp ]; then
    MPIRUN=/data/softs/intel/impi/5.0.3.048/intel64/bin/mpirun
    rundir=/scratch/globc/$USER/OASIS3-MCT/oasis3-mct_3.0/examples/${casename}/work_${casename}
elif [ ${arch} == neptune_gfortran ]; then
    rundir=/scratch/globc/coquart/oasis3-mct_buildbot/examples/${casename}/work_${casename}
elif [ ${arch} == napali ]; then
    PATH=/usr/local/pgi/linux86-64/2011/mpi/mpich/bin:/usr/local/pgi/linux86-64/2011/bin:$PATH
    MPIRUN=/usr/local/pgi/linux86-64/2011/mpi/mpich/bin/mpirun
    rundir=/data2/${user}/oasis3-mct_buildbot/examples/${casename}/work_${casename}
elif [ ${arch} == beaufix ]; then
# The toy must be directly in the TMPDIRs the jobs can only run in the TMPDIR
    rundir=/scratch/utmp/cglo355/oasis3-mct/examples/tutorial/work_${casename}
elif [ ${arch} == curie ]; then
    rundir=/ccc/work/cont005/pa0490/coquartl/oasis3-mct/examples/${casename}/work_${casename}
elif [ ${arch} == jade ]; then
    rundir=/data/11coqu/oasis3-mct/examples/${casename}/work_${casename}
elif [ ${arch} == neptune_gfortran ]; then
    rundir=/scratch/globc/coquart/oasis3-mct_buildbot/examples/${casename}/work_${casename}
elif [ ${arch} == ada ]; then
  MPIRUN=/opt/intel/impi/4.1.0.024/intel64/bin/mpirun
  rundir=/workgpfs/rech/ces/rces980/modBRETAGNE/oasis3-mct/examples/tutorial/work_${casename}
fi
#
############### End of user's section ################################
#
### - Name of the executables
    exe1=model1
    exe2=model2
#
### - Define number of processes to run each executable
###   The toy cannot be ran in parallel
    nproc_exe1=1
    nproc_exe2=1
#
echo '*****************************************************************'
echo '*** '$casename' : '$run
echo ''
echo 'Source grid :' $SRC_GRID
echo 'Target grid :' $TGT_GRID
echo 'Rundir       :' $rundir
echo 'Architecture :' $arch
echo 'Host         : '$host
echo 'User         : '$user
echo ''
echo $exe1' runs on '$nproc_exe1 'processes'
echo $exe2' runs on '$nproc_exe2 'processes'
echo ''
echo ''
######################################################################
###
### 1. Copy source example directory containing everything needed
###    into rundir

\rm -fr $rundir/*
mkdir -p $rundir

ln -sf $datagrid/grids.nc  $rundir/grids.nc
ln -sf $datagrid/masks.nc  $rundir/masks.nc
ln -sf $datagrid/areas.nc  $rundir/areas.nc

cp -f $srcdir/$exe1 $rundir/.
cp -f $srcdir/$exe2 $rundir/.

cp -f $datadir/namcouple_${SRC_GRID}_${TGT_GRID}_${interp} $rundir/namcouple
#
# Grid source characteristics
SRC_GRID_TYPE=`sed -n 26p $rundir/namcouple | tr -s ' ' | cut -d" " -f2` # source grid type
SRC_GRID_PERIOD=`sed -n 23p $rundir/namcouple | tr -s ' ' | cut -d" " -f1` # "P" for periodic, "R" for non-periodic
SRC_GRID_OVERLAP=`sed -n 23p $rundir/namcouple | tr -s ' ' | cut -d" " -f2` # Number of overlapping grid points for periodic grids
cat <<EOF >> $rundir/name_grids.dat
\$grid_source_characteristics
cl_grd_src='$SRC_GRID'
cl_remap='$interp'
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
#
######################################################################
###
### 3. Creation of configuration scripts

###---------------------------------------------------------------------
### Linux
###---------------------------------------------------------------------

if [ $arch == napali ]; then
if [ $nproc_exe1 == 1 ]; then
   cat <<EOF >> $rundir/appl-linux.conf
$host 0 $rundir/$exe1
EOF
else
   cat <<EOF >> $rundir/appl-linux.conf
$host 0 $rundir/$exe1
EOF

  count=1
  while [[ $count -lt $nproc_exe1 ]];do
   cat <<EOF >> $rundir/appl-linux.conf
$host 1 $rundir/$exe1
EOF
   (( count += 1 ))
  done
fi

  count=0
  while [[ $count -lt $nproc_exe2 ]];do
   cat <<EOF >> $rundir/appl-linux.conf
$host 1 $rundir/$exe2
EOF
   (( count += 1 ))
  done

elif [ $arch == tioman_gfortan_mpich2 ]; then 
   cat <<EOF >> $rundir/mpd.hosts
$host 0: $nproc_exe1
$host 1: $nproc_exe2
EOF

###---------------------------------------------------------------------
### BEAUFIX
###---------------------------------------------------------------------
elif [ $arch == beaufix ] ; then

   (( nproc = $nproc_exe1 + $nproc_exe2 ))

  cat <<EOF > $rundir/run_$casename.$arch
#!/bin/bash
#SBATCH --time=01:00:00
#SBATCH -p  normal32        # partition/queue
#SBATCH --job-name=toys     # job name
#SBATCH -N 1                # number of nodes
#SBATCH -n $nproc                # number of procs
#SBATCH -o job.out%j
#SBATCH -o job.err%j
#SBATCH --exclusive

ulimit -s unlimited
# rundir must be in the TMPDIR
cd $rundir
module load intel/13.1.4.183
module load intelmpi/4.1.1.036
module load netcdf/4.3.0 
#
#
time mpirun -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2
#
EOF

###---------------------------------------------------------------------
### NEPTUNE
###---------------------------------------------------------------------
elif [ $arch == neptune_gfortran ] ; then

  cat <<EOF > $rundir/run_$casename.$arch
# Nom du job
#PBS -N tests
# Temps limite du job
#PBS -l walltime=00:10:00
# Nombre de processus
#PBS -l select=1:mpiprocs=16:ncpus=16
#PBS -l place=scatter:excl

cd $rundir

export LD_LIBRARY_PATH=/usr/lib64:$LD_LIBRARY_PATH
ulimit -s unlimited
#
#
time mpirun -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2
#
EOF

###---------------------------------------------------------------------
### NEMO_LENOVO_GFORTRAN_OPENMPI
###---------------------------------------------------------------------
elif [ $arch == nemo_lenovo_gfortran_openmpi ] ; then

  (( nproc = $nproc_exe1 + $nproc_exe2 ))

  cat <<EOF > $rundir/run_$casename.$arch
#!/bin/bash -l
# Nom du job
#SBATCH --job-name tutorial
# Temps limite du job
#SBATCH --time=00:10:00
#SBATCH --output=$rundir/$casename.o
#SBATCH --error=$rundir/$casename.e
# Nombre de noeuds et de processus
#SBATCH --nodes=1 --ntasks-per-node=$nproc
#SBATCH --distribution cyclic

cd $rundir

ulimit -s unlimited
module purge
module load compiler/gcc/4.8.4 
module load mpi/openmpi/1.8.4
#
time mpirun -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2
#
EOF


###---------------------------------------------------------------------
### NEMO_LENOVO_INTEL_IMPI
###---------------------------------------------------------------------
elif [ ${arch} == nemo_lenovo_intel_impi ]; then

  (( nproc = $nproc_exe1 + $nproc_exe2 ))

  cat <<EOF > $rundir/run_$casename.$arch
#!/bin/bash -l
# Nom du job
#SBATCH --job-name toys
# Temps limite du job
#SBATCH --time=01:00:00
#SBATCH --output=$rundir/$casename.o
#SBATCH --error=$rundir/$casename.e
# Nombre de noeuds et de processus
#SBATCH --nodes=1 --ntasks-per-node=24
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

  (( nproc = $nproc_exe1 + $nproc_exe2 ))

  cat <<EOF > $rundir/run_$casename.$arch
#!/bin/bash -l
# Nom du job
#SBATCH --job-name openmp
# Temps limite du job
#SBATCH --time=12:00:00
#SBATCH --output=$rundir/$casename.o
#SBATCH --error=$rundir/$casename.e
# Nombre de noeud
#SBATCH -N 1
# Nombre de process MPI par noeud = nombre de proc par noeud
#SBATCH --ntasks-per-node=2
# Nombre de thread openmp par proc MPI = nombre de coeur par proc
#SBATCH --cpus-per-task=12
#SBATCH --mail-user=coquart@cerfacs.fr
#SBATCH --mail-type=END

cd $rundir

export KMP_STACKSIZE=1GB
export I_MPI_PIN_DOMAIN=socket
export KMP_AFFINITY=verbose,granularity=fine,compact
export OMP_NUM_THREADS=1
#

time mpirun -genv I_MPI_DEBUG 5  -np 1 ./$exe1 : -np 1 ./$exe2 
EOF


###---------------------------------------------------------------------
### CURIE (normal nodes)
###---------------------------------------------------------------------
elif [ $arch == curie ] ; then

   cat <<EOF > $rundir/appl-curie.conf
$nproc_exe1 $rundir/$exe1
$nproc_exe2 $rundir/$exe2
EOF

      (( nproc = $nproc_exe1 + $nproc_exe2 ))

  cat <<EOF > $rundir/run_$casename.$arch
#!/bin/bash
#MSUB -r test_interp # Request name
#MSUB -n $nproc # Number of tasks to use
#MSUB -T 3600 # Elapsed time limit in seconds
#MSUB -o $casename_%I.o # Standard output. %I is the job id
#MSUB -e $casename_%I.e # Error output. %I is the job id
#MSUB -A gen6028 # Project ID
set -x
cd $rundir
ccc_mprun -f appl-curie.conf
EOF

###-----------------------------------------------------------------
### JADE - CINES
###-----------------------------------------------------------------
elif [ $arch == jade ] ; then
(( NNODE = (  $nproc_exe1 + $nproc_exe2 ) / 8 ))
(( RESTE = (  $nproc_exe1 + $nproc_exe2 ) - ( $NNODE * 8 ) ))
if [[ $RESTE -gt 0 ]]; then
 (( NNODE = $NNODE + 1 ))
fi

echo $NNODE

cat << EOF > $rundir/run_$casename.$arch
#PBS -S /bin/bash
#PBS -N OASIS
#PBS -l walltime=00:10:00
#PBS -l select=$NNODE:ncpus=8:mpiprocs=8
#PBS -o $rundir/out
#PBS -e $rundir/err
#PBS -j oe
#
set -evx
module load netcdf
cd $rundir
cat $PBS_NODEFILE
## Lancement executable
which mpiexec
export MPI_GROUP_MAX=100
time mpiexec -n $nproc_exe1 ./$exe1 : -n $nproc_exe2 ./$exe2 

EOF

###---------------------------------------------------------------------
### ADA
###---------------------------------------------------------------------
elif [ $arch == ada ] ; then

 (( nproc = $nproc_exe1 + $nproc_exe2 ))

  cat <<EOF > $rundir/run_$casename.$arch
#!/bin/ksh
# ######################
# ##   ADA IDRIS   ##
# ######################
# Nom de la requete
# @ job_name = ${casename}
# Type de travail
# @ job_type = parallel
# Fichier de sortie standard
# @ output = Script_Output_${casename}.000001
# Fichier de sortie erreur (le meme)
# @ error = Script_Output_${casename}.000001
# Nombre de processus demandes
# @ total_tasks = ${nproc}
# @ environment = "BATCH_NUM_PROC_TOT=32"
# Temps CPU max. par processus MPI hh:mm:ss
# @ wall_clock_limit = 0:30:00
# Fin de l entete
# @ queue
#
# pour avoir l'echo des commandes
set -x

# on se place dans le repertoire rundir
cd ${rundir}

module load netcdf
module load hdf5

#
export KMP_STACKSIZE=64m
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/smplocal/pub/NetCDF/4.1.3/lib

poe -pgmmodel MPMD -cmdfile run_file
#
EOF

fi 

######################################################################
###
### 4. Execute the model

if [ $arch == training_computer ] || [ $arch == romulus_pgi_mpich ]; then
    echo 'Executing the model using '$MPIRUN 
    $MPIRUN -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2 > runjob.err
elif [ $(uname -n) == nemo1 ] || [ $(uname -n) == nemo2 ] || [ $(uname -n) == nemoglobc1 ] || [ $(uname -n) == nemoglobc2 ] || [ $(uname -n) == nemoglobc3 ] || [ $(
uname -n) == nemoglobc4 ]; then
    echo 'Submitting the job to queue using sbatch'
    sbatch $rundir/run_$casename.$arch
    squeue -u $USER
elif [ $arch == neptune_gfortran ]; then
    echo 'Submitting the job to queue using qsub'
    qsub $rundir/run_$casename.$arch
    qstat | grep $user
elif [ $arch == napali ]; then
    echo 'Executing the model using '$MPIRUN 
    $MPIRUN -p4pg appl-linux.conf ./$exe1 > runjob.err
elif [ $arch == beaufix ]; then
    echo 'Submitting the job to queue using sbatch'
    sbatch $rundir/run_$casename.$arch
    squeue -u $user
elif [ $arch == curie ]; then
    ccc_msub $rundir/run_$casename.$arch
    ccc_mpp | grep $user
elif [ $arch == jade ] ; then
    qsub $rundir/run_$casename.$arch
    qstat -awu $user
elif [ $arch == ada ]; then
    echo 'Submitting the job to queue using llsubmit'
    llsubmit $rundir/run_$casename.$arch
fi

echo $casename 'is executed or submitted to queue.'
echo 'Results are found in rundir : '$rundir 

######################################################################

