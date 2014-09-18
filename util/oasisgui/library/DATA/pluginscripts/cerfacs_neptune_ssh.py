import os,XDR,shutil


DISTANT_UNIX_DIRECTORY = "C3Sm_projects"

class cerfacs_neptune_ssh(XDR.Plugin):
    def __init__(self,typePlugin):
        XDR.Plugin.__init__(self, typePlugin)
        
        print "Using  neptune plugin, check in terminal if password is asked"
        
        self.login = self.getPluginParam("login")
        self.machine = "neptune"
        self.nbtasks = self.getPluginParam("nbtasks")
        
        self.nbcores = "none"
        if self.nbtasks == "one" :
            self.nbnodes = "1"
            self.nbcores = "1"
            self.nbcores_per_node = "1"
            
        if self.nbtasks == "two" :
            self.nbnodes = "1"
            self.nbcores = "2"
            self.nbcores_per_node = "2"
            
        if self.nbtasks == "four" :
            self.nbnodes = "1"
            self.nbcores = "4"
            self.nbcores_per_node = "4"
            
        if self.nbtasks == "eight" :
            self.nbnodes = "1"
            self.nbcores = "8"
            self.nbcores_per_node = "8"
            
        if self.nbtasks == "sixteen" :
            self.nbnodes = "1"
            self.nbcores = "16"
            self.nbcores_per_node = "16"
            
        if self.nbtasks == "thirtytwo" :
            self.nbnodes = "2"
            self.nbcores = "32"
            self.nbcores_per_node = "16"
            
        if self.nbtasks == "sixtyfour" :
            self.nbnodes = "4"
            self.nbcores = "64"
            self.nbcores_per_node = "16"
            
        if self.nbtasks == "onehundredtwentyeight" :
            self.nbnodes = "8"
            self.nbcores = "128"
            self.nbcores_per_node = "16"
            
       
        if self.nbcores == "none":
            msg_err = "Problem : option nbtasks ="+self.nbtasks+" not recognized in plugin neptune"
            error(msg_err)
            
        
        self.jobname = self.getPluginParam("jobname").replace(" ","_")
        self.dir2send = []
        self.distantDirectory = self.getPluginParam("directory")
        
        
        
        # create the DISTANT_UNIX_DIRECTORY on the distant machine
        sshCommand = """bash -c "cd """+self.distantDirectory+"""; if [ ! -d """+DISTANT_UNIX_DIRECTORY+""" ] ; then mkdir """+DISTANT_UNIX_DIRECTORY+""" ; fi" """
        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        
        self.distantDirectory = os.path.join(self.distantDirectory,DISTANT_UNIX_DIRECTORY)
        
        # then create the project_name folder if needed
        project_name = XDR.getValue("name","project","meta")
        sshCommand = """bash -c "cd """+self.distantDirectory+"""; if [ ! -d """+project_name+""" ] ; then mkdir """+project_name+""" ; fi" """    
        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        
        self.distantDirectory = os.path.join(self.distantDirectory,project_name)
        
        
        
        
        
    @XDR.supported_applications(['avsp52','avbp621','avbp70','avtp2','aap','prissma'])
    def executeDistantCommand(self,command,execDirectory,appli, flags=[]):       
        
        
        execDir_abspath = os.path.join(self.distantDirectory,execDirectory)
        print "Running executeDistantCommand "+command+" in "+execDirectory+" for appli"+appli
        ########
        # INITS
        ########
        command_exe = command
        appli_keyword = "none"
        supported_applis = []
        local_directory = os.getcwd()
        project_name = XDR.getValue("name","project","meta")
        #project_name = "NOM_PROJET"
        
        # Creating batch file
        batchContent = "#!/bin/bash \n"
        batchContent +="#PBS -N "+self.jobname+" \n"
        batchContent +="#PBS -l walltime=06:00:00 \n"
        batchContent +="#PBS -j oe \n"
        batchContent +="#PBS -l select="+self.nbnodes+":mpiprocs="+self.nbcores_per_node+":ncpus="+self.nbcores_per_node+"\n"
        batchContent +="#PBS -m ae \n"

        batchContent +="cat $PBS_NODEFILE \n "
        batchContent +="nb_cpu=$(wc -l < $PBS_NODEFILE)\n"

        batchContent +="cd "+execDir_abspath+"\n"




        # no modification before this line
################################################################################################################################
################################################################################################################################
################################################################################################################################
          
                    
        ########
        # AVSP #
        ########
        if appli == "avsp52":
            if command == "-c3sm_auto_avsp_exe-":
                batchContent+="module load avsp \n"
                command_exe = "cp /home/rolex/AVSP/avsp53/quiet3.py . ; /usr/bin/python ./quiet3.py &> ./quiet.out"
                avsp_command = "mpirun -np "+self.nbcores+ " /home/rolex/AVSP/avsp53/AVSP_V5.3.NEPTUNE"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_avsp_exe-",avsp_command)
                anozzle_command = "/home/rolex/AVSP/avsp53/Anozzle_NEPTUNE.exe"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_anozzle_exe-",anozzle_command)
                    
        ########
        # AVBP #
        ########
        if appli == "avbp621":
            #batchContent.append("export LD_LIBRARY_PATH=/home/cfd/avbp/LIBSUPPORT:$LD_LIBRARY_PATH")
            batchContent +="export LD_LIBRARY_PATH=/home/cfd/avbp/LIBSUPPORT:$LD_LIBRARY_PATH\n"
            if command == "-c3sm_auto_avbp_exe-":
                command_exe = "mpirun -np $nb_cpu /home/rolex/AVBP_V6.X/AVBP_D6.2.1/AVBP_V6.2.1_beta.NEPTUNE > avbp_log.txt"
        if appli == "avbp70":        
            batchContent +="export LD_LIBRARY_PATH=/home/cfd/avbp/LIBSUPPORT:$LD_LIBRARY_PATH\n"
            if command == "-c3sm_auto_avbp_exe-":
                #command_exe = "mpirun -np $nb_cpu /home/rolex/AVBP_V6.X/AVBP_D7.0/AVBP_V7.0_beta.NEPTUNE > avbp_log.txt"
                command_exe = "mpirun -np $nb_cpu /scratch/cfd/bonhomme/AVBP_D7.0/HOST/NEPTUNE/AVBPEXEC/AVBP_V7.0_beta.NEPTUNE > avbp_log.txt"


        ########
        # AVTP #
        ########
        if appli == "avtp2":
            if command == "-c3sm_auto_avtp_exe-":
                command_exe = "mpirun -np $nb_cpu /home/rolex/AVTP_V2.X/AVTP_V2.X.NEPTUNE > avtp_log.txt"

        ###########
        # prissma #
        ###########
        if appli == "prissma":
            if command == "-c3sm_auto_prissma_exe-":
                batchContent+="cp -r /home/rolex/PRISSMA/DATA/SPECTRAL . \n"
                command_exe = "mpirun -np $nb_cpu /home/rolex/PRISSMA/mpiprissma_V3.0.e_NEPTUNE > prissma_log.txt"
                
                
        #######
        # AAP #
        #######
        if appli == "aap":
            home_aap = "/home/rolex/AAP/"
            batchContent +="module load compiler/intel mpi/intelmpi \n"
            
                
            ###################
            # AAP : AVBP-AVTP #
            ###################
            if command.startswith("-c3sm_auto_avbp_avtp_exe-") and len(command.split(" ")) == 3:
                palm_main=home_aap+"AVBP_AVTP/"+"palm_main"
                main_avtp=home_aap+"AVBP_AVTP/"+"main_avtp"
                main_avbp=home_aap+"AVBP_AVTP/"+"main_avbp"
                nb_cores_avbp =  command.split(" ")[1]
                nb_cores_avtp =  command.split(" ")[2]
                batchContent +="export NBPROC_main_avbp="+nb_cores_avbp+" \n"
                batchContent +="export NBPROC_main_avtp="+nb_cores_avtp+" \n"
                batchContent +="cp "+home_aap+"AVBP_AVTP/avbp_avtp.pil . \n"
                command_exe = "mpirun -np 1 "+palm_main+" : -np $NBPROC_main_avtp "+main_avtp+" : -np $NBPROC_main_avbp "+main_avbp+" > cpl_log.txt"  
        
            ######################
            # AAP : PRISSMA-AVBP #
            ######################
            
            if command.startswith("-c3sm_auto_avbp_prissma_exe-")  and len(command.split(" ")) == 3:
                palm_main=home_aap+"AVBP_PRISSMA/"+"palm_main"
                main_prissma=home_aap+"AVBP_PRISSMA/"+"main_prissma"
                main_avbp=home_aap+"AVBP_PRISSMA/"+"main_avbp"
                nb_cores_avbp =  command.split(" ")[1]
                nb_cores_prissma =  command.split(" ")[2]
                batchContent +="export NBPROC_main_avbp="+nb_cores_avbp+" \n"
                batchContent +="export NBPROC_main_prissma="+nb_cores_prissma+" \n"
                batchContent +="cp "+home_aap+"AVBP_PRISSMA/avbp_prissma.pil . \n"
                batchContent+="cp -r /home/rolex/PRISSMA/DATA/SPECTRAL ./PRISSMA01/ \n"
                command_exe ="mpirun -np 1 "+palm_main+" : -np $NBPROC_main_prissma "+main_prissma+" : -np $NBPROC_main_avbp "+main_avbp+" > cpl_log.txt"  
        
            ###########################
            # AAP : AVBP-AVTP-PRISSMA #
            ###########################
           
            if command.startswith("-c3sm_auto_avbp_avtp_prissma_exe-")  and len(command.split(" ")) == 4 :
                nb_cores_avbp =  command.split(" ")[1]
                nb_cores_avtp =  command.split(" ")[2]
                nb_cores_prissma =  command.split(" ")[3]
                palm_main=home_aap+"AVBP_AVTP_PRISSMA/"+"palm_main"
                main_prissma=home_aap+"AVBP_AVTP_PRISSMA/"+"main_prissma"
                main_avbp=home_aap+"AVBP_AVTP_PRISSMA/"+"main_avbp"                
                main_avtp=home_aap+"AVBP_AVTP_PRISSMA/"+"main_avtp"
                batchContent +="cp "+home_aap+"AVBP_AVTP_PRISSMA/3codescoupling.pil . \n"
                batchContent+="cp -r /home/rolex/PRISSMA/DATA/SPECTRAL ./PRISSMA01/ \n"
                batchContent +="export NBPROC_main_avbp="+nb_cores_avbp+" \n"
                batchContent +="export NBPROC_main_avtp="+nb_cores_avtp+" \n"
                batchContent +="export NBPROC_main_prissma="+nb_cores_prissma+" \n"
                command_exe ="mpirun -np 1 "+palm_main+" : -np $NBPROC_main_avtp "+main_avtp+" : -np $NBPROC_main_prissma "+main_prissma+" : -np $NBPROC_main_avbp "+main_avbp+" > cpl_log.txt" 

       
################################################################################################################################
################################################################################################################################
################################################################################################################################
       # no modification after this line
        if command_exe.startswith("-c3sm_auto_"):
            XDR.error("command was not understood: "+command_exe )
        batchContent +=command_exe



        batchFile = open(os.path.join(XDR.getCurrentRun(), "batch_C3Sm_NEPTUNE"), 'w')
        batchFile.write(batchContent)
        batchFile.close()

        # create a zip archive of the directories list
        print "Final composition of c3sm_archive:"
        print self.dir2send
        
        XDR.ssh_send(self.machine,self.login, self.distantDirectory, self.dir2send, options="")   
        
        sshCommand = "cd {0}; /opt/pbs/default/bin/qsub batch_C3Sm_NEPTUNE".format(execDir_abspath)
        XDR.ssh(self.machine,self.login, sshCommand, options="")
        
        
        
    def retrieveDirectory(self,directory):
        
        local_directory = os.path.abspath(directory)
        dist_directory = os.path.basename(local_directory)
        # Retrieve Directory
        shutil.rmtree(local_directory)
        scpCommand = "scp -pr "+self.login+"@"+self.machine+":"+self.distantDirectory+"/"+dist_directory+" "+os.path.dirname(local_directory)
    
        XDR.execute(scpCommand)
        
        
    def removeDirectory(self,directory):
        if directory == "*":
            sshCommand = """bash -c "cd """+self.distantDirectory+"/.. "+"""; if [ -d """+self.distantDirectory+""" ] ; then rm -r """+self.distantDirectory+""" ; fi"
            """
        else:
            sshCommand = "rm -rf "+self.distantDirectory+"/"+directory
            
        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        
    
