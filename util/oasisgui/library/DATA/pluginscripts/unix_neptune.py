import os,XDR,shutil


DISTANT_UNIX_DIRECTORY = "C3Sm_projects"

class unix_neptune(XDR.Plugin):
    def __init__(self,typePlugin):
        XDR.Plugin.__init__(self, typePlugin)
        
        print "Using  neptune plugin, check in terminal if password is asked"
        
        self.login = self.getPluginParam("login")
        self.machine = "neptune"
        self.nbnodes = self.getPluginParam("nbnodes")
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
    
    def sendDirectory(self,directory):

        
        local_directory = os.path.abspath(directory)
        directory_basename = os.path.basename(local_directory)
        
        if os.path.exists(local_directory):
            pass
        else:
            raise Exception("Error : "+directory+" doesn't exist")
        # Je te met un exemple avec un scp (avec account et machine definis dans le init:
        self.dir2send.append(directory)
        print "Adding "+directory +" to the list of folders for the archive"
        print self.dir2send
      
        #XDR.execute("scp -pr "+local_directory+" "+self.login+"@"+self.machine+":"+self.distantDirectory + "/")
        
        
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
        batchContent =  """#!/bin/bash
#PBS -N C3Smjob
#PBS -l walltime=06:00:00
#PBS -j oe
#PBS -l select="""+self.getPluginParam("nbnodes")+"""
#PBS -m ae

cat \$PBS_NODEFILE
nb_cpu=\$(wc -l < \$PBS_NODEFILE)

        
cd """+execDir_abspath+"""
"""       
        
        #####################
        # SECTION DEPENDING ON APPLICATIONS
        ####################
        supported_applis.append("avsp52") 
        supported_applis.append("avbp621")
        supported_applis.append("avbp70")
        supported_applis.append("avtp2")
        supported_applis.append("aap01")
        if appli not in supported_applis:
            XDR.error("this application requires the application tag "+ appli+ ", which is not supported by the plugin unix_neptune.")
            return
               
               
                    
        ########
        # AVSP #
        ########
        if appli == "avsp52":
            if command == "-c3sm_auto_avsp_exe-":
                command_exe = "cd "+ execDir_abspath+" ; cp /home/rolex/AVSP/avsp53/quiet3.py . ; /usr/bin/python ./quiet3.py &> ./quiet.out"
                avsp_command = "mpirun -np "+self.nbnodes+ " /home/rolex/AVSP/avsp53/AVSP_V5.3.CORAIL"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_avsp_exe-",avsp_command)
                anozzle_command = "/home/rolex/AVSP/avsp53/Anozzle_CORAIL.exe"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_anozzle_exe-",anozzle_command)
                    
        ########
        # AVBP #
        ########
        if appli == "avbp621":
            if command == "-c3sm_auto_avbp_exe-":
                command_exe = "/home/rolex/AVBP_V6.X/AVBP_D6.2.1/AVBP_V6.2.1_beta.NEPTUNE > avbp_log.txt"

        if appli == "avbp70":
            if command == "-c3sm_auto_avbp_exe-":
                command_exe = "/home/rolex/AVBP_V6.X/AVBP_D7.0/AVBP_V7.0_beta.NEPTUNE > avbp_log.txt"                
        ########
        # AVTP #
        ########
        if appli == "avtp2":
            if command == "-c3sm_auto_avtp_exe-":
                command_exe = "/home/rolex/AVTP_V2.X/AVTP_V2.X.NEPTUNE > avtp_log.txt"
                
                
        ###################
        # AAP : AVBP-AVTP #
        ###################
        if appli == "aap01":
            if command == "-c3sm_auto_aap_exe-":
                batchContent +="""
module load compiler/intel mpi/intelmpi

export NBPROC_main_avtp="""+self.getPluginParam("nbcavtp")+"""
export NBPROC_main_avbp="""+self.getPluginParam("nbcavbp")+"""
palm_main=/home/rolex/AAP/AVBP_AVTP/palm_main
main_avtp=/home/rolex/AAP/AVBP_AVTP/main_avtp
main_avbp=/home/rolex/AAP/AVBP_AVTP/main_avbp
cp /home/rolex/AAP/AVBP_AVTP/avbp_avtp.pil .
"""
                command_exe = " -np 1 \$palm_main : -np \$NBPROC_main_avtp \$main_avtp : -np \$NBPROC_main_avbp \$main_avbp > cpl_log.txt"  
        
        ######################
        # AAP : AVBP-PRISSMA #
        ######################
        if appli == "aap02":
            if command == "-c3sm_auto_aap_exe-":
                batchContent +="""
module load compiler/intel mpi/intelmpi

export NBPROC_main_prissma="""+self.getPluginParam("nbcprissma")+"""
export NBPROC_main_avbp="""+self.getPluginParam("nbcavbp")+"""
palm_main=/home/rolex/AAP/AVBP_AVTP/palm_main
main_avtp=/home/rolex/AAP/AVBP_AVTP/main_prissma
main_avbp=/home/rolex/AAP/AVBP_AVTP/main_avbp
cp /home/rolex/AAP/AVBP_AVTP/avbp_prissma.pil .
"""
                command_exe = " -np 1 \$palm_main : -np \$NBPROC_main_prissma \$main_prissma : -np \$NBPROC_main_avbp \$main_avbp > cpl_log.txt"  
        
        ###########################
        # AAP : AVBP-AVTP-PRISSMA #
        ###########################
        if appli == "aap03":
            if command == "-c3sm_auto_aap_exe-":
                batchContent +="""
module load compiler/intel mpi/intelmpi

export NBPROC_main_prissma="""+self.getPluginParam("nbcprissma")+"""
export NBPROC_main_avbp="""+self.getPluginParam("nbcavbp")+"""
export NBPROC_main_avtp="""+self.getPluginParam("nbcavtp")+"""
palm_main=/home/rolex/AAP/AVBP_AVTP/palm_main
main_avtp=/home/rolex/AAP/AVBP_AVTP/main_avtp
main_avbp=/home/rolex/AAP/AVBP_AVTP/main_avbp
main_avtp=/home/rolex/AAP/AVBP_AVTP/main_prissma
cp /home/rolex/AAP/AVBP_AVTP/avbp_avtp_prissma.pil .
"""
                command_exe = " -np 1 \$palm_main : -np \$NBPROC_main_prissma \$main_prissma : -np \$NBPROC_main_avtp \$main_avtp : -np \$NBPROC_main_avbp \$main_avbp > cpl_log.txt" 


        if appli == "avbp621":
            batchContent +="""export LD_LIBRARY_PATH=/home/cfd/avbp/LIBSUPPORT:$LD_LIBRARY_PATH"""+"\n"
        batchContent+="mpirun "+command_exe+"\n"

        if appli == "avbp70":
            batchContent +="""export LD_LIBRARY_PATH=/home/cfd/avbp/LIBSUPPORT:$LD_LIBRARY_PATH"""+"\n"
        batchContent+="mpirun "+command_exe+"\n"        
      
        
        # create a zip archive of the directories list
        print "Final composition of c3sm_archive:"
        print self.dir2send
        #print "tar -cvf c3sm_archive.tar "+" ".join(self.dir2send)
        XDR.execute("tar -cvf c3sm_archive.tar "+" ".join(self.dir2send))
        
        #sending archive
        print "Sending archive..."
        XDR.execute("scp -pr c3sm_archive.tar "+self.login+"@"+self.machine+":"+self.distantDirectory + "/")
        
        # extract archive
        
        sshCommand = "cd "+self.distantDirectory+" ; tar -xvf "+os.path.join(self.distantDirectory,"c3sm_archive.tar")
        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        
        
        sshCommand = """cd """+execDir_abspath+"""; echo " """+batchContent+""" " > batch_C3Sm_neptune ; qsub batch_C3Sm_neptune"""
        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        
        # back to the initial directory
        os.chdir(local_directory)
        
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
        
    
