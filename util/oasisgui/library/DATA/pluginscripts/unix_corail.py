import os
import shutil
import XDR

DISTANT_UNIX_DIRECTORY = "C3Sm_projects"

class unix_corail(XDR.Plugin):
    def __init__(self,typePlugin):
        XDR.Plugin.__init__(self, typePlugin)
        
        print "Using corail plugin, check in terminal if password is asked"
        
        self.login = self.getPluginParam("login")
        self.machine = "corail"
        self.nbprocs = self.getPluginParam("nbprocs")
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
    
    
    # Applications supported
    @XDR.supported_applications(['avsp52','avbp621','avtp2'])
    
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

        # Creating batch file
        batchContent = [
                "#!/bin/bash",
                "#PBS -N C3Sm",
                "#PBS -j oe",
                "#PBS -l select=" + self.getPluginParam("nbprocs"),
                "#PBS -l walltime=06:00:00",
                "",
                "cat $PBS_NODEFILE",
                "nb_cpu=$(wc -l < $PBS_NODEFILE)",
                "",
                "cd $PBS_O_WORKDIR",
                "source /usr/local/bin/intelmpi.sh"]
               
############################################################################
############################################################################
############################################################################
                    
        ########
        # AVSP #
        ########
        if appli == "avsp52":
            if command == "-c3sm_auto_avsp_exe-":
                command_exe = "/usr/local/bin/python /home/rolex/AVSP/avsp53/quiet3.py &> ./quiet.out"
                avsp_command = "mpirun -np "+self.nbprocs+ " /home/rolex/AVSP/avsp53/AVSP_V5.3.CORAIL"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_avsp_exe-",avsp_command)
                anozzle_command = "/home/rolex/AVSP/avsp53/Anozzle_CORAIL.exe"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_anozzle_exe-",anozzle_command)
                    
        ########
        # AVBP #
        ########
        if appli == "avbp621":
            batchContent.append("export LD_LIBRARY_PATH=/home/cfd/avbp/LIBSUPPORT:$LD_LIBRARY_PATH")
            if command == "-c3sm_auto_avbp_exe-":
                command_exe = "mpirun -np $nb_cpu /home/rolex/AVBP_V6.X/AVBP_D6.2.1/AVBP_V6.2.1_beta.CORAIL > avbp_log.txt"
                
        ########
        # AVTP #
        ########
        if appli == "avtp2":
            if command == "-c3sm_auto_avtp_exe-":
                command_exe = "mpirun -np $nb_cpu /home/rolex/AVTP/avtp2/AVTP_V2.X.CORAIL > avtp_log.txt"
                
############################################################################
############################################################################
############################################################################

        if command_exe.startswith("-c3sm_auto_"):
            XDR.error("command was not understood: "+command_exe )

        batchContent.append(command_exe)
        
        batchFile = open(os.path.join(XDR.getCurrentRun(), "batch_C3Sm_CORAIL"), 'w')
        batchFile.write("\n".join(batchContent))
        batchFile.close()

        # create a zip archive of the directories list
        print "Final composition of c3sm_archive:"
        print self.dir2send
        
        XDR.ssh_send(self.machine,self.login, self.distantDirectory, self.dir2send, options="")   
        
        sshCommand = "cd {0}; /opt/pbs/default/bin/qsub batch_C3Sm_CORAIL".format(execDir_abspath)
        XDR.ssh(self.machine,self.login, sshCommand, options="")
       
 
    def retrieveDirectory(self,directory):
        local_directory = os.path.abspath(directory)
        dist_directory = os.path.basename(local_directory)
        XDR.ssh_retrieve(self.machine, self.login, self.distantDirectory+"/"+dist_directory, local_directory )

    def removeDirectory(self,directory):
        if directory == "*":
            sshCommand = """bash -c "cd """+self.distantDirectory+"/.. "+"""; if [ -d """+self.distantDirectory+""" ] ; then rm -r """+self.distantDirectory+""" ; fi"
            """
        else:
            sshCommand = "rm -rf "+self.distantDirectory+"/"+directory
            
        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
    
    def getRunsState(self):
        pass
