import os,XDR,shutil

from avbp import plugin_avbp
from avtp import plugin_avtp
from avsp import plugin_avsp
from prissma import plugin_prissma


DISTANT_UNIX_DIRECTORY = "C3Sm_projects"

class distant_unix(XDR.Plugin):
    def __init__(self,typePlugin):
        """ This method built a new instance of plugin.
        First it constructs the plugin thanks to mother class : XDR.Plugin"""
        # Il ne faut surtout pas supprimer cette ligne
        XDR.Plugin.__init__(self, typePlugin)
        
        
        self.login = self.getPluginParam("login")
        self.machine = self.getPluginParam("machine")
        self.distantDirectory = self.getPluginParam("directory")
        self.dir2send = []
        
        
        # test connexion
        sshCommand = 'echo "Ssh connection is ok..." '
        print "Running command : "+sshCommand
        test = XDR.ssh(self.machine,self.login, sshCommand, options=" -qo PasswordAuthentication=no")
    
        # create the DISTANT_UNIX_DIRECTORY on the distant machine
        sshCommand = """bash -c "cd """+self.distantDirectory+"""; if [ ! -d """+DISTANT_UNIX_DIRECTORY+""" ] ; then mkdir """+DISTANT_UNIX_DIRECTORY+""" ; fi" """
        #XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        XDR.ssh(self.machine,self.login, sshCommand, options="")
    
    
        self.distantDirectory = os.path.join(self.distantDirectory,DISTANT_UNIX_DIRECTORY)
        
        # then create the project_name folder if needed
        project_name = XDR.getValue("name","project","meta")
        sshCommand = """bash -c "cd """+self.distantDirectory+"""; if [ ! -d """+project_name+""" ] ; then mkdir """+project_name+""" ; fi" """    
        XDR.ssh(self.machine,self.login, sshCommand, options="")
    
           
        self.distantDirectory = os.path.join(self.distantDirectory,project_name)
        
        
    
    #def sendDirectory(self,directory):
    #
    #    local_directory = os.path.abspath(directory)
    #    directory_basename = os.path.basename(local_directory)
    #    
    #    if os.path.exists(local_directory):
    #        pass
    #    else:
    #        raise Exception("Error : "+directory+" doesn't exist")
    #    
    #    self.dir2send.append(directory)
    #    print "Adding "+directory +" to the list of folders for the archive"
    #    print self.dir2send
    #    
    #    #local_directory = os.path.abspath(directory)
    #    #dist_directory = os.path.basename(local_directory)
    #    #
    #    #if os.path.exists(local_directory):
    #    #    print "Plugin : distant_unix sending directory "+local_directory +" at "+  self.distantDirectory
    #    #    
    #    #else:
    #    #    raise Exception("Error : "+directory+" doesn't exist")
    #    #
    #    #
    #    #XDR.execute("scp -r "+local_directory+" "+self.login+"@"+self.machine+":"+self.distantDirectory+"/")
    #    ##XDR.execute("rsync -a "+local_directory+" "+self.login+"@"+self.machine+":"+self.distantDirectory)
    
    
    
    @XDR.supported_applications(['tool_avsp52','tool_avbp621','tool_avtp2','tool_icritles','tool_prissma','projeteur','projeteur','hip_current','hip_script','mps_script','coolant','dummy'])
        
    def executeDistantCommand(self,command,execDirectory,appli,flags=[]):       
        
        print "Plugin : Running executeDistantCommand "+command+" in "+execDirectory+"("+appli+")"
       
    
        ########
        # INITS
        ########
        command_exe = command
        supported_applis = []
        local_directory = os.getcwd()
        project_name = XDR.getValue("name","project","meta")
        
        hostname = "KALI"
        pythonexec= "/usr/bin/python"
        
        
        hip_cur_version = "/home/rolex/HIP/1.41.0/hip-1.41.0-"+hostname
                    
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

  
        
        
        
        ########
        # AVSP #
        ########
        if appli == "tool_avsp52":
            avsp_home =  "/home/rolex/QUIET_5.3/AVSP_HOME"
            # Temporary, bug with axisym duplication in HIP v1.41.0
            hip_cur_version = "/home/rolex/HIP/1.40.1/hip-1.40.1-"+hostname
            avsptool = plugin_avsp(avsp_home,hostname,hip_cur_version,pythonexec,execDirectory)
            command_exe = avsptool.switch_avsp_tools(appli,command)
            
            
        #######
        # HIP #
        #######
        if appli == "hip_current":
            if command.startswith("-c3sm_auto_hip-"):
                command_exe = command.replace("-c3sm_auto_hip-",hip_cur_version)
            elif command == "-c3s_auto_hip_scripttxt-":
                command_exe= hip_cur_version+" < ./script.txt"

        if appli == "hip_script":
            command_exe = hip_cur_version+" < " + command
            
        
        #############
        # PROJETEUR #
        #############
        if appli == "projeteur":
            if command == "-c3s_auto_hip_dumpskin-":
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_skin.py"),"-c3sm_auto_hip-",hip_cur_version)
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_skin.py"),"-c3sm_auto_h5dump-","h5dump")
                command_exe = "/usr/bin/python ./script_skin.py"
                
        ###########
        # COOLANT #
        ###########
        if appli == "coolant":
            coolant_version =  "/home/rolex/exe_coolant_rolex/coolant09.x.LINUX"
            if command == "-c3sm_auto_coolant-":
                command_exe = coolant_version+ " < ./input_coolant.dat"
    
        ########
        # AVBP #
        ########
        if appli == "tool_avbp621":
            avbp_home = "/home/rolex/AVBP_V6.X/AVBP_D6.2.1"
            avbptool = plugin_avbp(avbp_home,hostname,hip_cur_version,pythonexec,execDirectory)
            command_exe = avbptool.switch_avbp_tools(appli,command)           

        ############
        # ICRITLES #
        ############
        if appli == "tool_icritles":
            if command == "-c3sm_auto_icritles-":
                avbp_home = "/home/rolex/AVBP_V6.X/AVBP_D6.2.1"
                command_exe = avbp_home+"/HOST/"+hostname+"/TOOLEXEC/i_crit_les.e_"+hostname

        ########
        # MPS  #
        ########
        if appli == "mps_script":
            if command.startswith("-c3sm_auto"):
                if command == "-c3sm_auto_mps-" :
                    datafile = os.path.join(self.distantDirectory,execDirectory,"aerotherm.xml")
#                   execDirectory = "/home/rolex/multiphysics_setup"
                    command_exe = "cd /home/rolex/multiphysics_setup ; mpview -aerotherm_xml "+datafile


        ########
        # AVTP #
        ########
        if appli == "tool_avtp2":
            avtp_home = "/home/rolex/AVTP/AVTP_V2.X"
            avtptool = plugin_avtp(avtp_home,hostname,hip_cur_version,pythonexec,execDirectory)
            command_exe = avtptool.switch_avtp_tools(appli,command)           

            
        ###########
        # PRISSMA #
        ###########
        if appli == "tool_prissma":
            avbp_home = "/home/rolex/AVBP_V6.X/AVBP_D6.2.1"
            prissma_home = "/home/avtp_wkdir/PRISSMA/trunk/"
            prissmatool = plugin_prissma(prissma_home,avbp_home,hostname,hip_cur_version,pythonexec,execDirectory)
            command_exe = prissmatool.switch_prissma_tools(appli,command)
        
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################



        if command_exe.startswith("-c3sm_auto_"):
            XDR.error("command was not understood: "+command_exe )
        
        
        #####################
        # SENDING DIRECTORY #
        #####################
        print "Final composition of c3sm_archive:"
        print self.dir2send
        XDR.ssh_send(self.machine,self.login, self.distantDirectory, self.dir2send, options="")   
        
       
        #####################
        # EXECUTING COMMAND #       
        #####################
        sshCommand = "cd "+self.distantDirectory+"/"+execDirectory+"; "+command_exe    
        # NB : -X allows here an interactive action       
        output = XDR.ssh(self.machine,self.login, sshCommand, options="-X")
        
        # back to the initial directory
        os.chdir(local_directory)
        return output
        
    def retrieveDirectory(self,directory):
        
        local_directory = os.path.abspath(directory)
        dist_directory = os.path.basename(local_directory)
        # Retrieve Directory
        if os.path.exists(local_directory):
            shutil.rmtree(local_directory)
    
        #XDR.ssh_retrieve(self.machine,self.login, self.distantDirectory+"/"+dist_directory, os.path.dirname(local_directory), options="")   
        #scpCommand = "scp -r "+self.login+"@"+self.machine+":"+self.distantDirectory+"/"+dist_directory+" "+os.path.dirname(local_directory)
        # DANGEROUS , Il faudrait desaliaser le rsync au cas ou l'utilisateur l'aurait surcharge"
        scpCommand = "rsync -a "+self.login+"@"+self.machine+":"+self.distantDirectory+"/"+dist_directory+" "+os.path.dirname(local_directory)
        XDR.execute(scpCommand)
        
        
    def removeDirectory(self,directory):
        sshCommand = "/bin/rm -rf "+self.distantDirectory+"/"+directory
        output = XDR.ssh(self.machine,self.login, sshCommand, options="-X")    
        #XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        
    
    
