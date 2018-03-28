import os,XDR,shutil

class turbomeca_btmclx2_soumi(XDR.Plugin):
    
    def __init__(self,typePlugin):
        """ This method built a new instance of plugin.
        First it constructs the plugin thanks to mother class : XDR.Plugin"""

        XDR.Plugin.__init__(self, typePlugin)
               
        self.directory = self.getPluginParam("directory") 
        self.nbprocs = self.getPluginParam("nbprocs") 
        self.memory = self.getPluginParam("memory") 
        self.duration =  self.getPluginParam("duration") 
        self.external = self.getPluginParam("external")  
        
        
        self.dir2send = []
        
        
    @XDR.supported_applications(['avsp52','avbp621','yales2_1007','yales2_1008'])  
    def executeDistantCommand(self,command,execDirectory,appli,flags=[]):
        """ This command executes command in execDirectory on distant machine.
        Flags can be specified, they depend on the code and the plugin
        """

        ########
        # INITS
        ########
        command_exe = command
        appli_keyword = "none"
        local_directory = os.getcwd()
        project_name = XDR.getValue("name","project","meta")
       #####################
        # SECTION FOR ALL APPLICATIONS
        ####################
                
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
        
        ########
        # AVSP #
        ########
        if appli == "avsp52":
            avsp_home = "/Avbp/versions/ARCHIVE_QUIET/QUIET/QUIET_TM/AVSP_HOME_2"
            if command == "-c3sm_auto_avsp_exe-":
                command_exe = "cd "+execDirectory+" ; python "+avsp_home+"/TOOLS/quiet3.py"
                appli_keyword = "AVSP"
                avsp_command = "mpirun -np "+self.nbprocs+ " " + avsp_home+"/HOST/BTMCLX2/AVSPEXEC/AVSP_V5.2.BTMCLX2"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_avsp_exe-",avsp_command)
                anozzle_command = "$AVSP_HOME/HOST/BTMCLX2/anozzle.exe"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_anozzle_exe-",anozzle_command)
        ########
        # AVBP #
        ########
        if appli == "avbp621":
            avbp_home ="/Avbp/versions/AVBP_V6.2"
            if command == "-c3sm_auto_avbp_exe-":
                command_exe = avbp_home+"/HOST/BTMCLX2/AVBPEXEC/AVBP_CURRENT.BTMCLX2 > avbp_log.txt"

        
        ##########
        # YALES2 #
        ########## 
	if appli in ["yales2_1007","yales2_1008"] :
	  #Handle Version YALES TODO
	  if command == "-c3sm_YALES2_auto_exe-":
	    appli_keyword = "yales2"	
	    # Execute in execDirectory
	    os.chdir(execDirectory)																	
	    XDR.execute("make")
	    XDR.execute("mkdir dump")
	    XDR.execute("mkdir postprocdump")				
	    XDR.execute("touch warning.log")
	    os.chdir(local_directory)
	    print local_directory
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


        #####################
        # SECTION FOR ALL APPLICATIONS
        ####################


        # Creating soumi.cmd


        batchfile = open('soumi.cmd', 'w')
        batchfile.write("FICH_IN="+local_directory+"/run_c3sm.tar\n")
        batchfile.write("FICH_OUT=./"+execDirectory+".tar\n")
        batchfile.write("EXECUTABLE="+command_exe+"\n")
        batchfile.write("PREEXEC= module load apps/python_epd-6.3.2 ;mkdir "+project_name+"; cd "+project_name+"; mv ../run_c3sm.tar . ;tar xvf run_c3sm.tar;rm -f run_c3sm.tar\n")
        batchfile.write("POSTEXEC=tar -cvf "+execDirectory+".tar ./\n")
        batchfile.close()
        self.dir2send.append('soumi.cmd')
        
        # create a tar archive of the directories list
        XDR.execute("tar cvf run_c3sm.tar "+" ".join(self.dir2send))

        # Creating soumi command
        if self.external==0:
            externe = ""
        else:
            externe = "combustion "

        commandContent= "soumi -t "+self.duration+" -m "+self.memory+" -n "+self.nbprocs+" "+appli_keyword+" "+"soumi.cmd"
        # feeback on command soumi
        print "Commande soumi : "+commandContent
        XDR.execute(commandContent)


    
    def retrieveDirectory(self,directory):
        """ This command retrieves directory in current local directory
        """
        # uncompress archive
        XDR.execute("mv "+directory+".tar ./"+ directory +"/")
        os.chdir(directory)        
        XDR.execute("tar   -xvf  "+directory+".tar")
       
        pass
     
    def removeDirectory(self,directory):
        """ This command delete distant directory, if directory is "*", delete the project directory"""
        pass
        
    def getInfos(self):
        """ This method returns informations 
        """
        
        self.infos['platform'] = "UNIX"
        self.infos['parallelCommand'] = "mpirun -np "+XDR.getValue("nbprocs","unix_soumi",self.type+"_plugins","config")+" "
        return self.infos
        
