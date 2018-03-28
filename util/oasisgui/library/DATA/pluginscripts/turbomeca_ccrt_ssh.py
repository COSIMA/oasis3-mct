import os,XDR,shutil

MAIN_DIRECTORY_NAME = "C3Sm_projects"

class turbomeca_ccrt_ssh(XDR.Plugin):
    
    def __init__(self,typePlugin):
	 """ This method built a new instance of plugin.
        First it constructs the plugin thanks to mother class : XDR.Plugin"""
	
        XDR.Plugin.__init__(self, typePlugin)
                
        self.login = self.getPluginParam("login") 
        self.machinecea = self.getPluginParam("machinecea") 
        self.nbprocs = self.getPluginParam("nbprocs") 
        self.timelimit =  self.getPluginParam("timelimit") 
        self.distantDirectory = self.getPluginParam("distantdir")    
        self.mail = self.getPluginParam("mail")         
        
        if self.machinecea == "curie":
             self.machine = "curie-ccrt.ccc.cea.fr"
             self.platformname = "CURIE"
        if self.machinecea == "airain":
             self.machine = "airain.ccc.cea.fr"
             self.platformname = "AIRAIN"

        sshCommand = """bash -i -c "cd """+self.distantDirectory+"""; if [ ! -d """+MAIN_DIRECTORY_NAME+""" ] ; then mkdir """+MAIN_DIRECTORY_NAME+""" ; fi" """  

        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")

        
        self.distantDirectory = os.path.join(self.distantDirectory,MAIN_DIRECTORY_NAME)
        sshCommand = """bash -c "cd """+self.distantDirectory+"""; if [ ! -d """+XDR.getValue("name","project","meta")+""" ] ; then mkdir """+XDR.getValue("name","project","meta")+""" ; fi" """    
        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        
        self.distantDirectory = os.path.join(self.distantDirectory,XDR.getValue("name","project","meta"))
        
            
    # Applications supportes 
    @XDR.supported_applications(['avsp52','yales2_1007','yales2_1008'])
    def executeDistantCommand(self,command,execDirectory,appli,flags=[]):       
        """ This command executes command in execDirectory on distant machine.
        Flags can be specified, they depend on the code and the plugin
        """

        batchContent =  "#!/bin/bash\n"
        batchContent+=  "source /etc/profile.d/modules.sh\n"
        batchContent+=  "#MSUB -r "+XDR.getValue("name","project","meta")+"\n"
        batchContent+=  "#MSUB -n "+self.nbprocs+"\n"
        batchContent+=  "#MSUB -T "+self.timelimit+"\n"
        batchContent+=  "#MSUB -q standard\n"
        batchContent+=  "#MSUB -o "+XDR.getValue("name","project","meta")+"%I.o\n"
        batchContent+=  "#MSUB -e "+XDR.getValue("name","project","meta")+"%I.e\n"
        batchContent+=  "#MSUB -@ "+self.mail+"\n"
        batchContent+=  "#MSUB -q standard\n"

################################################################################################################################
################################################################################################################################
################################################################################################################################
        ########
        # AVSP #
        ########

        if appli == "avsp52":
            homeAVSP52="/ccc/work/cont001/turbomec/lederlin/codes/avsp/AVSP_V5.2"
            batchContent += "module load hdf5/1.8.5\n"
            avsp_command = "ccc_mprun -x -n "+ self.nbprocs+" "+homeAVSP52+"/AVSP.exe"

            if command == "-c3sm_auto_avsp_exe-":
                command_exe = "cp "+homeAVSP52+"/quiet3.py . ; " + pythonexec + " ./quiet3.py &> ./quiet.out"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_avsp_exe-",avsp_command)
                anozzle_command = homeAVSP52+"/Anozzle.exe"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_anozzle_exe-",anozzle_command)
        #########
        # YALES #
        #########


        if appli in ["yales2_1007","yales2_1008"] :
            if appli == "yales2_1007" :
                version_yales2 = " "
            if appli == "yales2_1008" :
                version_yales2 = "v_1.0.0.8"
			
			batchContent +="# PROLOGUE \n "
			batchContent +="module load hdf5/1.8.5 \n "
			batchContent +="module load parmetis \n "
			batchContent +="module load fftw3 \n "
			batchContent +="module load scotch \n "
			batchContent +="module load petsc/3.2-p7_real \n "
			batchContent +="module load slepc/3.2-p4 \n "
			
			batchContent +="make \n "
			batchContent +="mkdir dump \n "
			batchContent +="mkdir postprocdump \n "
			
			batchContent +="set -x \n "
			command_exe = "ccc_mprun -x -n "+self.nbprocs+" ./caseYales2"



################################################################################################################################
################################################################################################################################
################################################################################################################################

        if command_exe.startswith("-c3sm_auto_"):
            XDR.error("command was not understood: "+command_exe )

        batchContent +=command_exe+"\n"
 
        batchFile = open("batch_C3Sm_CCRT",'w')
        batchFile.write(batchContent)
        batchFile.close()
        
        XDR.execute("scp -r batch_C3Sm_CCRT "+self.login+"@"+self.machine+":"+self.distantDirectory + "/" + execDirectory + "/")

        sshCommand = """cd """+os.path.join(self.distantDirectory,execDirectory)+"""; ccc_msub batch_C3Sm_CCRT"""
        
        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        
        
        
    def retrieveDirectory(self,directory):
        
        local_directory = os.path.abspath(directory)
        dist_directory = os.path.basename(local_directory)
        # Retrieve Directory
        shutil.rmtree(local_directory)
        scpCommand = "scp -r "+self.login+"@"+self.machine+":"+self.distantDirectory+"/"+dist_directory+" "+os.path.dirname(local_directory)
    
        XDR.execute(scpCommand)
        
        
    def removeDirectory(self,directory):
        if directory == "*":
            sshCommand = """bash -c "cd """+os.path.join(self.distantDirectory,"..")+"""; if [ -d """+self.distantDirectory+""" ] ; then rm -r """+self.distantDirectory+""" ; fi"
            """
        else:
            sshCommand = "rm -r "+os.path.join(self.distantDirectory,directory)
            
        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        
