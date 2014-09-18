import os,XDR,shutil,sys

import pckgfile
if sys.platform.startswith('linux'):
    RUNAPPLIDIR="/gpfs/appl/APPLI_SNECMA/RUNAPPLI"
    PYTHONEXE="/gpfs/appl/APPLI_SNECMA/PYTHON/Python-2.5.4/python"
else:
    RUNAPPLIDIR="//winappl/calcul2000/Runappli"
    #utilisation du python non-graphique avec le "w"
    PYTHONEXE="//winappl/calcul2000/Outils/Python/Oper/v2.5.1/pythonw.exe"



class snecma_runappli(XDR.Plugin):  
    def __init__(self,typePlugin):
        """ This method built a new instance of plugin.
        First it constructs the plugin thanks to mother class : XDR.Plugin"""
        
        # Il ne faut surtout pas supprimer cette ligne
        XDR.Plugin.__init__(self, typePlugin)
        
        # Ici, je stocke les donnees du config que je pourrais avoir besoin plus tard 
        self.version_ra = self.getPluginParam("version_ra")
        self.platform = self.getPluginParam("platform")
        self.nbprocs = self.getPluginParam("nbprocs")
        self.memory = self.getPluginParam("memory")
        self.duration = self.getPluginParam("duration")
        self.noclean = self.getPluginParam("noclean")
        self.runapplidir = RUNAPPLIDIR
        self.pythonexe = PYTHONEXE

        
        self.dir2send = []
        # Ici, pour les autres plugins, je verifie que tous mes dossiers existent bien, sinon, je les cree
        if self.platform=="curie":
            self.platformname = "CURIE"
        if self.platform=="airain":
            self.platformname = "AIRAIN"
        if self.platform=="zephyr":
            self.platformname = "ZEPHYR"
        if self.platform=="titane":
            self.platformname = "TITANE"
        self.ra_path = self.version_ra.replace("#","/")



        
    # Applications supportes par Runappli a snecma
    @XDR.supported_applications(['avsp52','yales2_1007','yales2_1008'])



    def executeDistantCommand(self,command,execDirectory,appli,flags=[]):
        """ This command executes command in execDirectory on distant machine.
        Flags can be specified, they depend on the code and the plugin
        """

        print "Running executeDistantCommand "+command+" in "+execDirectory+" for appli"+appli
        ########
        # INITS
        ########
        command_exe = command
        appli_keyword = "none"
        local_directory = os.getcwd()
        
        if self.platform=="zephyr":
# Creating batch file for ZEPHYR
            batchContent =  "#!/bin/bash\n"
            pythonexec = "/appl/APPLI_SNECMA/PYTHON/2.7.3/bin/python"
        else:
# Creating batch file for CEA
            batchContent =  "#!/bin/bash\n"
            batchContent+=  "source /etc/profile.d/modules.sh\n"
            pythonexec = "/usr/bin/python"

#############################################################################                
#############################################################################                
#############################################################################                

        ########
        # AVSP #
        ########
        if appli == "avsp52":
            if  self.platform=="zephyr":
                homeAVSP52="/appl/APPLI_SNECMA/AVSP/test/5.2.0"
                batchContent += "export LD_LIBRARY_PATH=/appl/APPLI_SNECMA/COMPOSANTS_COMMUNS/HDF5/oper/1.8.5-P1/hdf5-1.8.5-patch1-linux-x86_64-shared/lib:$LD_LIBRARY_PATH\n"        
                avsp_command =  "mpirun  -np "+ self.nbprocs+" "+homeAVSP52+"/AVSP.exe"
            else :
                homeAVSP52="/ccc/cont001/home/snecma/snecma/APPLI_SNECMA/AVSP/test/5.2.0"
                batchContent += "module load hdf5/1.8.5\n"
                avsp_command = "ccc_mprun -x -n "+ self.nbprocs+" "+homeAVSP52+"/AVSP.exe"

            if command == "-c3sm_auto_avsp_exe-":
                command_exe = "cp "+homeAVSP52+"/quiet3.py . ; " + pythonexec + " ./quiet3.py &> ./quiet.out"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_avsp_exe-",avsp_command)
                anozzle_command = homeAVSP52+"/Anozzle.exe"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_anozzle_exe-",anozzle_command)
 
	##########
	# YALES2 #
	##########
	if appli in ["yales2_1007","yales2_1008"] :
	    if  self.platform=="zephyr":
		error("Yales2 is not available on Zephyr")
	    else :
		
		if appli == "yales2_1007" :
		    version_yales2 = " "
		if appli == "yales2_1008" :
		    version_yales2 = "v_1.0.0.8"
		    
		
		batchContent +="export YALES2_HOME=/ccc/cont001/home/snecma/snecma/APPLI_SNECMA/YALES2/test/"+self.platformname+"/"+ version_yales2+"\n "
		batchContent +="export YALES2_HOSTTYPE="+self.platform +"\n "
		batchContent +="export PATH=$PATH:$YALES2_HOME/tools \n "  
		batchContent +="export LD_LIBRARY_PATH=/usr/local/hdf5-1.8.5/lib:$LD_LIBRARY_PATH \n "
			
		batchContent +="cd "+execDirectory+"\n "
		
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
		batchContent +="sed -i.bak 's:\\:/:g' \n "
		batchContent += "cd ..\n"
		command_exe = "ccc_mprun -x -n "+self.nbprocs+" ./caseYales2"
                    
#############################################################################                
#############################################################################                
#############################################################################                

        if command_exe.startswith("-c3sm_auto_"):
            XDR.error("command was not understood: "+command_exe )       
                    
        
        batchContent += """cd """+execDirectory+"\n"
        batchContent +=command_exe+"\n"
        batchContent += "cd ..\n"
        
        batchFile = open("batch_c3sm",'w')
        batchFile.write(batchContent)
        batchFile.close()
        self.dir2send.append("batch_c3sm")
        
        # create a zip archive of the directories list
        print "Ok : Repertoires envoyes : "+" ".join(self.dir2send)
        archive=pckgfile.PckgFile(os.path.join(".", "c3sm_archive.tar"), 'w')
        for file in self.dir2send: archive.write(file)
        archive.close()
        
        # Creating runappli script
        os.environ["RA_C3SM_ANALYSE"]=r"0"
        os.environ["RA_C3SM_DATA_IN"]=os.getcwd()
        os.environ["RA_C3SM_FILE"]="c3sm_archive.tar"

        #Cree la ligne de commandes
        cmd = "%(pythonexe)s %(runapplidir)s/%(ra_path)s/run_appli.py "
        cmd += " -a C3SM"
        cmd += " -A C3SM"
        cmd += " -V SEQ"
        cmd += " -p %(platformname)s"
        cmd += " -n %(nbprocs)s"
        cmd += " -c %(duration)s"
        cmd += " -m %(memory)s"
        if self.noclean == "1" :
	    cmd += " -k"
        cmd += " -e null"
        cmd += " -i auto"
        cmd += " -Z tar"
        cmd%=self.__dict__
        print cmd
        # ici on utilise os.system pour avoir les variables d'environnement, contrairement a XDR.execute.
        out=os.system(cmd)
        print out        

        if "myflag" in flags:
            pass
    def retrieveDirectory(self,directory):
        """ This command retrieves directory in current local directory
        """
        pass
     
    def removeDirectory(self,directory):
        """ This command delete distant directory, if directory is "*", delete the project directory"""
        pass
        
