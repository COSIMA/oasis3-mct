import os,XDR,time,shutil

RUNAPPLIDIR=r'\\winappl\calcul2000\Runappli\Simu\V04R02'

class snecma_windows2zephyr(XDR.Plugin):
    
    def __init__(self,typePlugin):
        XDR.Plugin.__init__(self, typePlugin)
    
    

    @XDR.supported_applications(["tool_avsp52","hip_current","hip_script","yales2"])        
    def executeDistantCommand(self,command,exec_directory,appli,flags=[]):
        print "Plugin : Using Robot Unix : "+command+" ("+appli+")"
       
    
        ########
        # INITS
        ########
        command_exe = command
        supported_applis = []
        local_directory = os.getcwd()
        project_name = XDR.getValue("name","project","meta")
        

##############################################################################
##############################################################################
##############################################################################

        ########
        # AVSP #
        ########
        if appli == "tool_avsp52":
            toolAVSPhome = "/appl/APPLI_SNECMA/AVSP/test/5.2/AVSP_HOME/HOST/ZEPHYR/TOOLEXEC"
            if command == "-c3sm_auto_avbp2avsp_exec-":
                command_exe = toolAVSPhome + "/avbp2avsp_ZEPHYR.exe"
            if command == "-c3sm_auto_avspinitsol_exec-":
                command_exe = toolAVSPhome + "/avspinitsol_ZEPHYR.exe"
            if command == "-c3sm_auto_preproc_mlpf_exec-":
                command_exe = toolAVSPhome + "/preproc_mlpf_ZEPHYR.exe"
            if command == "-c3sm_auto_zinn_exec-":
                command_exe = toolAVSPhome + "/Zinn_nozzle_ZEPHYR.exe"
           
            
     
		#######
        # HIP #
        #######
        if appli == "hip_current":
            homehip="/appl/APPLI_SNECMA/HIP/test/"
            hip_cur_version =  homehip + "1.39.3" + "/hip.exe"
            if command.startswith("-c3sm_auto_hip-"):
                command_exe = command.replace("-c3sm_auto_hip-",hip_cur_version)
            elif command == "-c3s_auto_hip_scripttxt-":
                command_exe= hip_cur_version+" < ./script.txt"

        if appli == "hip_script":
            homehip="/appl/APPLI_SNECMA/HIP/test/"
            hip_cur_version =  homehip + "1.39.3" + "/hip.exe"
            command_exe = hip_cur_version+ " < " + command

	########
        # YALES2 #
        ########
				
        if appli == "yales2" :
            if command == "-load_chemtable-":
                 temp_path = exec_directory
                 table_path = '/data/Recherches/METHODES_LES/_Chambre/TAF/YALES2/TABLES_CHIMIQUES/'
                 XDR.replace_pattern_in_file(os.path.join(temp_path,"batch_chemtable"),"-table_path-",table_path)
		#####
            	# Prepare command
            	 command_exe = "chmod +x ./batch_chemtable; dos2unix ./batch_chemtable; ./batch_chemtable"    

				       
##############################################################################
##############################################################################
##############################################################################
            
        if command_exe.startswith("-c3sm_auto_"):
            XDR.error("command was not understood: "+command_exe )
        
       
        # get the user name
        username = os.getenv('USERNAME')
        
        # get the original path
        oldDir = os.path.abspath(os.getcwd())
        
        # build the path to the tool
        fullexec_directory = os.path.join(oldDir,exec_directory)
       

        # convert into UNIX path
        fullexec_directory_unix =  self.convert_unc_filename(fullexec_directory)
        
        # open the user's robot input file, and set the operating directory
        user_robot_file = os.path.join(r"\\windata\128_Recherches\METHODES_LES\_Chambre\TAF\C3SM", username, "robot.in")
        inputfile = open(user_robot_file, 'w')
        inputfile.writelines(fullexec_directory_unix)
        inputfile.close()
        
        
        os.chdir(exec_directory)
        print "Robot archive created in "+fullexec_directory
        
        # create the batch
        batchname = os.path.join(fullexec_directory,"batch_c3sm.todo")
        batchname_done = os.path.join(fullexec_directory,"batch_c3sm.done")
        batchname_run = os.path.join(fullexec_directory,"batch_c3sm.run")
        batchname_fail = os.path.join(fullexec_directory,"batch_c3sm.fail")
        
        
        batchContent =  "#!/bin/sh \n"
        batchContent+=command_exe
        batchFile = open(batchname,'w')
        batchFile.write(batchContent)
        batchFile.close()
        
        # ONCE THE BATCH is created, the robot should work on it withing 5 sec
        status = "waiting"
        # check is job is processed 
        max_delay = 15
        delay = 0
        while status=="waiting":
            time.sleep(0.01)
            print "Waiting for the robot :"+batchname_run
            if os.path.exists(batchname_run):
                status="running"
            if os.path.exists(batchname_done):
                status="done"
            print "First loop"
            
            time.sleep(2)
            delay += 2
            if delay >= max_delay :
                print "Warning : "+str(max_delay)+" sec  without answer from the robot \n Please, check if your robot is up and running on Zephyr"
                status="no_robot"
                
        
        
        # check if job is done, max 30 min
        max_delay = 1800
        while status=="running" :
            time.sleep(0.01)
            print "Waiting for the end of the run :"
            if os.path.exists(batchname_done):
                status="done"
            if os.path.exists(batchname_fail):
                status="fail"
                error("Robot tried to execute the tool and failed...")
            print "Second loop"    
            time.sleep(2)
            delay += 2

            if delay >= max_delay :
                print "Warning : "+str(max_delay)+" sec  without answer from the robot \n Run beyond the time limit of robot"
                status="too_long"

        print "Final status : "+status
        print "Final delay : "+str(delay) +" sec."
        

        # clean the robot input file to stop him from searching 
        inputfile = open(user_robot_file, 'w')
        inputfile.close()
        try:
		    os.remove(batchname_done)
        except Exception: 
		    pass
		
        # come back to old directory
        os.chdir(oldDir)
        
    def retrieveDirectory(self,directory):
        # Do nothing as we are local
        pass
    
    def removeDirectory(self,directory):
        pass
     
    def convert_unc_filename(self,filepath):
        import sys,os

        if os.path.isdir(RUNAPPLIDIR):
            p=os.path.join(RUNAPPLIDIR,'PyModules')
            if not p in sys.path: sys.path.append(p)
            import Filesystem
            Filesystem.FILE=os.path.join(RUNAPPLIDIR,'Env','UNC.txt')
            unix_filename = Filesystem.convert_unc_filename(filepath,strict=1)
            if not unix_filename:
                XDR.error("Le chemin '%s' n'existe pas sous unix.\nVerifier que votre projet est construit sur \\\\windata"%filepath)
        else:
            XDR.error("Le repertoire '%s' n'existe plus.\nContacter le garant pour qu'il mette a jour la ligne 3 du plugin %%C3SM_HOME%%\\library\\DATA\\pluginscripts\\snecma_windows2zephyr.py"%RUNAPPLIDIR)
        return unix_filename
     
    def convert_nfs_filename(self,filepath):
        import sys,os

        if os.path.isdir(RUNAPPLIDIR):
            p=os.path.join(RUNAPPLIDIR,'PyModules')
            if not p in sys.path: sys.path.append(p)
            import Filesystem
            Filesystem.FILE=os.path.join(RUNAPPLIDIR,'Env','UNC.txt')
            windows_filename = Filesystem.convert_nfs_filename(filepath)
            if not windows_filename:
                XDR.error("Le chemin '%s' n'existe pas sous unix.\nVerifier que votre projet est construit sur \\\\windata"%filepath)
        else:
            XDR.error("Le repertoire '%s' n'existe plus.\nContacter le garant pour qu'il mette a jour la ligne 3 du plugin %%C3SM_HOME%%\\library\\DATA\\pluginscripts\\snecma_windows2zephyr.py"%RUNAPPLIDIR)
        return windows_filename
     
