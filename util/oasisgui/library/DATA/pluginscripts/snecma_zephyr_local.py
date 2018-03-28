import os,XDR,shutil

class  snecma_zephyr_local(XDR.Plugin):
    
    def __init__(self,typePlugin):
        XDR.Plugin.__init__(self, typePlugin)
    
    

    @XDR.exec_command
    @XDR.supported_applications(['hip_current','hip_script','tool_avsp52','yales2'])
    def executeDistantCommand(self,command,execDirectory,appli,flags=[]):
##########################################################################################
##########################################################################################
##########################################################################################
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
        # AVSP #
        ########
        if appli == "tool_avsp52":
            homeAVSP52="/appl/APPLI_SNECMA/AVSP/test/5.2.0/"
            if command == "-c3sm_auto_preproc_mlpf_exec-":
                command_exe = homeAVSP52+"preproc_mlpf_ZEPHYR.exe"
            if command == "-c3sm_auto_avspinitsol_exec-":
                command_exe = homeAVSP52+"avspinitsol_ZEPHYR.exe"
            if command == "-c3sm_auto_avbp2avsp_exec-":
                command_exe = homeAVSP52+"avbp2avsp_ZEPHYR.exe"
            if command == "-c3sm_auto_zinn_exec-":
                command_exe = homeAVSP52+"Zinn_nozzle_ZEPHYR.exe"

		########
        # YALES2 #
        ########
				
        if appli == "yales2" :
			if command == "-load_chemtable-":			
				# I use execDirectory to pass chemtable file name
				
				table_file = exec_directory
				exec_directory = 'tmp'
				own_path = os.getcwd()
				tmp_local = os.path.join(own_path,"tmp")
				script_path = XDR.getScriptDir()
				
				# Copy template batch file
				batch_template_path = os.path.join(script_path,"batch_chemtable")
				shutil.copy(batch_template_path,tmp_local)			
				# Replace pattern in local copy of the template
				batch_local_path = os.path.join(tmp_local,"batch_chemtable")
				XDR.replace_pattern_in_file(batch_local_path,"table_path",table_file)					
				# Prepare command
				command_exe = "chmod +x ./batch_chemtable; dos2unix ./batch_chemtable; ./batch_chemtable"        
##########################################################################################
##########################################################################################
##########################################################################################			
        XDR.check_command_exe(command_exe)
	
        os.chdir(execDirectory)
        XDR.execute(command_exe)
        
    def retrieveDirectory(self,directory):
        # Do nothing as we are local
        pass
    
    def removeDirectory(self,directory):
        pass
