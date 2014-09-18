import os,XDR
import shutil
import re

class turbomeca_btmclx2_local(XDR.Plugin):
    
    
    # 
    def __init__(self,typePlugin):
	"""This method transfer the informations form the XML file to the present code
	it should essentially involve lines of type
	self.myparam = self.getPluginParam("mypram_in_the_xml")"""
        XDR.Plugin.__init__(self, typePlugin)
    	self.nbprocs = self.getPluginParam("nbprocs")
    
    @XDR.exec_command 
    @XDR.supported_applications(['tool_avsp52','tool_avbp621','yales2','hip_current','hip_script'])
    def executeDistantCommand(self,command,execDirectory,appli,flags=[]):
	"""This method will interpret commands sent from
	python scripts associated to a C3Sm application
	
	Code-specific management must be done here, and only here.
	"""
        
##############################################################################################################################        
##############################################################################################################################
##############################################################################################################################
        ########
        # AVBP #
        ########
        if appli == "tool_avbp621":
            hip_cur_version = "/Avbp/versions/bin/hip"
            avbp_home = "/Avbp/versions/AVBP_V6.2"
            avbp_scripts = os.path.join(avbp_home,"TOOLS","BIN","SCRIPTS")

            if command.startswith("-c3sm_auto_"):

                command_exe=""


                if command.startswith("-c3sm_auto_loadmesh-"):
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_mesh.py"),"-c3sm_auto_meshinfo-",os.path.join(avbp_scripts,"meshinfo "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_mesh.py"),"-c3sm_auto_get_patch_surf-",os.path.join(avbp_scripts,"get_patch_surf "))
                    command_exe+= "/usr/bin/python ./script_mesh.py"

                if command.startswith("-c3sm_auto_track-"):
                    command_exe += command.replace("-c3sm_auto_track-",os.path.join(avbp_scripts,"track "))

                if command.startswith("-c3sm_auto_tadia-"):
                    command_exe += command.replace("-c3sm_auto_tadia-",os.path.join(avbp_scripts,"tadia "))

                if command.startswith("-c3sm_auto_flame_param-"):
                    command_exe += command.replace("-c3sm_auto_flame_param-",os.path.join(avbp_scripts,"flame_param "))

                if command == "-c3sm_auto_prepare_boundary-":
                    print "my friend"
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_boundaries.py"),"-c3sm_auto_hip-",hip_cur_version)
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_boundaries.py"),"-c3sm_auto_makesolutbound-",os.path.join(avbp_scripts,"makesolutbound "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_boundaries.py"),"-c3sm_auto_genprofile-",os.path.join(avbp_scripts,"genprofile2 "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_boundaries.py"),"-c3sm_auto_makeinject-",os.path.join(avbp_scripts,"makeinject "))
                    command_exe+= "/usr/bin/python ./script_boundaries.py"

                if command == "-c3sm_auto_initsol-":
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_solinfo-",os.path.join(avbp_scripts,"solinfo "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_add_vortex-",os.path.join(avbp_scripts,"addvortex "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_makesolution-",os.path.join(avbp_scripts,"makesolution "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_add_spec-",os.path.join(avbp_scripts,"add_spec "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_init_spec-",os.path.join(avbp_scripts,"init_spec "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_gas_out-",os.path.join(avbp_scripts,"gas_out "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_ign_sphere-",os.path.join(avbp_scripts,"ignition_sphere "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_add_tpf-",os.path.join(avbp_scripts,"add_tpf "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_add_fic-",os.path.join(avbp_scripts,"add_fic "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_check_perio-",os.path.join(avbp_scripts,"check_perio "))
                    command_exe+= "/usr/bin/python ./script_init.py"

                if command.startswith("-c3sm_auto_xtools-"):
                    command_exe=command.replace("-c3sm_auto_xtools-","")
 
        #######
        # HIP #
        #######
        if appli == "hip_current":
            hip_cur_version =  "/Avbp/versions/bin/hip"
            if command.startswith("-c3sm_auto_hip-"):
                command_exe = command.replace("-c3sm_auto_hip-",hip_cur_version)
            elif command == "-c3s_auto_hip_scripttxt-":
                command_exe= hip_cur_version+" < ./script.txt"

        if appli == "hip_script":
            command_exe = "/Avbp/versions/bin/hip < " + command
    
        ########
        # AVSP #
        ########
        if appli == "tool_avsp52":
            avsp_home = "/Avbp/versions/ARCHIVE_QUIET/QUIET/QUIET_TM/AVSP_HOME_2"
            tools_rep ="/HOST/BTMCLX2/TOOLEXEC/"
            if command == "-c3sm_auto_preproc_mlpf_exec-":
                command_exe = avsp_home+tools_rep+"preproc_mlpf_BTMCLX2.exe"
            if command == "-c3sm_auto_avspinitsol_exec-":
                command_exe = avsp_home+tools_rep+"avspinitsol_BTMCLX2.exe"
            if command == "-c3sm_auto_avbp2avsp_exec-":
                command_exe = avsp_home+tools_rep+"avbp2avsp_BTMCLX2.exe"
            if command == "-c3sm_auto_zinn_exec-":
                command_exe = avsp_home+tools_rep+"Zinn_nozzle_BTMCLX2.exe"

        
	##########
	# YALES2 #
	#########
	if appli == "yales2":
		
		script_path = XDR.getScriptDir()
		
		if command == "-c3sm_YALES2_auto_exe-":
		
			#NO JOB RUNNING ON BTMCLX2 only tools execution
			XDR.error("Job cann't run on BTMCLX2 (code plugin) only tools execution. Change your config.xml code plugin available are for soumi or CCRT")
		
		elif command == "-load_chemtable-":
						
			table_path=XDR.getValue("table",XDR.getValue("eos","chemistry"),"eos","chemistry")
			table_path=os.path.split(table_path)[0]+'/'
			# Replace pattern
			XDR.replace_pattern_in_file(os.path.join(execDirectory,"batch_chemtable"),"-table_path-",table_path)						
			# Prepare command
			batch_path=os.path.join(execDirectory,'batch_chemtable')
			XDR.execute("chmod +x "+batch_path)			
			command_exe = "/bin/bash ./batch_chemtable"
							
		else:
			XDR.error("Command unknown")
			

##############################################################################################################################        
##############################################################################################################################
##############################################################################################################################

        os.chdir(execDirectory)   
        XDR.execute(command_exe)     
        os.chdir(self.local_directory)
        
    def retrieveDirectory(self,directory):
	"""This method will download some directories on the ressource
	  or do nothing if its local"""
        pass
    
    def removeDirectory(self,directory):
        """This method will remove some directories on the ressource
	  Rarely used, kept for backward compatibility"""
        pass
