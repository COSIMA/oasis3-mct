import os,XDR

class cerfacs_formation_local(XDR.Plugin):
    
    def __init__(self,typePlugin):
        XDR.Plugin.__init__(self, typePlugin)
        self.login = self.getPluginParam("login")
        self.nbprocs = self.getPluginParam("nbprocs")
       
        if self.login not in ["form00","form01","form02","form03","form04","form05","form06","form07","form08","form09","form10","form11"] :
	    error("Your login is not of the form 'formXX' with XX btw 01 and 11. Check your config file")
    
    def sendDirectory(self,directory):
        
        if os.path.exists(directory):
            return 1
        else:
            raise Exception("Error : "+directory+" doesn't exist")
        
    def executeDistantCommand(self,command,execDirectory,appli,flags=[]):
         ########
        # INITS
        ########
        command_exe = command
        supported_applis = []
        local_directory = os.getcwd()
        project_name = XDR.getValue("name","project","meta")
        myhome = "/home/"+self.login

        
         #####################
        # SECTION DEPENDING ON APPLICATIONS
        ####################
        supported_applis.append("tool_avsp52") 
        supported_applis.append("avsp52") 
        supported_applis.append("tool_avbp621") 
        supported_applis.append("avbp621") 
        supported_applis.append("tool_avtp2") 
        supported_applis.append("hip_script") 
        supported_applis.append("dummy")
        
        if appli not in supported_applis:
            XDR.error("this application requires the application tag "+ appli+ ", which is not supported by the plugin cerfacs_formation_local.")

        ########
        # AVBP #
        ########
	if appli == "avbp621":
            avbp_home = myhome+"/AVBP_V6.2"
            command_exe ="mpiexec -boot -np 2 "+ avbp_home+"/HOST/FORMATION/AVBPEXEC/AVBP_V6.2.FORMATION > avbp_log.txt" 
	if appli == "tool_avbp621":
            hip_cur_version =  myhome+"/AVBP_V6.2/HOST/FORMATION/TOOLEXEC/hip-1.39.3-PCLINUX"
            avbp_home = myhome+"/AVBP_V6.2"
            avbp_scripts = os.path.join(avbp_home,"TOOLS","BIN","SCRIPTS")
           
            if command.startswith("-c3sm_auto_"):
 
                #command_exe="source "+rolex_tag+" ; "
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
            hip_cur_version =  myhome+"/AVBP_V6.2/HOST/FORMATION/TOOLEXEC/hip-1.39.3-PCLINUX"
            if command.startswith("-c3sm_auto_hip-"):
                command_exe = command.replace("-c3sm_auto_hip-",hip_cur_version)
            elif command == "-c3s_auto_hip_scripttxt-":
                command_exe= hip_cur_version+" < ./script.txt"

        if appli == "hip_script":
            hip_cur_version =  myhome+"/AVBP_V6.2/HOST/FORMATION/TOOLEXEC/hip-1.39.3-PCLINUX"
            command_exe = hip_cur_version + " < " + command

            
        ########
        # AVSP #
        ########
	if appli == "avsp52":
            command_exe = "/usr/bin/python /home/avbp_00/AVSP_FORMATION/QUIET_FORMATION/quiet3.py &> quiet.out"
            #avsp_command = "/home/avbp_00/AVSP_FORMATION/QUIET_FORMATION/AVSP_HOME/HOST/KALI/AVSPEXEC/AVSP_V5.2.KALI"
            from shutil import copy
            copy("/home/avbp_00/AVSP_FORMATION/QUIET_FORMATION/AVSP_HOME/HOST/KALI/AVSPEXEC/AVSP_V5.3.KALI", execDirectory)
            avsp_command = "mpirun -np 2 ./AVSP_V5.3.KALI"
            XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_avsp_exe-",avsp_command)
            anozzle_command = "/home/avbp_00/AVSP_FORMATION/QUIET_FORMATION/AVSP_HOME/HOST/KALI/TOOLEXEC/Anozzle_CORAIL.exe"
            XDR.replace_pattern_in_file(os.path.join(execDirectory,"user_params.py"),"-c3sm_auto_anozzle_exe-",anozzle_command)
        if appli == "tool_avsp52":
            if command == "-c3sm_auto_preproc_mlpf_exec-":
                command_exe = "/home/avbp_00/AVSP_FORMATION/QUIET_FORMATION/AVSP_HOME/HOST/KALI/TOOLEXEC/preproc_mlpf_KALI.exe"
            if command == "-c3sm_auto_avspinitsol_exec-":
                command_exe = "/home/avbp_00/AVSP_FORMATION/QUIET_FORMATION/AVSP_HOME/HOST/KALI/TOOLEXEC/avspinitsol_KALI.exe"
            if command == "-c3sm_auto_zinn_exec-":
                command_exe = "/home/avbp_00/AVSP_FORMATION/QUIET_FORMATION/AVSP_HOME/HOST/KALI/TOOLEXEC/Zinn_nozzle_KALI.exe"

        ########
        # AVTP #
        ########
        if appli == "tool_avtp2":
            hip_cur_version =  "/home/rolex/HIP/1.38.1/hip-1.38.1-KALI"
            avtp_tag =  "/home/cfd1/avtp/avtp_tag"
            avtp_home = "/home/cfd1/avtp/AVTP_V2.X"
            avtp_scripts = os.path.join(avtp_home,"TOOLS","BIN","SCRIPTS")

            if command.startswith("-c3sm_auto"):

                command_exe ="source "+avtp_tag+";"

                if command.startswith("-c3sm_auto_loadmesh-"):
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_mesh.py"),"-c3sm_auto_meshinfo-",os.path.join(avtp_scripts,"meshinfo "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_mesh.py"),"-c3sm_auto_get_patch_surf-",os.path.join(avtp_scripts,"get_patch_surf "))
                    command_exe = "/usr/bin/python ./script_mesh.py"

                if command.startswith("-c3sm_auto_track-"):
                    command_exe = command.replace("-c3sm_auto_track-",os.path.join(avtp_scripts,"track "))

                if command == "-c3sm_auto_prepare_boundary-":
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_boundaries.py"),"-c3sm_auto_hip-",hip_cur_version)
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_boundaries.py"),"-c3sm_auto_makesolutbound-",os.path.join(avtp_scripts,"makesolutbound "))
                    command_exe = "/usr/bin/python ./script_boundaries.py"

                if command == "-c3sm_auto_initsol-":
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_solinfo-",os.path.join(avtp_scripts,"solinfo "))
                    XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_init.py"),"-c3sm_auto_makesolution-",os.path.join(avtp_scripts,"makesolution "))
                    command_exe = "/usr/bin/python ./script_init.py"

	
        os.chdir(execDirectory)
        XDR.execute(command_exe)
        os.chdir(local_directory)
        
    def retrieveDirectory(self,directory):
        # Do nothing as we are local
        pass
    
    def removeDirectory(self,directory):
        pass
    
    def getInfos(self):
        self.infos['platform']="UNIX"
        self.infos['parallelCommand'] = "mpirun -np "+XDR.getValue("nbprocs","local_unix",self.type+"_plugins","config")+" "
        return self.infos    
