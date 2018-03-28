import os,XDR

from plugin_avbp import plugin_avbp

class cerfacs_neptune_local(XDR.Plugin):
    
    def __init__(self,typePlugin):
        XDR.Plugin.__init__(self, typePlugin)
    
    
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
        
 #####################
        # SECTION DEPENDING ON APPLICATIONS
        ####################
        supported_applis.append("avbp621") 
        supported_applis.append("tool_avsp52") 
        supported_applis.append("tool_avbp621") 
        supported_applis.append("tool_avtp2") 
        supported_applis.append("hip_script")
        supported_applis.append("hip_current")
        supported_applis.append("dummy")
        
        if appli not in supported_applis:
            XDR.error("this application requires the application tag "+ appli+ ", which is not supported by the plugin cerfacs_neptune_local.")

        ########
        # AVBP #
        ########
        if appli == "tool_avbp621":
#               avbp_home = "/home/rolex/AVBP_V6.X/AVBP_D6.2.1"
               avbp_home = "/scratch/cfd/hannebiq/AVBP_D6.2.1"
               hostname = "NEPTUNE"
               hip_cur_version = " /home/cfd/avbp/HIP/1.40.1/hip-1.40.1-NEPTUNE"
               pythonexec = "python"
               avbptool = plugin_avbp(avbp_home,hostname,hip_cur_version,pythonexec,execDirectory)
               command_exe = avbptool.switch_avbp_tools(appli,command)    
               
               # Creating batch file
               batchContent =  """#!/bin/bash
               #PBS -N C3Sm_tools
               #PBS -l walltime=00:30:00
               #PBS -j oe
               #PBS -l select=1:ncpus=1:mpiprocs=1 -l place=scatter
               #PBS -m ae
               #PBS -q debug
               
               cat \$PBS_NODEFILE
               nb_cpu=\$(wc -l < \$PBS_NODEFILE)
               
                       
               """                    
               batchContent +="""export LD_LIBRARY_PATH=/home/cfd/avbp/LIBSUPPORT:$LD_LIBRARY_PATH"""+"\n"
               batchContent+="mpirun "+command_exe+"\n" 
        
        if appli == "avbp621":
               avbp_home = "/home/rolex/AVBP_V6.X/AVBP_D6.2.1"
               command_exe = "/home/rolex/AVBP_V6.X/AVBP_D6.2.1/AVBP_V6.2.1_beta.NEPTUNE > avbp_log.txt" 
		
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
               batchContent +="""export LD_LIBRARY_PATH=/home/cfd/avbp/LIBSUPPORT:$LD_LIBRARY_PATH"""+"\n"
               batchContent+="mpirun "+command_exe+"\n" 
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

            
         ########
        # AVSP #
        ########
        if appli == "tool_avsp52":
            hip_cur_version =  "/home/rolex/HIP/1.40.1/hip-1.40.1-KALI"
            if command == "-c3sm_auto_preproc_mlpf_exec-":
                command_exe = "/home/rolex/QUIET_5.3/AVSP_HOME/HOST/KALI/TOOLEXEC/preproc_mlpf_KALI.exe"
            if command == "-c3sm_auto_avspinitsol_exec-":
                command_exe = "/home/rolex/QUIET_5.3/AVSP_HOME/HOST/KALI/TOOLEXEC/avspinitsol_KALI.exe"
            if command == "-c3sm_auto_zinn_exec-":
                command_exe = "/home/rolex/QUIET_5.3/AVSP_HOME/HOST/KALI/TOOLEXEC/Zinn_nozzle_KALI.exe"
            if command == "-c3sm_auto_add_activflame-":
                global2avsp_cmd = "/home/rolex/QUIET_5.3/AVSP_HOME/HOST/KALI/TOOLEXEC/global2avsp_KALI.exe"
                hipscript_cmd = hip_cur_version + " < script_hip"
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_active_flame.py"),"-c3sm_auto_global2avsp_exec-",global2avsp_cmd)
                XDR.replace_pattern_in_file(os.path.join(execDirectory,"script_active_flame.py"),"-c3sm_auto_hipscript-",hipscript_cmd)
                command_exe = "/usr/bin/python ./script_active_flame.py"
            if command == "-c3sm_auto_avbp2avsp_exec-":
                command_exe = "/home/rolex/QUIET_5.3/AVSP_HOME/HOST/KALI/TOOLEXEC/avbp2avsp_KALI.exe"

        ########
        # AVTP #
        ########
        if appli == "tool_avtp2":
            hip_cur_version =  "/home/rolex/HIP/1.39.3/hip-1.39.3-KALI"
            avtp_tag =  "/home/rolex/rolex_avtp_20"
            avtp_home = "/home/rolex/AVTP/AVTP_V2.X"
            avtp_scripts = os.path.join(avtp_home,"TOOLS","BIN","SCRIPTS")

            if command.startswith("-c3sm_auto"):

                command_exe="source "+avtp_tag+";"

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
        output = XDR.execute(command_exe)
        os.chdir(local_directory)
        return output
        
    def retrieveDirectory(self,directory):
        # Do nothing as we are local
        pass
    
    def removeDirectory(self,directory):
        pass
    
