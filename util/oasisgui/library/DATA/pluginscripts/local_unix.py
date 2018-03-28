import os,XDR

from avbp import plugin_avbp
from avtp import plugin_avtp
from prissma import plugin_prissma


class local_unix(XDR.Plugin):
    
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
        
        
        hostname = "KALI"
        pythonexec= "/usr/bin/python"
        hip_cur_version = "/home/rolex/HIP/1.41.0/hip-1.41.0-"+hostname
        
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

         #####################
        # SECTION DEPENDING ON APPLICATIONS
        ####################
        supported_applis.append("tool_avsp52") 
        supported_applis.append("tool_avbp621") 
        supported_applis.append("tool_avtp2")
        supported_applis.append("tool_prissma")
        supported_applis.append("tool_icritles")
        supported_applis.append("mps_script") 
        supported_applis.append("hip_script")
        supported_applis.append("hip_current")
        supported_applis.append("dummy")
        
        if appli not in supported_applis:
            XDR.error("this application requires the application tag "+ appli+ ", which is not supported by the plugin local_unix.")

        ########
        # AVBP #
        ########
        if appli == "tool_avbp621":
               avbp_home = "/home/rolex/AVBP_V6.X/AVBP_D6.2.1"
               avbptool = plugin_avbp(avbp_home,hostname,hip_cur_version,pythonexec,execDirectory)
               command_exe = avbptool.switch_avbp_tools(appli,command)    
                    

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
            avsp_home =  "/home/rolex/QUIET_5.3/AVSP_HOME"
            # Temporary, bug with axisym duplication in HIP v1.41.0
            hip_cur_version = "/home/rolex/HIP/1.40.1/hip-1.40.1-"+hostname
            avsptool = plugin_avsp(avsp_home,hostname,hip_cur_version,pythonexec,execDirectory)
            command_exe = avsptool.switch_avsp_tools(appli,command)

         ########
        # AVTP #
        ########
        if appli == "tool_avtp2":
            avtp_home = "/home/rolex/AVTP/AVTP_V2.X"
            avtptool = plugin_avtp(avtp_home,hostname,hip_cur_version,pythonexec,execDirectory)
            command_exe = avtptool.switch_avtp_tools(appli,command)           

        ############
        # ICRITLES #
        ############
        if appli == "tool_icritles":
            if command == "-c3sm_auto_icritles-":
                avbp_home = "/home/rolex/AVBP_V6.X/AVBP_D6.2.1"
                command_exe = avbp_home+"/HOST/"+hostname+"/TOOLEXEC/i_crit_les.e_"+hostname
                
        ###########
        # PRISSMA #
        ###########
        if appli == "tool_prissma":
            avbp_home = "/home/rolex/AVBP_V6.X/AVBP_D6.2.1"
            prissma_home = "/home/avtp_wkdir/PRISSMA/trunk/"
            prissmatool = plugin_prissma(prissma_home,avbp_home,hostname,hip_cur_version,pythonexec,execDirectory)
            command_exe = prissmatool.switch_prissma_tools(appli,command)

        ########
        # MPS  #
        ########
        if appli == "mps_script":
            if command.startswith("-c3sm_auto"):
                if command == "-c3sm_auto_mps-" :
                    datafile = os.path.join(local_directory,execDirectory,"aerotherm.xml")
                    execDirectory = "/home/rolex/multiphysics_setup"
                    command_exe = "mpview -aerotherm_xml "+datafile
        
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

        os.chdir(execDirectory)
        output = XDR.execute(command_exe)
        os.chdir(local_directory)
        return output
        
    def retrieveDirectory(self,directory):
        # Do nothing as we are local
        pass
    
    def removeDirectory(self,directory):
        pass
    
