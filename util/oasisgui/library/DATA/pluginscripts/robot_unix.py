import os,XDR,time

class robot_unix(XDR.Plugin):
    
    def __init__(self,typePlugin):
        XDR.Plugin.__init__(self, typePlugin)
    
    
    def sendDirectory(self,directory):
        
        if os.path.exists(directory):
            return 1
        else:
            raise Exception("Error : "+directory+" doesn't exist")
        
    def executeDistantCommand(self,command,exec_directory,appli,flags=[]):
        print "Plugin : Using Robot Unix : "+command+" ("+appli+")"
       
    
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
        supported_applis.append("tool_avsp52") 
        supported_applis.append("hip_current") 
        supported_applis.append("dummy")
        
        if appli not in supported_applis:
             XDR.error("this application requires the application tag "+ appli+ ", which is not supported by the plugin library\DATA\pluginscripts\robot_unix.")
            
  
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
            toolHIPhome = "/appl/APPLI_SNECMA/HIP/simu/1.36.0"
            if command == "-c3s_auto_hip_scripttxt-":
                command_exe= toolHIPhome + "/hip < ./script.txt"
            
            
        if command_exe.startswith("-c3sm_auto_"):
            XDR.error("command was not understood: "+command_exe )
        
       
        # get the user name
        username = os.getenv('USERNAME')
        
        # get the original path
        oldDir = os.path.abspath(os.getcwd())
        
        # build the path to the tool
        fullexec_directory = os.path.join(oldDir,exec_directory)
        
        # convert into UNIX path
        pathlist = fullexec_directory.split("\\")
        newpath = []
        for item in pathlist :
            if item == "windata" :
                item = "data"
            if item.endswith("_Recherches") :
                register = item.split("_")[0] 
                item = "Recherches"
                
            if item != "" :
                newpath.append(item)
            
        newpath = "/"+"/".join(newpath)
        fullexec_directory_unix = newpath
        
        
        # open the user's robot input file, and set the operating directory
        user_robot_file=os.path.join("\\\\windata\\"+register+"_Recherches\\METHODES_LES\\_Chambre\\TAF\\ROBOT", username, "robot.in")
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
        
        # come back to old directory
        os.chdir(oldDir)
        
    def retrieveDirectory(self,directory):
        # Do nothing as we are local
        pass
    
    def removeDirectory(self,directory):
        pass
