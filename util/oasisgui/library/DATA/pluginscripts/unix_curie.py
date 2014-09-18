import os,XDR,shutil

MAIN_DIRECTORY_NAME = "C3Sm_projects"

class unix_curie(XDR.Plugin):
    
    def __init__(self,typePlugin):
        XDR.Plugin.__init__(self, typePlugin)
        
        print "Using curie plugin"
        try :
            self.login = XDR.getValue("login","unix_curie",self.type+"_plugins","config")
            self.machinecea = XDR.getValue("machinecea","unix_curie",self.type+"_plugins","config")
            self.nbprocs = XDR.getValue("nbprocs","unix_curie",self.type+"_plugins","config")
            self.timelimit = XDR.getValue("timelimit","unix_curie",self.type+"_plugins","config")
            self.distantDirectory = XDR.getValue("distantdir","unix_curie",self.type+"_plugins","config")
        except XDRnoNodeException:
            XDR.error("Some values are missing in the configuration of plugin unix_curie")
        
        if self.machinecea == "curie":
             self.machine = "curie-ccrt.ccc.cea.fr"
        if self.machinecea == "airain":
             self.machine = "airain.ccc.cea.fr"

        sshCommand = """bash -c "cd """+self.distantDirectory+"""; if [ ! -d """+MAIN_DIRECTORY_NAME+""" ] ; then mkdir """+MAIN_DIRECTORY_NAME+""" ; fi" """    
        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        
        self.distantDirectory = os.path.join(self.distantDirectory,MAIN_DIRECTORY_NAME)
        sshCommand = """bash -c "cd """+self.distantDirectory+"""; if [ ! -d """+XDR.getValue("name","project","meta")+""" ] ; then mkdir """+XDR.getValue("name","project","meta")+""" ; fi" """    
        XDR.execute("ssh "+self.login+"@"+self.machine+" '"+sshCommand+"'")
        
        self.distantDirectory = os.path.join(self.distantDirectory,XDR.getValue("name","project","meta"))
    
    def sendDirectory(self,directory):

        
        local_directory = os.path.abspath(directory)
        directory_basename = os.path.basename(local_directory)
        
        if os.path.exists(local_directory):
            pass
        else:
            raise Exception("Error : "+directory+" doesn't exist")
      
        print "scp -r "+local_directory+" "+self.login+"@"+self.machine+":"+self.distantDirectory + "/"
        XDR.execute("scp -r "+local_directory+" "+self.login+"@"+self.machine+":"+self.distantDirectory + "/")
        print "done"
        
    def executeDistantCommand(self,command,execDirectory,flags=[]):       
        
        # Creating batch file
        batchContent =  """#!/bin/bash
#MSUB -r """+XDR.getValue("name","project","meta")+"""
#MSUB -n """+self.nbprocs+"""
#MSUB -T """+self.timelimit+"""
#MSUB -q standard
#MSUB -o %I.o
#MSUB -e %I.e

source /etc/profile.d/modules.sh

"""
        if "mpi" in flags:
            batchContent +=""
        
        if ("hdf185" in flags) or ("hdf5" in flags):
            batchContent += "module load hdf5/1.8.5\n"
        
        batchContent += """cd """+os.path.join(self.distantDirectory,execDirectory)+"\n"
        
        if "nompirun" in flags:
            batchContent+=command+"\n"
        else:
            batchContent+="mpirun "+command+"\n"
        
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
        
