import os,XDR,shutil


DISTANT_UNIX_DIRECTORY = "C3Sm_projects"

class coria_anymachine_ssh(XDR.Plugin):
    def __init__(self,typePlugin):

		# Il ne faut surtout pas supprimer cette ligne
		XDR.Plugin.__init__(self, typePlugin)
		
		self.login = self.getPluginParam("login")
		self.machine = self.getPluginParam("machine")
		self.nbprocs = self.getPluginParam("nbprocs")        
		self.distantDirectory = self.getPluginParam("directory")
		self.project_name = XDR.getValue("name","project","meta")
		
		self.distantDirectory = os.path.join(self.distantDirectory,DISTANT_UNIX_DIRECTORY, self.project_name)         
		
		# create the DISTANT_UNIX_DIRECTORY on the distant machine
		XDR.ssh_prepare_directory(self.machine, self.login, self.distantDirectory)
        
            
    def sendDirectory(self,directory):

        local_directory = os.path.abspath(directory)
        
        if os.path.exists(local_directory):
            print "Plugin : distant_unix sending directory "+local_directory +" at "+  self.distantDirectory
            
        else:
            raise Exception("Error : "+directory+" doesn't exist")
        
        XDR.execute("scp -r "+local_directory+" "+self.login+"@"+self.machine+":"+self.distantDirectory)
        
        
        
    def executeDistantCommand(self,command,execDirectory,appli,flags=[]):       
		  
		print "Plugin : Running executeDistantCommand "+command+" in "+execDirectory+"("+appli+")"
		
		dist_path = os.path.join(self.distantDirectory,execDirectory)
		script_path = XDR.getScriptDir()
				
		if command == "-c3sm_YALES2_auto_exe-":
		
			# Copy template batch file
			batch_template_path = os.path.join(script_path,"batch_distant")
			shutil.copy(batch_template_path,execDirectory)			
			# Replace pattern in local copy of the template
			batch_local_path = os.path.join(execDirectory,"batch_distant")
			XDR.replace_pattern_in_file(batch_local_path,"nbprocs",self.nbprocs)						
			# Send batch file      		
			XDR.execute("scp -r "+batch_local_path+" "+self.login+"@"+self.machine+":"+dist_path)			
			# Prepare command		        
			sshCommand = "cd "+dist_path+"; chmod +x ./batch_distant ; /bin/bash -i ./batch_distant"
		
		
		elif command == "-load_chemtable-":
						
			table_path_local=XDR.getValue("table",XDR.getValue("eos","chemistry"),"eos","chemistry")
			dist_path=os.path.join(self.distantDirectory,execDirectory)+'/'
			# Replace pattern + Send batch
			XDR.replace_pattern_in_file(os.path.join(execDirectory,"batch_chemtable"),"-table_path-",dist_path)	
			XDR.execute("scp -r "+execDirectory+" "+self.login+"@"+self.machine+":"+dist_path)	
			# Send table      		
			XDR.execute("scp -r "+table_path_local+" "+self.login+"@"+self.machine+":"+dist_path)					
			# Prepare command
			sshCommand = "cd "+dist_path+"; chmod +x ./batch_chemtable ; /bin/bash -i ./batch_chemtable"
			
		else:
			XDR.error("Command unknown")
		
		
		# Execute final command
		
		XDR.execute("ssh "+self.login+"@"+self.machine+" "+sshCommand)	
		
		
    def retrieveDirectory(self,directory):
    
		distant_directory = os.path.join(self.distantDirectory,directory)
		own_path = os.getcwd()
		XDR.execute("scp -r "+self.login+"@"+self.machine+":"+distant_directory+" "+own_path)
		
		
    def removeDirectory(self,directory):
    
    	distant_directory = os.path.join(self.distantDirectory,directory)
    	XDR.execute("ssh "+self.login+"@"+self.machine+" if [ -d "+distant_directory+" ]; then rm -rf "+distant_directory+" ; else echo Directory missing ; fi" )
        
    
    
    def getInfos(self):
    	self.infos['Name']= "coria_anymachine_ssh"
        self.infos['platform']="UNIX"
        self.infos['parallelCommand'] = "mpirun -np "+self.nbprocs
        return self.infos