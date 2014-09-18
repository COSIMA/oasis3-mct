import os,XDR

class no_plugin(XDR.Plugin):
    
    def __init__(self,typePlugin):
        XDR.Plugin.__init__(self, typePlugin)
    
    
    def sendDirectory(self,directory):
        print "sendDirectory with firectory  :",directory
        
    def executeDistantCommand(self,command,exec_directory,appli,flags=[]):
        print "executeDistantCommand with arguments :"
        print "command : ",command
        print "exec_directory : ",exec_directory
        print "flags : ",flags
        print "Warning : executeDistantCommand no_plugin selected, nothing gonna be done here..."
        
    def retrieveDirectory(self,directory):
        print "Warning : retrieveDirectory no_plugin selected, nothing gonna be done here..."
        pass
    
    def removeDirectory(self,directory):
        print "Warning : removeDirectory no_plugin selected, nothing gonna be done here..."
        pass
    
