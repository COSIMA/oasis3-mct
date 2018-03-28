#!/usr/bin/env python
# -*- coding: utf-8 -*-
 # __   _______  _____              
 # \ \ / /  __ \|  __ \             
 #  \ V /| |  | | |__) |_ __  _   _ 
 #   > < | |  | |  _  /| '_ \| | | |
 #  / . \| |__| | | \ \| |_) | |_| |
 # /_/ \_\_____/|_|  \_\ .__/ \__, |
 #                     | |     __/ |
 #                     |_|    |___/ 

"""

XDR.py
------

Created  by A. Dauptain and G Frichet

Main generic tools for C3Sm interface
Requires version 2.6.6 or later of python 2
Does not run with python 3



"""
import sys
if ((sys.hexversion < 0x020606F0) or
    (sys.hexversion > 0x030000A0)):
    raise Exception(
            "Must be run with python version at least 2.6.6, and not python 3\n"
            "Your version is %i.%i.%i" % sys.version_info[:3])

from xml.dom.minidom import *
import xml.parsers.expat
import time, os, subprocess, re, string,shutil
import filecmp
import fileinput
import imp
import codecs
from itertools import izip
import shlex

import glob


# Warnig cannot be importedblike this :
#if we use xdrpy on distant locations, SAV will be missing
#import SAV




"""
This library intends to provide methods to access and modify XML datasets read and written
by C3Sm.
Moreover, this library provides methods to deal with ascii files that might be read and written by external programs.

There are two parts in this library :
The first one is composed of high-level procedures.
The second one is a set of classes used by the high-level procedures.
Normal developers does not have to use them, even if they can !"""

 # _    _ _       _       _                _ 
 #| |  | (_)     | |     | |              | |
 #| |__| |_  __ _| |__   | | _____   _____| |
 #|  __  | |/ _` | '_ \  | |/ _ \ \ / / _ \ |
 #| |  | | | (_| | | | | | |  __/\ V /  __/ |
 #|_|  |_|_|\__, |_| |_| |_|\___| \_/ \___|_|
 #           __/ |                           
 #          |___/                            

def init():
    """
    This procedure initializes the XDR environment.
    It must be called at the beginning of every script using XDR.
    
    ==== Dev =====
    This procedures creates an interface "dsin" for reading XML file "dataset.xml" produced by C3Sm
    It creates also a copy of this dataset "dsout", modifiable, to be read by C3Sm, "out_dataset.xml"
    It also stores useful paths (current directory at launch of the script (not to lose C3Sm) and script directory)
    """   
    
    global dsin
    global dsout
    global startingDir
    global scriptDir
    global verbose
    global OrigAppName
    global projectName
    
    global timestamp
    global COMMON_DIRECTORY,PREFIX,DIGIT_NUMBER

    
    verbose = 0
    startingDir=os.getcwd()
    scriptDir=os.path.dirname(os.path.abspath(sys.modules['__main__'].__file__))

    dsin=Dataset("dataset.xml")
    dsout=Dataset("dataset.xml")
    os.remove("dataset.xml")
    
    OrigAppName = dsout.getValue("name","solver","meta")
    projectName = dsout.getValue("name","project","meta")
    
    
    
    print "Local  use of XDRpy for project ",projectName, "(",OrigAppName,")"
    
    
    print  dsout.getValue("pluginsPath","engine","meta")
    #
    
    PREFIX = "RUN_"
    DIGIT_NUMBER = 3
    COMMON_DIRECTORY = "COMMON"
    
    if OrigAppName == "none" :
        print "Warning : XDRpy unable to find the origin application"
    else :
        print "XDRpy running for application : ",OrigAppName
        COMMON_DIRECTORY = os.path.join(COMMON_DIRECTORY,OrigAppName.upper())
    
    
    
    
    timestamp = time.time()
    
    return dsin,dsout

def finish(a=0):
    """
    This procedure closes the XDR environment.
    It must be called at the end of every script using XDR.
    
    ==== Dev ====
    This procedure saves the output XML file and changes the directory to the initial one
    """

    oldDir = os.getcwd()
    os.chdir(startingDir)
    if a != 0 :
        a.save2file("out_dataset.xml")
    else :
        try:
            dsout.save2file("out_dataset.xml")
            
        except NameError:
            pass

def progress(percent,message):
    """ This function lets c3sm know progresse of the current script
    percent is the percentage of work done (0 < percent < 100)
    message is a little status written by the progressbar, it shouldn't be too long
    """
    global timestamp
    
    time.sleep(0.1)
    newtime = time.time()

    print "%"+str(int(percent))+"%"+str(message)
    print '{0:2f}'.format(newtime-timestamp-0.1)+"s \n"+ str(int(percent))+"% "+str(message) 
    sys.stdout.flush()
    timestamp=newtime
    
    
def getDsin():
    """ This procedure returns the Dataset object representing the input xml file """
    try:
        return dsin
    except NameError:
        error("XDRError : XDR library must be initialized")
        
        
def getDsout():
    """ This procedure returns the Dataset object representing the current dataset """
    try:
        return dsout
    except NameError:
        error("XDRError : XDR library must be initialized")
    

def tryGetValue(default, key, *path):
    """
    Search for the node "nodeName" with elements of "path" in its path and return its value
    If node doesn't exist, catch XDRnoNodeException end return the default value
    
    It would be cleaner to use "path" as a list, instead of "*path", but this would change all the geValue calls :(
        To be evaluated later
    """
    return dsout.tryGetValue(default, key, *path)


def getValue(key,*path):
    """
    This procedure returns the value of a specified tag in the dataset.
    If more than one node have the same name, one or more parts of the path have to be specified.
    Ex :
    getValue("myParam")
    or
    getValue("myParam","parent","parent of parent")
    
    It would be cleaner to use "path" as a list, instead of "*path", but this would change all the geValue calls :(
        To be evaluated later
    """
    return dsout.getValue(key,*path)
    

def getListValue(key,*path):
    """
    This procedure gets the asked value from the dataset and returns it as a list
    """
    return dsout.getListValue(key,*path)


def getIntegerValue(key,*path):
    """
    This procedure returns an integer corresponding to the value of a specified tag in the dataset. 
    If more than one node have the same name, one or more parts of the path have to be specified.
    """
    return int(dsout.getValue(key,*path))


def getFloatValue(key,*path):
    """
    This procedure returns a float corresponding to the value of a specified tag in the dataset. 
    If more than one node have the same name, one or more parts of the path have to be specified.
    """
    return float(dsout.getValue(key,*path))


def getChildrenName(nodeName,*path):
    """
    This procedure returns a list of the name of all the children of the node named "nodeName".
    If more than one node has the same name, parts of its path can be specified as for the getValue method
    """
    return dsout.getChildrenName(nodeName,*path)


def setValue(value,nodeName,*path):
    """
    This procedure modifies the node specify and set its value to "value"
    If more than one node has the same name, parts of its path can be specified as for the getValue method
    """
    dsout.setValue(value,nodeName,*path)
    

def nodeExists(nodeName,*path):
    """
    This procedure checks if the node exists or not. It returns 1 if it exists, 0 if not
    """
    return dsout.nodeExists(nodeName,*path)
    


 #
 # ______                     _       
 #|  ____|                   | |      
 #| |__  __  _____  ___ _   _| |_ ___ 
 #|  __| \ \/ / _ \/ __| | | | __/ _ \
 #| |____ >  <  __/ (__| |_| | ||  __/
 #|______/_/\_\___|\___|\__,_|\__\___|
 #                                    
                                     
def execute(command, always_print_err=False, silent=True):
    """
    This procedure searches for the specified executable in the script directory, if not, it tries to execute the command itself.
    The command is then executed and its output is printed in standard output
    """
    ####### Need some work to handle long run and reading of the output on the fly !
    if (os.path.exists(os.path.join(scriptDir,command))):
        command=os.path.join(scriptDir,command)
    print "command "+ command
    command=shlex.split(command)
    if silent == False :
        print "Executing " + repr(command) + ' in ' + repr(os.getcwd()) + ':\n' + 50*'-' + '\n'
    read_from = None
    if "<" in command :
        read_from = command[-1]
        command = command[:-2]
    p=subprocess.Popen(command,stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if read_from:
        p.stdin.write(open(read_from, "r").read())

    stdout_data = []
    if silent == False :
        print "\nXDRExecute =============StdOut=================\n"
    while True:
        line = p.stdout.readline()
        if not line:
            break
        if silent == False :
            print 'XDRExecute ' + line.rstrip()
        sys.stdout.flush()
        stdout_data.append(line)
    returncode = p.wait()
    stderr_data = p.stderr.read()
    
    if ( always_print_err and not returncode ):
        if silent == False :
            print "\nXDRExecute =============StdErr=================\n"
            print 'XDRExecute ' + "\nXDRExecute ".join(stderr_data.split('\n'))
    # if traite "None" "0" False" "" Comme des retours negatif
    if returncode:
        error("Problem while running command :"+" ".join(command)+"\n=============StdErr=================\n"+stderr_data)
    return "".join(stdout_data)



 #
 #  _____     _     
 # / ____|   | |    
 #| (___  ___| |__  
 # \___ \/ __| '_ \ 
 # ____) \__ \ | | |
 #|_____/|___/_| |_|
 #                  
                           
def ssh_host(host, login):
    """reconstruct full host description"""    
    full_host = host
    if host == None or host == "":
        error("SSH host unspecified")
    
    if login != None and login != "":
        full_host = login + '@' + host

    return full_host

def ssh(host, login, command, options=""):
    """ Executes a command through SSH """
    # TODO: check that we are on a platform were ssh actually exists
    full_host = ssh_host(host, login)

    output= execute("""ssh """ + options + """ """ + full_host + """ '"""+command+"""'""")
    return output

def ssh_prepare_directory(host, login, distant_directory):
    """ Prepare (creates) a directory on a distant server """
    # TODO: check that we are on a platform where ssh actually exists

    if len(distant_directory) == 0:
        error("Please specify a distant directory, for example : /home/toto/")

    if distant_directory[0] != '/' and distant_directory[0] != '~':
        error("Relative path given. Please specify an absolute path")

    print "Making sure distant directory is ready..."
    command = """mkdir -p """ + distant_directory
    ssh(host, login, command)

def ssh_send(host, login, distant_directory, local_directories_list, options=""):
    """ Uploads files to a server using ssh.
        This creates a tar archive, and pipes it through ssh to a 'tar xf' on the distant side.
        In short, the command that we run is:
        tar cf - directory1 directory2 | ssh distant_server "tar xf - -C distant_directory"
        (with some additional safety)
    """
    # TODO: check that we are on a platform where tar, scp, ssh actually exist

    # TODO: Check that local_directories_list is actually a list.
    # People will try with string, and it does bad things with strings.

    full_host = ssh_host(host, login)

    if (len(local_directories_list) == 0):
        print "No directories to be sent"
        return


    print "Sending and extracting archive..."
    
    # Decomposition
    # Bash : passage en bash
    # -c  :(...)
    # tar cvf : compresse
    # local directoryes_list (liste de tous les rep a envoyer)
    # - : vers le pipe
    # | : pipe
    # ssh mylogin@host -
    # \:(...)
    # tarc xv : decompresse
    # - depuis le pipe
    # -C:(...)
    # distant directory : Dans la directory distante
    # \ :(...)
    
    command = """bash -c "tar cvf - """ + (" ".join(local_directories_list)) + """ | ssh """ + full_host + """ \\\"tar xf - -C """ + distant_directory + """\\\" " """
    #print "ssh_send command ",command
    execute(command,always_print_err=True)

def ssh_retrieve(host, login, distant_directory, directory, options=""):
    """ Transfers files from a server using ssh.
        This is the opposite of ssh_send, except it operates on a single directory.
    """
    # TODO: check that we are on a platform where scp actually exists
    full_host = ssh_host(host, login)
    
    
    # M RIVIERE Solution -Deprecated-  ( Il y a pb non resolu dedans)
    
    # Decomposition
    # Bash : passage en bash
    # -c  : interprete la chaine de caractères suivante
    # ssh mylogin@host -
    # \:(...)
    # tar cvf : compresse
    # - : vers le pipe
    # -C:(...)
    # distant directory
    # local directory
    # \ :(...)
    # | : pipe
    # tarc xvf : decompresse
    # - depuis le pipe
    #command = """bash -c "ssh """ + full_host + """ \' tar cvf - -C """ + distant_directory + """ """ + directory + """\\\" | tar xvf -" """
   
    # G DEJEAN Sokution
    # Decomposition
    # Bash : passage en bash
    # -c  : interprete la chaine de caractères suivante
    # ssh mylogin@host
    # '
    # cd distant directory: 
    # tar cvf : compresse
    # - : vers le pipe
    # . : le repretoire courant
    # '
    # | : pipe
    # tar -x vf : decompresse
    # -C local directory : en renommant le repertoire
    # -vf :
    # - :depuis le pipe
    command = """bash -c " ssh """ + full_host + """ 'cd """+ distant_directory + """;  tar cvf - .'| tar -x -C """ +directory+""" -vf - " """
   
    print "ssh_retrieve command ",command
    execute(command,always_print_err=True)


def ssh_cleanup_directory(host, login, distant_directory, directory):
    """ Remove a directory on distant ssh server """
    print "Cleaning up distant files..."
    command = """rm -rf """ + distant_directory + """/""" + directory
    ssh(host, login, command)

def tar(file, filelist):
    """ Create an archive from a list of files """
    # TODO: Windows support ? Use pkgfile.py or something ?
    execute("tar cvf " + file + " " + " ".join(filelist))



def error(txt):
    """
    This procedure stops the script and make sure to say it to C3Sm
    """
    try :
        dsout.setValue("0","scriptSuccess","meta")
    except NameError:
        pass
    print "Error : "+txt
    finish()
    exit()
    

def verbose():
    """ This procedure activates verbosing for the library"""
    global verbose
    verbose = 1
    print "Verbose activated ... ("+str(verbose)+") "
    
    
def getScriptDir():
    """ This accessor returns the value of the script directory for the current solver"""
    global scriptDir
    return scriptDir

def getCommonDir():
    """ This accessor returns the value of the script directory for the current solver"""
    global COMMON_DIRECTORY
    return COMMON_DIRECTORY


 #   
 # _____        _                 _   
 #|  __ \      | |               | |  
 #| |  | | __ _| |_ __ _ ___  ___| |_ 
 #| |  | |/ _` | __/ _` / __|/ _ \ __|
 #| |__| | (_| | || (_| \__ \  __/ |_ 
 #|_____/ \__,_|\__\__,_|___/\___|\__|
 #                                    
                                     
class Dataset:
    """ This class defines a reader for XML files written by C3Sm.
    Specifying a XML file, the XDR interface stores the dataset and 
    implements several methods to access values and structures of the dataset"""
    
    def __init__(self,xmlfile):
        """ XDR constructor : it needs a XML file from C3Sm """
        self._xmlFile=xmlfile
        try :
            self._dom=parse(self._xmlFile)
        except IOError:
            error("XDRError : Problem with file system, check that permissions and free space are ok.\n"+xmlfile+" couldn't be read")
        except xml.parsers.expat.ExpatError , err:
            print err
            XMLfile = open(self._xmlFile,"r").read().split("\n")
            print "--> "+XMLfile[err.lineno+1]
            print " "*(4+err.offset)+"^"
            
            
            error("XDRError : Xml file from c3sm wasn't correct xml, please check that you have enough free space and you have the last version of C3Sm. Otherwise, please send dataset.xml from your project directory to C3Sm team")
        self._root=self._dom.documentElement
        self._currentNode=self._root
        self._outputFile=""
        
    def __repr__(self):
        return self._printNode() 
    
    def _printNode (self, sep=''):
        """ This private method is a recursive method to describe the content of the dataset for debugging purpose"""
        desc=""
        node=self._currentNode
        if node.nodeType == 1:
            desc+= sep+node.nodeName+ ' >> '
            for cle,valeur in node.attributes.items():
                desc+= ' '+valeur+'   '
            desc+="\n"
        for child in node.childNodes:
            self._currentNode=child
            desc+=self._printNode(sep+'   ')
            self._currentNode=node
        return desc
    
    def _getTags(self,tagList):
        node=self._currentNode
        if node.nodeType == 1:
            tagList.append(node.nodeName)
        for child in node.childNodes:
            self._currentNode=child
            tagList = self._getTags(tagList)
            self._currentNode=node
        return tagList
    
    def _getNode(self,key,*path):        
        try:
            return self._searchNode(key,*path)
        except XDRtooManyNodesException, e:
            solverNode = self.getValue("callingAddress","action","meta")
            solverNode= solverNode.split(".")[1]
            return self._searchNode(key,solverNode,*path)

    def _searchNode(self,key,*path):
        """ This private method needs a key to search a node with name=key
        If no node is found, an Exception is raisen
        If one node is found, an Element is returned
        If more than one node is, the most relevant one (according to path) is chosen and returned as an Element
        
        path is a list of keyword"""
        
        if not isinstance(key,basestring) :
            raise XDRillFormed("XDRError : the type of key ", key , "is ", type(key), "instead of a basestring")      
        
        result=self._dom.getElementsByTagName(key)
        if result.length==0:
            raise XDRnoNodeException("XDRError : no node found with key "+str(key)+" and path containing "+str(path))
        else :
            resultToDel=[]
            for i,elt in enumerate(result):
                address=self._getAddress(elt)
                for eltPath in path:
                    if eltPath not in address:
                        resultToDel.append(i)
                        break
            #Now, the result should be one item long, if not there is a problem
            # Raising XDRnoNodeException or XDRtooManyNodesException
            resultToDel.sort(reverse=True)
            for i in resultToDel:
                del result[i]
            
            if result.length == 1:
                return result[0]
            elif result.length == 0:
                raise XDRnoNodeException("XDRError : no node found with key "+str(key)+" and path containing "+str(path))
            else:

                errMsg = "XDRError :"+str(result.length)+" nodes found with key '"+str(key)+"' and path "+str(path)+" :\n"
                for elt in result:
                    errMsg += str(self._getAddress(elt))+"\n"
                raise XDRtooManyNodesException(errMsg)
    
    def _getAddress(self,node):
        """ This private method constructs the address of a node in the DOM"""
        parents=list()
        
        security=100
        while (node.nodeName != '#document'):
            security-=1
            if security==0:
                raise Exception("Infinite loop while searching for all the parents ...")
            
            parents.insert(0,node.nodeName)
            node=node.parentNode
        return parents
    
    def addChild(self,nodeName,value,fatherName,*path):
        """
        This method adds a child named nodeName to fatherName
        It also sets attribute "value" to value
        It returns the createdNode
        """
        fatherNode = self._getNode(fatherName,*path)
        newNode = self._dom.createElement(nodeName)
        
        newNode = fatherNode.appendChild(newNode)
        if value != "":
            newNode.setAttribute("value", value)
        return newNode
    
    def addMultipleChild(self,value,fatherName,*path):
        """
        This methods adds a child to a multiple node to fatherName
        It also sets attribute "value" to value
        It returns the createdNode name
        """
        
        # Retrieve next "item" number
        tagList= []
        tagList = self._getTags(tagList)
        itemList=[]
        
        for item in tagList:
            p=re.compile("^item_\d+$")
            if p.search(item):
                itemList.append(int(item.split("_")[1]))
                
        itemList.sort()
        try:
            nextItem = "item_"+str(itemList[-1]+1)
        except IndexError:
            nextItem = "item_1"

        self.addChild(nextItem,value,fatherName,*path)
        
        return nextItem
        
    def nodeExists(self,nodeName,*path):
        """
        This method tests if the node exists
        """
        try :
            self._getNode(nodeName,*path)
        except XDRnoNodeException:
            return 0
        else:
            return 1
        
    def tryGetValue(self, default, nodeName, *path):
        """
        Search for the node "nodeName" with elements of "path" in its path and return its value
        If node doesn't exist, use "default" for value
        
        It would be cleaner to use "path" as a list, instead of "*path", but this would change all the geValue calls :(
        To be evaluated later
        """
        
        if not isinstance(default,basestring) :
            raise XDRillFormed("XDRError : the type of default ", default , "is ", type(default), "instead of a basestring")      
        
        try :
            result = self.getValue(nodeName,*path)
        except XDRnoNodeException:
            result = default
            print "Note : Could not find node \"", nodeName, " \" for path" , path ,". Using default value :", default 
        return result

    def getValue(self,nodeName,*path):
        """
        Search for the node "nodeName" with elements of "path" in its path and return its value
        If node doesn't exist, XDRnoNodeException will be raised
        
        It would be cleaner to use "path" as a list, instead of "*path", but this would change all the geValue calls :(
        To be evaluated later
        """
        node = self._getNode(nodeName, *path)
        return node.getAttribute("value").encode("utf-8")

    def getListValue(self, key, *path):
        """
        This procedure gets the asked value from the dataset and returns it as a list
        """
        result = self.getValue(key, *path).split(';')
            
        return result
        
    def setValue(self,value,nodeName,*path):
        """
        This method searches for the node "nodeName" with elements of "path" in its path and change its value to "value"
        If the value is a python list, it's converted into a Tcl list
        """
        
        if isinstance(value,list):
            value2=[]
            for elt in value:
                value2.append(str(elt).strip())
            value=";".join(value2)
            
        try :
            node=self._getNode(nodeName, *path)
            node.setAttribute("value",str(value))
        except :
            print "Note : Could not find node \"", nodeName, " \" for path" , path ,". skipping setValue"
            
        
    def getChildrenName(self,nodeName,*path):
        """
        This method returns a list of the name of all the children of the node named "nodeName".
        If more than one node has the same name, parts of its path can be specified as for the getValue method
        """
        try:
            node=self._getNode(nodeName, *path)
        except XDRnoNodeException:
            return ""
        
        #result=list()
        result=[child.nodeName for child in node.childNodes if child.nodeType == 1]
            #result.append(child.nodeName)
            
        return result
    
    def removeNode(self,nodeName,*path):
        """
        This method removes the node named "nodeName".
        If more than one node has the same name, parts of its path can be specified as for the getValue method
        """
        node=self._getNode(nodeName,*path)
        parent = node.parentNode
        parent.removeChild(node)
        
    def save2file(self,fileName):
        """
        This method save the dataset in C3Sm XML format
        """
        try:
            f=codecs.open(fileName,"w","utf-8")
            self._dom.writexml(f,addindent="  ",encoding="utf-8")
            f.close()
        except IOError:
            error("XDRError : could not save XML content to file "+fileName+" from "+os.getcwd()+". Check path, permissions and disk space.")



 #
 #                   _ _ ______ _ _           
 #    /\            (_|_)  ____(_) |          
 #   /  \   ___  ___ _ _| |__   _| | ___  ___ 
 #  / /\ \ / __|/ __| | |  __| | | |/ _ \/ __|
 # / ____ \\__ \ (__| | | |    | | |  __/\__ \
 #/_/    \_\___/\___|_|_|_|    |_|_|\___||___/
 #                                            
 #                                            

        
class WriteAsciiFile:
    """
    This class allows developers to easily write informations to ASCII files
    It provides writing methods
    """
    def __init__(self,fileName):
        """
        This constructor open "fileName"
        """
        self._outputFile=open(fileName,"w")
        
    def write(self,str):
        """
        This method write the string "str" in the file
        """
        self._outputFile.write(str)
    def writeLine(self,line):
        """
        This method write the string "str" followed by "\n" in the file
        """
        self._outputFile.write(line+"\n")
        
    def close(self):
        self._outputFile.close()
        del self




        
class ReadAsciiFile:
    """ This class provides methods to read and parse ascii files"""
    def __init__(self,fileName):
        try:
            self._inputFile=open(fileName,"r")
        except IOError:
            print "XDRError :\nThe file "+fileName+" has not been found\nPlease check that the command supposed to create it is working well\n==============="
        self._content=self._inputFile.read()
        self._contentList=self._content.split('\n')
        self._inputFile.close()
        
        
    def __iter__(self):
        return self._contentList.__iter__()
        
    def getContent(self):
        """
        This method returns the content of the file
        """
        return self._content
        

 #
 # _____                                          _   _                 
 #|  __ \                                        | | (_)                
 #| |__) |   _ _ __     ___  _ __   ___ _ __ __ _| |_ _  ___  _ __  ___ 
 #|  _  / | | | '_ \   / _ \| '_ \ / _ \ '__/ _` | __| |/ _ \| '_ \/ __|
 #| | \ \ |_| | | | | | (_) | |_) |  __/ | | (_| | |_| | (_) | | | \__ \
 #|_|  \_\__,_|_| |_|  \___/| .__/ \___|_|  \__,_|\__|_|\___/|_| |_|___/
 #                          | |                                         
 #                          |_|                                             
 #   
def getRunsList():
    """ This function returns a list containing the directories which are considered as runs
    """
     # Retrieve list of element in current directory
    listElt = os.listdir(".")
    listDir = []
    for elt in listElt:
        if os.path.isdir(elt):
            # The element is a directory
            p=re.compile('^'+PREFIX+'\d{'+str(DIGIT_NUMBER)+'}$')
            if p.search(elt):
                # The directory is detected as a run
                listDir.append(elt)
    listDir.sort()            
    return listDir

def getCurrentRun():
    """ This function analyses the directories in the current directory and return the name of the directory corresponding to the last "run"
    """
    
    # Retrieve runs list
    listDir = getRunsList()
   
    
    # Sorting the list to get the last element
    listDir.sort()
    if len(listDir) == 0:
        return createNewRun()
    return listDir[-1]

def goIntoCurrentRun():
    """ This function changes directory to go into current run directory, if no run director is present, then it creates RUN001 and goes into it
    """
    currentRun = getCurrentRun()
    
    if currentRun == "":
        currentRun = createNewRun()
        
    os.chdir(currentRun)


def formatRunNumber(int):
    """ This function formats int to be correct towards the defined format for run number
    """
    formatter ="%(#)0"+str(DIGIT_NUMBER)+"d" 
    return formatter % {"#" : int}

def createNewRun():
    """ This function creates a new directory following the last detected run.
        It returns the name of the newly created folder
    """
    # Retrieve runs list
    listDir = getRunsList()

    # Sorting the list to get the last element
    listDir.sort()
    if len(listDir) == 0:
        lastRun = ""
    else:
        lastRun = listDir[-1]
        

    #formatter = '{0:0='+str(DIGIT_NUMBER)+'d}'
    if lastRun == "" :
        #nextRunNumber = formatter.format(1)
        nextRunNumber = formatRunNumber(1)
    else:
        p=re.compile('^'+PREFIX+'(\d{'+str(DIGIT_NUMBER)+'})$')
        m = p.search(lastRun)
        if m:
            #nextRunNumber = formatter.format(int(m.group(1))+1)
            nextRunNumber = formatRunNumber(int(m.group(1))+1)
        else:
            print("XDRError : Return from getLastRun function is not well formatted, contact C3Sm team ...")
        
    nextRun = PREFIX+nextRunNumber
    os.mkdir(nextRun)
    return nextRun

def createNewRunFromLastRun():
    """ This function creates a new directory following the last detected run and duplicate its content
        It returns the name of the newly created folder
    """
    # Retrieve runs list
    listDir = getRunsList()

    # Sorting the list to get the last element
    listDir.sort()
    if len(listDir) == 0:
        lastRun = ""
    else:
        lastRun = listDir[-1]
        

    #formatter = '{0:0='+str(DIGIT_NUMBER)+'d}'
    if lastRun == "" :
        #nextRunNumber = formatter.format(1)
        nextRunNumber = formatRunNumber(1)
        nextRun = PREFIX+nextRunNumber
        os.mkdir(nextRun)
    else:
        p=re.compile('^'+PREFIX+'(\d{'+str(DIGIT_NUMBER)+'})$')
        m = p.search(lastRun)
        if m:
            #nextRunNumber = formatter.format(int(m.group(1))+1)
            nextRunNumber = formatRunNumber(int(m.group(1))+1)
            nextRun = PREFIX+nextRunNumber
            shutil.copytree(lastRun, nextRun)
        else:
            print("XDRError : Return from getLastRun function is not well formatted, contact C3Sm team ...")
        
    
    return nextRun








def saveDatasetInCurrentRun():
    """ This function saves the current dataset in the last known run.
        In order to use this function, you must have initialized the XDR library
    """
    if "XDR" not in sys.modules:
        raise Exception("XDRError : XDR library is not loaded")
    
    try :
        dsout = getDsout()
    except NameError:
        dsout = XDR.dsout() 
    
    currentRun = getCurrentRun()
    dsout.save2file(os.path.join(".",currentRun,currentRun+".xml"))
    

def copyFileInLocalProject(path,tgt_dir="COMMON"):
    """ This function copy the file path into the common directory.
    If the file already exists, it compares the two files and rename the new one if they are different
    It finally returns the new path
    """
    global COMMON_DIRECTORY,OrigAppName
    
    
    
    if tgt_dir == "COMMON" :
        tgt_dir = COMMON_DIRECTORY
        ensureDirectory(COMMON_DIRECTORY,clean=False)
    else :
        ensureDirectory(tgt_dir,clean=False)
    
    tgt_dir = os.path.relpath(tgt_dir,os.getcwd())
        
    
        
        
    # Checking if original path exists
    if not(os.path.exists(path) == True and os.path.isfile(path)):
        msgerr =  "XDRError : the file "+path+" can't be accessed"
        error(msgerr)
    
    # Checking existence of file in COMMON
    dirName,fileName = os.path.split(path) 
    
    
    newPath = os.path.join(tgt_dir,fileName)
    
    
    if fileName in os.listdir(tgt_dir):
        # The file already exists
        print "This file already exists in "+tgt_dir
        
        # Checking equality
        if filecmp.cmp(path,newPath):
            print "File equal, skipping copy..."
            
        else:
            print "Warning : File differ, previous file will be ecrased (ha ha ha)..."
            print "Copying "+path +" in "+newPath
            shutil.copy(path,newPath)
    else :
        print "Copying "+path +" in "+newPath
        shutil.copy(path,newPath)
    return newPath



def copyFileFromDatasetInLocalProject(key,*path):
    """ This function retrieves the path from the field identified by key and path (see getValue() )
        It copies the file from the retrieved path into the common directory.
        If the file already exists, it compares the two files and rename the new one if they are different
        The function then update the field defined by key and path
    """
    originalPath = getValue(key,*path)
    
    newPath = copyFileInLocalProject(originalPath)
    
    setValue(newPath,key,*path)
    return newPath
    
def createFileInCurrentRun(fileName,fileContent):
    """ This function creates a new file named fileName in current run and writes fileContent into it
    """
    
    currentRun = getCurrentRun()
    
    if currentRun == "" :
        currentRun = createNewRun()
    
    filePath = os.path.join(".",currentRun,fileName)
    f = open(filePath,"w")
    f.write(fileContent)
    f.close()
    
    return filePath


 #
 #_____  _                  _____           
 #|  __ \| |                |_   _|          
 #| |__) | |_   _  __ _ ______| |  _ __  ___ 
 #|  ___/| | | | |/ _` |______| | | '_ \/ __|
 #| |    | | |_| | (_| |     _| |_| | | \__ \
 #|_|    |_|\__,_|\__, |    |_____|_| |_|___/
 #                 __/ |                     
 #                |___/                      



class Plugin:
    """ All the plugins inherit from this class 
    """
    def __init__(self,type):
        """
        Initializes the plugin with type = (code|tool)
        """
        if (type != "code") and (type != "tool"):
            error("XDRError : Plugin type was "+type+", it has to be 'code' or 'tool'")
        self.type = type
        self.infos={}
        self.infos['platform']="XDRError : Platform is not specified in current plugin"
        self.infos['parallelCommand'] = ""

        self.dir2send = []
        self.plugin_name = self.__class__.__name__
        self.nbcores = 0
        self.enable_sav = False
        

    def get_cores(self):
        if self.nbcores == 0 :
            msg_err = "Warning : 0 cores reserved for the present computation"
            error(msg_err)
        return self.nbcores
    

#    def sendDirectory(self,directory):
#        """ This method sends directory on a distant place """
#        error("Plugin : sendDirectory() is not implemented in current plugin ...")

    def executeDistantCommand(self,command,execDirectory,flags=[]):
        """ This method goes into execDirectory and execute command """
        error("Plugin : executeDistantCommand() is not implemented in current plugin ...")

    def sendDirectory(self,directory):
        """ This method adds a directory to the run archive. """
        # basename(abspath()) removes the trailing '/'s.
        #local_directory = os.path.abspath(directory)
        #directory_name = os.path.basename(local_directory)

        local_directory = os.path.relpath(directory)

        # We don't check that relpath doesn't contain some '../../../'.
        # Might be an issue.
        # tar removes them on its own, a plain scp might not.

        # Also, handling of symlinks is unknown.
        
        # Check that directory exists
        if not os.path.exists(local_directory):
            error("Error : " + directory + " doesn't exist")

        # Check that we didn't change working directory along the way.
        if (len(self.dir2send) > 0):
            if os.getcwd() != self._dir2send_curdir:
                error("Current directory changed between calls to sendDirectory(). This is not supported !")
        else:
            self._dir2send_curdir = os.getcwd()

        # Add to the list of files to be sent        
        self.dir2send.append(local_directory)

    def retrieveDirectory(self,directory):
        """ This method retrieves directory from the distant place """
        error("Plugin : retrieveDirectory is not implemented in current plugin ...")
        
    def removeDirectory(self,directory):
        """ This method removes directory from the distant place """
        error("Plugin : removeDirectory is not implemented in current plugin ...")
    
    def getPlatform(self):
        """ This method has to be deleted
        """
        error("Plugin : getPlatform souldn't be called anymore")
        
    def getInfos(self):
        """ This methods returns informations regarding the plugin as a dictionary.
        This dictionary is instantiated in the constructor of mother class Plugin
        This method has to be deleted
        """
        error("Plugin : getInfos souldn't be called anymore")
        
    def getPluginParam(self,paramName):
        """ This method searches for paramName where the plugin is configurated and return its value
        """
        try:
            return getValue(paramName,self.__class__.__name__,self.type+"_plugins")
        except XDRtooManyNodesException:
            #error("Your plugin "+self.__class__.__name__+" is configurated in multiple places ...\n Remove it from your config !")
            return getValue(paramName,self.__class__.__name__,self.type+"_plugins",getValue("name","solver","meta"))

    def runSAV(self):
        if not self.enable_sav:
            print "SAV not available for current plugin"
            return

        shutil.copy(os.path.join(os.path.dirname(os.path.abspath(__file__)), "SAV.py"), ".")
        self.sendDirectory('SAV.py')
        self.executeDistantCommand("python SAV.py", ".", "SAV")
        self.retrieveDirectory('XDR-FileDB.csv')

        db = SAV.FileDB('XDR-FileDB.csv')
        print db





def loadPlugin(pluginName):
    
    callDir = os.path.split(scriptDir)[1]
    #print callDir
    #pluginPath = ""
    #if callDir == "XDRpy" :
    #    pluginPath = os.path.join(scriptDir,"..","library","DATA","pluginscripts")
    #if callDir == "scripts" :
    #    pluginPath = os.path.join(scriptDir,"..","..","DATA","pluginscripts")
    #    
    #if pluginPath == "" :
    #    error("Problem, this case is not managed by load plugin")
    pluginsPath = getValue("pluginsPath","engine","meta")

    print "Loading plugin : "+str(os.path.join(pluginsPath,pluginName+".py"))
    
 
    try:
        return imp.load_source(pluginName,os.path.join(pluginsPath,pluginName+".py"))
    except IOError:
        error("XDRError : plugin \""+pluginName+"\" couldn't be found")



def loadCodePlugin(pluginName=""):
    
    if pluginName == "" :
        try:
            pluginName = getValue("code_plugins")
        except XDRtooManyNodesException:
            #error("Your plugin "+self.__class__.__name__+" is configurated in multiple places ...\n Remove it from your config !")
            pluginName = getValue("code_plugins",getValue("name","solver","meta"))

    pluginLib = loadPlugin(pluginName)
    pluginClass = getattr(pluginLib,pluginName)
    plugin = pluginClass("code")
    return plugin

def loadToolPlugin():
    try:
        pluginName = getValue("tool_plugins","config")
    except XDRnoNodeException:
        error("XDRError : Your config file is not up to date")
        
    
    pluginLib = loadPlugin(pluginName)
    pluginClass = getattr(pluginLib,pluginName)
    plugin = pluginClass("tool")
    return plugin


def executeTool(command,directory):
    """ This command execute a tool on a distant service regarding the chosen plugin in config file
    """
   
    
    plugin = loadToolPlugin()

    plugin.sendDirectory(directory)
    
    plugin.executeDistantCommand(command,directory)
    
    plugin.retrieveDirectory(directory)


 #           ______                                                              _ 
 #   ____   |  ____|                                                            | |
 #  / __ \  | |__  __  _____  ___   ___ ___  _ __ ___  _ __ ___   __ _ _ __   __| |
 # / / _` | |  __| \ \/ / _ \/ __| / __/ _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` |
 #| | (_| | | |____ >  <  __/ (__ | (_| (_) | | | | | | | | | | | (_| | | | | (_| |
 # \ \__,_| |______/_/\_\___|\___| \___\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|
 #  \____/                     ______                                              
 #                            |______|                                             


def exec_command(fun):
    """ This function defines the annotation @exec_command, to be used by all plugin scripts.
       It defines self.command_exe (defaults to command), self.local_directory (the current directory)
       that can then be accessed in executeDistantCommand """
    def wrapped_fun(self, command, execDirectory, appli, flags=[]):
        print "Plugin : Running executeDistantCommand " + command + " (appli: " + appli + ") in " + execDirectory
        
        # Initialize some variables
        self.command_exe = command
        self.local_directory = os.getcwd()

        # Used only for testing deployment plugins
        if appli == "-c3sm_auto_deployment_tests":
            if command == "-c3sm_auto_deploy_test_write-":
                self.command_exe = "cat my_input_file > my_new_file"

        # Check that execDirectory makes sense
        if len(execDirectory) == 0:
            print "No execDirectory given. Assuming '.'"
            execDirectory = '.'
        
        if execDirectory[0] == '/' or execDirectory[0] == '~':
            error("execDirectory should be relative to the project directory, not an absolute path")

        # Run the actual command
        r = fun(self, command, execDirectory, appli, flags=[])

        # The command is supposed to have uploaded pending files
        # Empty the list, so they aren't sent again if we invoke executeDistantCommand another time.
        self.dir2send = []

        # Make sure we get back to the initial directory
        os.chdir(self.local_directory)
    return wrapped_fun

 #
 #            _____                              _           _                    _ _           _   _                 
 #   ____    / ____|                            | |         | |                  | (_)         | | (_)                
 #  / __ \  | (___  _   _ _ __  _ __   ___  _ __| |_ ___  __| |  __ _ _ __  _ __ | |_  ___ __ _| |_ _  ___  _ __  ___ 
 # / / _` |  \___ \| | | | '_ \| '_ \ / _ \| '__| __/ _ \/ _` | / _` | '_ \| '_ \| | |/ __/ _` | __| |/ _ \| '_ \/ __|
 #| | (_| |  ____) | |_| | |_) | |_) | (_) | |  | ||  __/ (_| || (_| | |_) | |_) | | | (_| (_| | |_| | (_) | | | \__ \
 # \ \__,_| |_____/ \__,_| .__/| .__/ \___/|_|   \__\___|\__,_| \__,_| .__/| .__/|_|_|\___\__,_|\__|_|\___/|_| |_|___/
 #  \____/               | |   | |                          ______   | |   | |                                        
 #                       |_|   |_|                         |______|  |_|   |_|                                        

def supported_applications(apps):
    """ Annotation @supported_applications(['appli1', 'appli2']) automates checking that a passed application is supported """
    def wrapper(fun):
        def wrapped_fun(self, command, execDirectory, appli, flags=[]):
            # This is a list of apps that the user doesn't have to specifically accept.
            # XDRpy will handle them internally, and developers of deployment plugins
            # don't have to care about them.
            apps_whitelist = ['-c3sm_auto_deployment_tests', 'SAV']

            if (appli not in apps) and (appli not in apps_whitelist):
                error("This application requires the application tag " + appli + ", which is not supported by the plugin " + self.plugin_name)
            return fun(self, command, execDirectory, appli, flags=[])
        return wrapped_fun
    return wrapper

def check_command_exe(command_exe):
    """ Checks that the command is valid. If it starts with -c3sm_auto-, the associated command hasn't been found """
    if command_exe.startswith("-c3sm_auto_"):
        error("command was not understood: "+command_exe )

    
def executeCode(command,currentRunDirectory="None"):
    
    
    error("XDRError : executeCode is not managed anymore in XDR library, update your library")
    
    try:
        pluginName = getValue("code_plugins","config")
    except XDRnoNodeException:
        error("XDRError : Your config file is not up to date")
        return
    
    if currentRunDirectory=="None" :
        currentRunDirectory = getCurrentRun()
    
    typeRun = "code"
    
    plugin = loadPlugin(pluginName)
    
    plugin.sendDirectory(os.path.join(".",COMMON_DIRECTORY),typeRun)
    
    plugin.sendDirectory(os.path.join(".",getCurrentRun()),typeRun)
    
    plugin.executeDistantCommand(command,currentRunDirectory,typeRun)
    
def retrieveCodeDirectory(directory):
    try:
        pluginName = getValue("code_plugins","config")
    except XDRnoNodeException:
        print "XDRError : Your config file is not up to date"
        return
    
    plugin = loadPlugin(pluginName)
    
    plugin.retrieveDirectory(directory,"code")
    
    
    

 #
 #
 # ______                    _   _                 
 #|  ____|                  | | (_)                
 #| |__  __  _____ ___ _ __ | |_ _  ___  _ __  ___ 
 #|  __| \ \/ / __/ _ \ '_ \| __| |/ _ \| '_ \/ __|
 #| |____ >  < (_|  __/ |_) | |_| | (_) | | | \__ \
 #|______/_/\_\___\___| .__/ \__|_|\___/|_| |_|___/
 #                    | |                          
 #                    |_|                          
 #

class XDRException(Exception):
    """ All exceptions raisen by XDR library inherits from this one"""
    
class XDRnoNodeException(XDRException):
    """" No node found """

class XDRtooManyNodesException(XDRException):
    """ Too many nodes have been found"""
        
class XDRnoFileException(XDRException):
    """ No file found """

class XDRillFormed(XDRException):
    """ Ill formed XML address """




def replace_pattern_in_file(file,searchExp,replaceExp):
    """ serach and replace a pattern in a file
    needs the fileinput package"""
    
    if not os.path.exists(file) :
        print "Warning : file not found:"+file
        return
    
    replace = 0
    for line in fileinput.input(file, inplace=1):
        if searchExp in line:
            line = line.replace(searchExp,replaceExp)
            replace += 1
        sys.stdout.write(line)
        
    if replace == 0 :
        print "Warning : no replacement made \n "+file+":"+ searchExp +">"+replaceExp
    else :
        print str(replace)+" replacement made \n"+file+":"+ searchExp +">"+replaceExp
    return
        
 #       
 # ______                               _                _   _                 
 #|  ____|                             | |              | | (_)                
 #| |__ _ __ ___  __ _ _   _  ___ _ __ | |_    __ _  ___| |_ _  ___  _ __  ___ 
 #|  __| '__/ _ \/ _` | | | |/ _ \ '_ \| __|  / _` |/ __| __| |/ _ \| '_ \/ __|
 #| |  | | |  __/ (_| | |_| |  __/ | | | |_  | (_| | (__| |_| | (_) | | | \__ \
 #|_|  |_|  \___|\__, |\__,_|\___|_| |_|\__|  \__,_|\___|\__|_|\___/|_| |_|___/
 #                  | |                                                        
 #                  |_|                                                        

    
def afterstring(text,pattern):
    """
    This procedure searches for the first line beginning with "pattern" in "text" and return the rest of the line
    """
    p= re.compile('^[ \t]*'+pattern+'[\t ]*(.*?)[ \t]*$', re.MULTILINE)
    m=p.search(text)
    if m:
        return m.group(1)
    else:
        print "No result found for "+pattern+" ..."
        return "Error"
    

def grep(text,pattern):
    textLines=text.split("\n")
    p= re.compile(pattern)
    result=[]
    for line in textLines:
        m=p.search(line)
        if m:
            result.append(line)
    return "\n".join(result)
    
    
def betweenlines(text,pattern1,pattern2):
    """
    This procedure return the text between the lines "pattern1" and "pattern2"
    """
    textLines=text.split("\n")
    p=re.compile('^[ \t]*'+pattern1+'[ \t]*$')
    position="out"
    result=[]
    for line in textLines:
        m=p.search(line)
        if m:
            if position=="out":
                p=re.compile('^[ \t]*'+pattern2+'[ \t]*$')
                position="in"
            else:
                position="out"
        else:
            if position=="in":
                result.append(line)
    return "\n".join(result)


def getColumn(text,pos,sep=None):
    """ This procedure reads the lines of "text" and return an array containing the pos-th element of each line
    sep is the separator, if not specified, the separator is assumed to be any kind of whitespace (tab, space, etc ...)
    """
    finalList = []
    result=text.split("\n")
    for line in result:
        toto=line.split(sep)
        if len(toto)>=pos:
            finalList.append(toto[pos-1].strip())
    return finalList

    
def readFile(filename):
    """ This procedure returns the content of the file "filename" """
#    asciiIn = ReadAsciiFile(filename)
#    result = asciiIn.getContent()
    return ReadAsciiFile(filename).getContent()
    
    


def pairwise(iterable):
    """
    This procedure allows to loop in a list in a pairwise fashion
    need package izip
    s -> (s0,s1), (s2,s3), (s4, s5), ...
    """
    a = iter(iterable)
    return izip(a, a)
        
def ensureDirectory(list_strings,clean=False):
    """ Check wether a directory exists in the current working directory
    argument is a list of strings to be contcatenated.
    Returns the directory path in absolute form
    """
    
    if isinstance(list_strings, basestring) :
        dirlist = [list_strings]
    else :
        dirlist = list_strings
    
        
    directory= os.path.join(os.getcwd(),*dirlist)
 
    
    if os.path.exists(directory) :
        if clean :
            print "Ensure clean directory", directory
            # directory exists, and need to clean it 
            shutil.rmtree(directory)
            os.makedirs(directory)
        else :
            # directory exists, and no need to clean it
            pass
            
    else :
        #directory does not exists
        print "Create directory", directory
        os.makedirs(directory)
    
   
    
    return directory

def readAsciiBound(asciiBoundFile):
    asciiBound = readFile(asciiBoundFile)
    asciiBound = asciiBound.split("\n")
   
    nbPatches = int(asciiBound[1].strip().split(" ")[0])

    results= []
    for i,line in enumerate(asciiBound):
        if re.compile("^\s*Patch: ").search(line):
            results.append(asciiBound[i+1].strip())

    if len(results) != nbPatches:
        msg = "Mismatch between the number of patches and the patches read in file "+asciiBoundFile
        error(msg)

    return results


def getFileList(path,filter_search="",output="listonly"):
    """ return the list of files in a path (compulsory) matching a filter string (facultative).
    output (facultative) can be : - listonly : a list of the files
                                  - relative : a list relative pathes
                                  - absolute : a list of absolute path
    """
    
    
    lresult =  []
    glob_arg = os.path.join(path,filter_search)
    list_dir = glob.glob(glob_arg)
    
    
    if output == "relative" :
        for f in list_dir:
                lresult.append(os.path.relpath(f))
    if output == "listonly" :
         for f in list_dir:
                lresult.append(os.path.relpath(f,path))
    if output == "absolute" :
        lresult = list_dir

    #print "getFileList",lresult
    
   
    return sorted(lresult)



 #
 # _____  __  __         _   _ _     
 #|  __ \|  \/  |       | | (_) |    
 #| |__) | \  / |  _   _| |_ _| |___ 
 #|  ___/| |\/| | | | | | __| | / __|
 #| |    | |  | | | |_| | |_| | \__ \
 #|_|    |_|  |_|  \__,_|\__|_|_|___/
 #                                   
                                    
def SimToColor(simu) :
    color = "black"
    if simu == "1" :
        color="black"
    if simu == "2" :
        color="red"
    if simu == "3" :
        color="blue"
    if simu == "4" :
        color="green4"    
    return color

def SimToSymbol(simu) :
    symbol = ""
    if simu == "1" :
        symbol="circle"
    if simu == "2" :
        symbol="square"
    if simu == "3" :
        symbol="triangle"
    if simu == "4" :
        symbol="diamond"
    return symbol

