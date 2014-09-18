#!/usr/bin/env python


#######################################
#
# Projet Manager XMF utilities
#
########################################
# This class allows to handle a cluster of h5/xmf files
# for visualization purpose, through ensight and paraview.
# this clas is originally used by common_visualize.py.
#
# Created by A. Dauptain
#
# Last modified : 16 septembre 2013 A Dauptain (comments)
#                
########################################


import os
from XDR import *


class XMF_Utilities(object):
    def __init__(self,code_id):
        self.code = code_id
        self.post_proc_dir = os.path.join(os.getcwd(),code_id.upper(),"SPATIAL")
        self.filename = os.path.join(self.post_proc_dir,"c3sm_xdmfgroup.xmf")
        
        self.filelocation = []
        if self.code == "avbp" :
            self.filelocation = ["SOLUT"]
        if self.code == "avtp" :
            self.filelocation = ["SOLUT"]    

        self.time_id_list = [0.0]
        self.faketime = False
        if self.code == "avsp" :
            self.faketime = True
        
        
        # cleaning
        for f in getFileList(self.post_proc_dir,"*.xmf","relative") :
            print "Cleaning file :",f
            os.remove(f)
        
        
    def CreateTemporaryXMFs(self,selection_files,projectLocations):
        """ This procedure create the main XMF solution file according to
        post_proc_dir is a string of the form AVSP, AVBP or YALES2,
        the data will be generated in the relative path ./AVSP/SPATIAL for exemple
        
        selection_files is a list of  the form  [file1],[0],[file2],[1],[file3],[1],[file6],[0]
        with files of the form "identifier # simulation # projectname # name "
        
        projectLocations is a dictionnary with project names as keys, and  project relative pathes as values
        created smthg similar to:
        projectLocations={}
            for sc_project in getChildrenName("projects","allprojects"):
                project_xml=getValue("sc_project","sc_project")
                project_folder=project_xml.replace(".xml","")
                project_name = os.path.splitext( os.path.basename(project_xml) )[0]
                projectLocations[project_name] = project_folder
        """
        
        
        
        
        print "Create main XMF file "+self.filename
        
        xmfsolgroup = open(self.filename, 'w')
        
        # header
        xmfsolgroup.write("""<?xml version="1.0" ?>
        <!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" >
        <Xdmf xmlns:xi="http://www.w3.org/2003/XInclude" Version="2.0">
        <Domain>
          <Grid GridType="Collection" CollectionType="Temporal" >
        """)
        
        
        # listy ids stores the data necessary for tags in images (enspythoncut)
        self.list_ids = []     
         
        for item,onoff in pairwise(selection_files) :
            #print onoff
            if onoff=="1" :
                # retreive project path
                sol_identifier = item.split('#')[0]
                sol_simu = item.split('#')[1]
                sol_project_name = item.split('#')[2]
                sol_run = item.split('#')[3]
                sol_name = item.split('#')[4]

                #Check if coupling or not on AVTP or AVBP
                addfolder=""
                if self.code == "avtp" :
                    cpldir = os.path.join(projectLocations[sol_project_name],sol_run,"AVTP01")
                    cpl_avtpfile = getFileList(cpldir,"input_couple.dat")
                    cplfile="input_couple.dat"
                    if cplfile in cpl_avtpfile :
                        print "we are in couple mode of AVTP"
                        addfolder="AVTP01"
                if self.code == "avbp" :
                    cpldir = os.path.join(projectLocations[sol_project_name],sol_run,"AVBP01")
                    cpl_avtpfile = getFileList(cpldir,"input_couple.dat")
                    cplfile="input_couple.dat"
                    if cplfile in cpl_avtpfile :
                        print "we are in couple mode of AVBP"
                        addfolder="AVBP01"
                
             
                temporary_name = sol_project_name+"_"+sol_run+"_"+sol_name
                sol_path =os.path.join(projectLocations[sol_project_name],sol_run,addfolder,*self.filelocation)
                
                time_id = self.getTimeInXMF(os.path.join(sol_path,sol_name))
 
                # declaration of the file
                xmfsolgroup.write('     <xi:include href="'+temporary_name+'" xpointer="xpointer(//Xdmf/Domain/Grid)" />\n')
            
                # copy of directing file
                self.copy_XMF_file_updating_path(sol_name,sol_path,temporary_name,self.post_proc_dir,time_id)
                
                # solution description
                self.list_ids.append(sol_identifier+"#"+sol_simu+"#"+sol_project_name+"#"+sol_run)
        
             
        # end     
        xmfsolgroup.write("""   </Grid>
        </Domain>
        </Xdmf>
        """)
                
        xmfsolgroup.close()
        
    
    def getTimeInXMF(self,filename):
        """ get the time id in an XMF file """
        
        # search for time
        if not os.path.exists(filename) :
            msg =  "File "+ filename+ " Does not exists"
            error(msg)
        else :
             # if time must be faken
            if self.faketime :
                time_id =  self.time_id_list[-1] + 1.0
            else :
                xmffile =  open(filename,"r")
                lines = xmffile.readlines()
                xmffile.close()
                time_id = "-1"
                for line in lines :
                    if "Time Value=" in line :
                        time_id = float(line.split('"')[1].strip())
            
                # case time is not faken, but no time foun 
                if time_id == "-1" :
                    msg = "No Time value stored in the file "+ filename+ ". Exiting..."
                    error(msg)

        # avoid time duplicate
        if time_id in  self.time_id_list :
            time_id +=  self.time_id_list[-1]/100
            
        self.time_id_list.append(time_id)
        return str(time_id)
            
    
    
    def copy_XMF_file_updating_path(self,filename,location,new_filename,new_location,time_id):
        """ create a local copy of the target file, replacing the local relative path references by absolute ones
        time_id is written in the XMF to serve as an additionnal data
        """    
        
        print "Copy "+filename+ " into"+ new_location+", updating the inner pathes"
        
        location = location.rstrip('/')
        
        initfile =  open(os.path.join(location,filename),"r")
        lines = initfile.readlines()
        initfile.close()
        
        location_m1=os.path.dirname(location)
        location_m2=os.path.dirname(location_m1)
        location_m3=os.path.dirname(location_m2)
        
        h5file = filename.replace(".xmf",".h5")
        
        final_lines=[]
        # parsing document
        for line in lines :
            newline=line
            
            # time value must be given After the Grid statment
            if "Time Value" in line:
                newline = ''
                
            if "<Grid" in newline :
                newline = '<Grid name="'+self.code.upper()+' by C3Sm">\n'+ '    <Time Value="           {0}"/>\n'.format(time_id)
                
                
            if h5file in newline :
                newline = newline.replace(h5file,os.path.join(location,h5file))
            
            if "../../../" in newline:
                newline=newline.replace("../../../",location_m3+"/")
            elif "../../" in newline:
                newline=newline.replace("../../",location_m2+"/")
            elif "../" in newline:
                newline=newline.replace("../",location_m1+"/")
            elif "./" in newline:
                newline=newline.replace("./",location+"/")
            final_lines.append(newline)
        
                  
        newfile = open(os.path.join(new_location,new_filename), 'w')
        newfile.writelines(final_lines)
        newfile.close()
    
     
    

if __name__ == '__main__':
    pass # Do tests