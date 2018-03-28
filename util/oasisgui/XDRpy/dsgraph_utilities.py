#!/usr/bin/env python


#######################################
#
# Projet Manager Graph Dataset utilities
#
########################################
# This class allows to handle a the datasets stored in xml files
# for further datamining thourough the compare widget
#
# Created by A. Dauptain
#
# Last modified : 16 septembre 2013 A Dauptain (comments)
#                
########################################


import os
from XDR import getFileList


class DSGraph_Utilities(object):
    def __init__(self,code_id):
        self.solver = code_id
        self.post_proc_dir = os.path.join(os.getcwd(),code_id.upper(),"COMPARE")
        self.runlist = []
        
        # cleaning
        for f in getFileList(self.post_proc_dir,"*.dat","relative") :
            print "Cleaning file :",f
            os.remove(f)


    def DatasetDump(self,ds,simu,project_name,run_name):
        """ This procedure dup the content of an XML file in a key/value file
        , to be read afterward for comparisons """
        
        run_id = "Sim."+simu+"#"+project_name+"#"+run_name
        self.runlist.append(run_id)
        
       
        
        address = []
        address.append("dataset")
        
        arraylist = []
        #point de depart
        arraylist.append('dataset-value= \n')
        arraylist.append("dataset-children= "+self.solver+"\n")
        address.append(self.solver)
        
        # EXTENSION PAR RECURSION
        arraylist.extend(self.DatasetDumpIterate(ds,address))
        
        # SAUVEGARDE
        filename=os.path.join( self.post_proc_dir, run_id+".dat")
        tracefile = open(filename, 'w')
        tracefile.writelines(arraylist)
        tracefile.close()
        
    
    def DatasetDumpIterate(self,ds,address):
        """ This procedure is the iterative part of DatasetDump procedure """
         
        dsadr = []
        arraylist=[]
        dsadr.append(address[-1])
        
       
        
        dsadr.extend(address[0:-1])
        value=ds.getValue(*dsadr)
        if type(value) is list:
            value=" ".join(value)
            
        listChildren = ds.getChildrenName(*dsadr)
        for banned in ["avbp_db","scriptNode", "projcontent"]:
            if banned in listChildren :
                listChildren.remove(banned)
        
        arraylist.append(".".join(address)+'-value= '+str(value)+'\n')
        arraylist.append(".".join(address)+"-children= "+" ".join(listChildren)+"\n")
        
        
        
        for child in listChildren: 
            newaddress=list(address)
            newaddress.append(child)
            arraylist.extend(self.DatasetDumpIterate(ds,newaddress))        
        return arraylist



if __name__ == '__main__':
    pass # Do tests