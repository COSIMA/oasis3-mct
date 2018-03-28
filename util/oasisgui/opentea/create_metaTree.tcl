#  This program is under CECILL_B licence. See footer for details.

proc fill_metaTree { } {
    uplevel 1 {} {
    global loadApplication relaunchCmd pluginsPath libraryPath configPath
        
        
    dTree_init metaTree
        
    dTree_addNode metaTree "root" "meta"
    
    # SOLVER INFORMATION
    dTree_addNode metaTree "root meta" "solver"
    dTree_addNode metaTree "root meta solver" "name" 
    dTree_setAttribute metaTree "root meta solver name" "value" "$loadApplication"
    
    #PROJECT INFORMATION (initialization)
    dTree_addNode metaTree "root meta" "project"
    dTree_addNode metaTree "root meta project" "name"    
    dTree_setAttribute metaTree "root meta project name" "value" "undefined"        
    dTree_addNode metaTree "root meta project" "address"    
    dTree_setAttribute metaTree "root meta project address" "value" "undefined"

    # USER INFORMATION
    dTree_addNode metaTree "root meta project" "username"
    set username [ getConfig "config id user name"]
    dTree_setAttribute metaTree "root meta project username" "value" $username
    dTree_addNode metaTree "root meta project" "company"
    set company [ getConfig "config id user company"]
    dTree_setAttribute metaTree "root meta project company" "value" $company

    # INFORMATIONS FOR SCRIPT EXECUTION
    dTree_addNode metaTree "root meta" "action"
    dTree_addNode metaTree "root meta action" "callingAddress"
    dTree_addNode metaTree "root meta" "temporary"
    dTree_addNode metaTree "root meta" "scriptSuccess"    
    dTree_addNode metaTree "root meta" "engine"
    dTree_addNode metaTree "root meta engine" "name"
    dTree_setAttribute metaTree "root meta engine name" "value" "OpenTEA"
    dTree_addNode metaTree "root meta engine" "launchCommand"
    dTree_setAttribute metaTree "root meta engine launchCommand" "value" "$relaunchCmd"
    dTree_addNode metaTree "root meta engine" "pluginsPath"
    dTree_setAttribute metaTree "root meta engine pluginsPath" "value" "$pluginsPath"
    dTree_addNode metaTree "root meta engine" "libraryPath"
    dTree_setAttribute metaTree "root meta engine libraryPath" "value" "$libraryPath"
    dTree_addNode metaTree "root meta engine" "configPath"
    dTree_setAttribute metaTree "root meta engine configPath" "value" "$configPath"
    
    }
}


#  Copyright CERFACS 2014
#   
#  antoine.dauptain@cerfacs.fr
#   
#  This software is a computer program whose purpose is to ensure technology
#  transfer between academia and industry.
#   
#  This software is governed by the CeCILL-B license under French law and
#  abiding by the rules of distribution of free software.  You can  use, 
#  modify and/ or redistribute the software under the terms of the CeCILL-B
#  license as circulated by CEA, CNRS and INRIA at the following URL
#  "http://www.cecill.info". 
#   
#  As a counterpart to the access to the source code and  rights to copy,
#  modify and redistribute granted by the license, users are provided only
#  with a limited warranty  and the software's author,  the holder of the
#  economic rights,  and the successive licensors  have only  limited
#  liability. 
#   
#  In this respect, the user's attention is drawn to the risks associated
#  with loading,  using,  modifying and/or developing or reproducing the
#  software by the user in light of its specific status of free software,
#  that may mean  that it is complicated to manipulate,  and  that  also
#  therefore means  that it is reserved for developers  and  experienced
#  professionals having in-depth computer knowledge. Users are therefore
#  encouraged to load and test the software's suitability as regards their
#  requirements in conditions enabling the security of their systems and/or 
#  data to be ensured and,  more generally, to use and operate it in the 
#  same conditions as regards security. 
#   
#  The fact that you are presently reading this means that you have had
#  knowledge of the CeCILL-B license and that you accept its terms.