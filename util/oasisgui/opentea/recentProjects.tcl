#  This program is under CECILL_B licence. See footer for details.

proc getRecentProjects {} {
    global configPath
    set recentPath [file join [file dirname $configPath] "recents.xml"]
    # Test if the file already exists
    if [file exists $recentPath] {
        # Loads the file
        set tree ""
        dTree_init tree
        parseFile $recentPath tree "" "DStree"
        
        # Transforms old recents.xml version into the new one
        if {[dTree_nodeExists $tree "root recents"] == 0 } {
            debug "dataset does not exist"
            foreach child [dTree_getChildren $tree "root" ] {
                dTree_moveBranch tree "root $child" tree "root recents"
            }
        }
        
        return $tree
    } else {
        # Create the file
        set tree ""
        dTree_init tree
        dTree_addNode tree "root" "recents"
        #saveXMLTree $recentPath $tree
        return $tree
    }
}

proc newRecentProject {XMLfile code} {
    global topPath
    set recentPath [file join $topPath "recents.xml"]
    set XMLfile [file normalize $XMLfile]
    # Test if the file already exists
    
        # Loads the file
    set tree ""
    dTree_init tree
    
    if [file exists $recentPath] {
       parseFile $recentPath tree "" "DStree"
        # Transforms old recents.xml version into the new one
        if {[dTree_nodeExists $tree "root recents"] == 0 } {
            
            foreach child [dTree_getChildren $tree "root" ] {
                dTree_moveBranch tree "root $child" tree "root recents"
            }
        }        
       foreach elt [dTree_getChildren $tree "root recents"] {
           if {[dTree_getAttribute $tree "root recents $elt" "path"] == $XMLfile} {
               dTree_rmNode tree "root recents $elt"
           }
       }
    } else {
        dTree_addNode tree "root" "recents"
    }
    
    set id [clock seconds]
    dTree_addNode tree "root recents" $id
    dTree_setAttribute tree "root recents $id" "path" $XMLfile
    dTree_setAttribute tree "root recents $id" "code" $code
    
    #saveXMLTree $recentPath $tree

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