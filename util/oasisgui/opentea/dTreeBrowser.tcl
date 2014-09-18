#  This program is under CECILL_B licence. See footer for details.

proc dTreeBrowser_create {win} {
    ttk::frame $win
    ttk::scrollbar $win.ybar -command "$win.treev yview"
    ttk::scrollbar $win.xbar -orient horizontal -command "$win.treev xview"
    ttk::treeview $win.treev -yscrollcommand "$win.ybar set" -xscrollcommand "$win.xbar set" -columns "attribute value"

    
    $win.treev heading #0 -text "Node"
    $win.treev heading #1 -text "Attribute"
    $win.treev heading #2 -text "Value"    
       
    pack $win.ybar -side right -fill y
    pack $win.xbar -side bottom -fill x
    pack $win.treev -side left -fill both -expand 1
    return $win
}

proc dTreeBrowser_fill {args} {
    set win [lindex $args 0]
    set tree [lindex $args 1]
    set status 0
    catch {set status [lindex $args 2]}
    dTreeBrowser_fillNode $win $tree "root" $status
}

proc dTreeBrowser_fillNode {win tree node status} {
    global widgetInfo
    set attributs [dTree_getAttributes $tree $node]
    set children [dTree_getChildren $tree $node]
    set father [lrange $node 0 end-1]
    set name [lindex $node end]
    
    if {$status == 1} {
        catch {lappend attributs [list "status" $widgetInfo([tree2gui $node]-status)]}
        catch {lappend attributs [list "visible" $widgetInfo([tree2gui $node]-visible)]}
        
    }
    
    $win.treev insert $father end -id $node -text $name  -open true -tags "tree_nodes"
    
    foreach att $attributs {
        $win.treev insert $node end -text "" -values $att -tags "tree_attributes"
    }
    
    foreach child $children {
        set dummy_node $node
        lappend dummy_node $child
        dTreeBrowser_fillNode $win $tree $dummy_node $status
    }
  
    $win.treev tag configure "tree_nodes" -background grey80
    $win.treev tag configure "tree_attributes" -background grey90
    $win.treev column  #0 -stretch 0
    $win.treev column  #1 -stretch 0
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