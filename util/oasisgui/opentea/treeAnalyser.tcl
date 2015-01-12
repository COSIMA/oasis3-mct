#  This program is under CECILL_B licence. See footer for details.


################################################################
# This function analyses a parsed tree to
#       - Fill attributes of nodes from a directory
#       - Be sure keywords are well-formed
#ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ
proc analyseModelTree {itree nodes_to_skip solvers_to_add} {
    global nodesToDelete libraryPath dataPath solverOrder
    upvar $itree tree
#    analyseModelNode tree "root"
    
    
    # delete nodes if requested
    foreach solver_to_add $solvers_to_add {
        set solver [lindex $solver_to_add 0]
        set order [lindex $solver_to_add 1]
        
        puts "## GRAFTING SOLVER $solver"
        
        set solverOrder($solver) $order
        
        set modelPath_add [file normalize [file join $libraryPath $solver XML]]
        
        OpenTeaXML2tree XMLtree_add $modelPath_add $dataPath
        
        # not necessary apparently, to be checked thoroughly if problems arise
        #analyseModelTree XMLtree_add $nodes_to_skip ""
        
        dTree_copyBranch XMLtree_add "root $solver" tree "root"
    }
    
    
    
    # Perform all inclusions 
    set nodesToDelete ""
    # Analyse each child
    set childrenList [dTree_getChildren $tree "root"]
   
    foreach child $childrenList {
        set childNode "root $child"
        performInclusionForNode tree $childNode
    }
    set nodesToDelete [lsort -unique $nodesToDelete]
    foreach address $nodesToDelete {
        puts "Pruning branch $address"
        dTree_rmBranch tree $address        
    }    
    
    # delete nodes if requested
    foreach node_to_skip $nodes_to_skip {
        set resultsAddress [dTree_searchNode $tree $node_to_skip]
        foreach address $resultsAddress {
            dTree_rmBranch tree $address 
        }
    }
    
    
    
    # Analyse every node
    set nodesToDelete ""
    # Analyse each child
    set childrenList [dTree_getChildren $tree "root"]
    foreach child $childrenList {
        set childNode "root $child"
        analyseModelNode tree $childNode
    }
    set nodesToDelete [lsort -unique $nodesToDelete]
    foreach address $nodesToDelete {
        dTree_rmBranch tree $address        
    }
}


proc performInclusionForNode {itree node} {
    upvar $itree tree
    global nodesToDelete
    
    # NodeType
    if {[catch {set nodeType [dTree_getAttribute $tree "$node" nodeType]} ]} {
        popup_error "no nodeType detected for the node $node. Please check your XML file system" strict
    }
    
    ####################################
    # Except "DATA" directory
    #ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ
    if {$node == "root DATA"} {
        return 0
    }
    
    ####################################
    # Perform inclusions when detecting "include"
    #ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ
    if {$nodeType == "include"} {
        # Be sure address is written
        if {[catch {set addrInclude [dTree_getAttribute $tree "$node" "address"]}]} {
            popup_error "No attribute -address- found in node : $node \n Please, be sure it is well written " strict
        }
        set resultsAddress [dTree_searchNode $tree $addrInclude]
        set resultsCount [llength $resultsAddress]
        if {$resultsCount == 0 } {popup_error "$addrInclude not found while reading node : $node\nPlease, be sure it well written" strict}
        if {$resultsCount > 1} {
           set resultsAddress [list [ lindex $resultsAddress 0]]
        }
        puts "Grafting branch $resultsAddress in [lrange $node end-2 end-1]" 
        
        # We are now sure the address is unique
        set includeAddress [lindex $resultsAddress 0]
        
        # Delete the include node
        dTree_rmBranch tree "$node"
        
        
        dTree_copyBranch tree "$includeAddress" tree "[lrange $node 0 end-1]"
        lappend nodesToDelete "$includeAddress"
        return
    }    
    
    ####################################
    # Analyse each child for inclusion
    #ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ
    set childrenList [dTree_getChildren $tree $node]
    foreach child $childrenList {
        set childNode "$node $child"
        if {[dTree_nodeExists $tree $childNode]} {
            performInclusionForNode tree $childNode
        }
    }    
}







################################################################
# Recursive part of the analyseModelTree function
#ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ
proc analyseModelNode {itree node} {
    upvar $itree tree
    global nodesToDelete additionalWidgets
    
    
    
    
    
    # NodeType
    if {[catch {set nodeType [dTree_getAttribute $tree "$node" nodeType]} ]} {
        popup_error "no nodeType detected for the node $node. Please check your XML file system" strict
    }
    ####################################
    # Except "DATA" directory
    #ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ
    if {$node == "root DATA"} {
        return 0
    }
    
    
   
    
    ####################################
    # Add item node in the multiple
    #ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ    
    if {$nodeType == "multiple"} {
        set multipleChildren [dTree_getChildren $tree $node]
        dTree_addNode tree $node "item"
        dTree_setAttribute tree "$node item" "nodeType" "item"
        dTree_setAttribute tree "$node item" "name" "item"
        dTree_setAttribute tree "$node item" "nodeType" "item"        
        foreach child $multipleChildren {
            dTree_moveBranch tree "$node $child" tree "$node item"            
        }
        
    }
    
    
    
    set childrenList [dTree_getChildren $tree $node]
    ####################################
    # Fill attributes of directory nodes
    # Move docu nodes to attribute of the father
    # Include "include"
    #ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ
    foreach child $childrenList {
        # get name of the child if it exists
        
        if {![catch {set childName [dTree_getAttribute $tree "$node $child" name]}]} {
            if {$childName == "folderParameters"} {
                # If child's name is 'folderParameters'  ...

                # ... Attributes from the child are stored ...
                set childAttributes [dTree_getAttributes $tree "$node $child"]
                
                # ... into the father!
                foreach attribute $childAttributes {
                    if {[lindex $attribute 0] != "name"} {
                        dTree_setAttribute tree "$node" [lindex $attribute 0] [lindex $attribute 1]
                    }
                }
                
                # Delete the child (love this one ...)
                dTree_rmNode tree "$node $child"
                
            }
        }
        
        if {$child == "docu"} {
            dTree_setAttribute tree "$node" "docu" "[dTree_getAttribute $tree "$node docu" XMLContent]"
            dTree_rmNode tree "$node docu"
        }
        
        if {$child == "desc"} {
            dTree_setAttribute tree "$node" "desc" "[dTree_getAttribute $tree "$node desc" XMLContent]"
            dTree_rmNode tree "$node desc"
        }
        
        
        
    }
    
    
    ####################################
    # Analyse elements of the node
    #ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ
    
    # A analyser :
    # -ValiditŽ du nodeType
    # -PrŽsence des attributs en fonction du nodeType
    # -Default correspond ˆ un des enfants
    # - Place dans l'arbre
    
    
    # NodeType
    if {[catch {set nodeType [dTree_getAttribute $tree "$node" nodeType]} ]} {
        popup_error "no nodeType detected for the node $node. Please check your XML file system"
    }
    set possibleNodeTypes "docu status solver tab comment comparator graph glance timeline viewer view3d view3d_source model xor action info param choice option multiple item" 
    
    foreach widget $additionalWidgets {
        lappend possibleNodeTypes  $widget
    }
    
    
    if {[lsearch $possibleNodeTypes $nodeType] < 0} {
        popup_error "$nodeType is not a valid nodeType for the node $node.\nIt has to be one of the following :\n$possibleNodeTypes" strict
    }
    
    # Check attributes and depending on the nodeType
    # quite loose for the moment.
    
    set checkName 1
    set checkValue 0
    
    
    if {$nodeType ni $possibleNodeTypes} {
        error "You got to add the nodeType you invented in the switch in the tree analyser (where this message is emitted from)"
    }
    
    switch $nodeType {
        "docu" {
            set checkName 0   
        }
        "status" {
            set mandatoryArgs "name"
            set optionalArgs "msgtrue msgfalse msgunknown default"
        }
        "solver" {
            set mandatoryArgs "name title"
            set optionalArgs "size"
        }
        "comment" {
            set mandatoryArgs "name title"
        }
        "comparator" {
            set mandatoryArgs "name title folder require"
        }
        "graph" {
            set mandatoryArgs "name title"
        }
        "glance" {
            set mandatoryArgs "name title"
        }
        "timeline" {
            set mandatoryArgs "name title"
        }
        "tab" {
            set mandatoryArgs "name title"
            set optionalArgs "script existif custombutton"
        }
        "model" {
            set mandatoryArgs "name title"
            set optionalArgs "existif layout"
        }
        "xor" {
            if {![catch {set defaultTemp [dTree_getAttribute $tree "$node" default]} ]} {
                set children [dTree_getAttribute $tree "$node" children]
                if {[lsearch $children $defaultTemp] < 0} {
                    popup_error "In $node,\nThe 'default' attribute you specified can't be identified among the options : $children" strict
                }
            }
            set optionalArgs "groups"
        }
        "param" {
            set mandatoryArgs "name title type"
            set optionalArgs "existif require fixed default filter ratio labels headings controls selection"
            
        }
        "option" {
            set checkName 0
            set checkValue 1
        }
        "multiple" {
            set mandatoryArgs "size"
            
        }
        "include" {
            set checkName 0
            # But normally, the case is treated above
        }
        default {
            
        }
    }
    
    # name presence
    if {$checkName == 1 && [catch {set nameTemp [dTree_getAttribute $tree "$node" name]} ]} {
        popup_error "no name detected for the node\n $node\n Please check your XML file system" strict
    }
    
    # value presence
    if {$checkValue == 1 && [catch {set valueTemp [dTree_getAttribute $tree "$node" value]} ]} {
        popup_error "no value detected for the node\n $node\n Please check your XML file system" strict
    }

    # title presence, if not, copy name
    if {[catch {set titleTemp [dTree_getAttribute $tree "$node" title]} ]} {
        if {$checkName} {
            dTree_setAttribute tree "$node" title [dTree_getAttribute $tree "$node" name]
        }
    }
    
    ####################################
    # Analyse each child
    #ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ
    set childrenList [dTree_getChildren $tree $node]
    foreach child $childrenList {
        set first_char [string index $child 0]
         if {[string map {";" ""} $child ] != $child} {
            popup_error "Semicolons (character ;) are not allowed for the node \n $node + $child \nPlease check your XML file system"   strict 
        }
        if {[string map {" " ""} $child ] != $child} {
            popup_error "Spaces are not allowed for the node  \n $node + $child \nPlease check your XML file system" strict
        }
        # firstchar rule
    
        if {[string tolower $first_char] != $first_char} {
            popup_error "Ill formed node :  uppercase letters are not allowed at the begginning of the node \n $node + $child \n Please check your XML file system" strict 
        }
        set forbidden_chars [list  "." "_"  ":" "?" "!"]
        if {$first_char in $forbidden_chars} {
            popup_error "Ill formed node :  nodes names cannot start with  punctuation marks \n $node + $child \n Please check your XML file system" strict 
        }
    
     
        
        
        
        set childNode "$node $child"
        if {[dTree_nodeExists $tree $childNode]} {
            analyseModelNode tree $childNode
        }
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