#  This program is under CECILL_B licence. See footer for details.

# This library is intended to provide a new data structure, the tree, to Tcl 8.5 using dictionaries

#####################
# Adresses procedures
#####################

proc gui2tree address {
    return [split $address .]
}

proc tree2gui address {
    return [join $address .]
}

proc addrGetFather address {
    return [tree2gui [lrange [gui2tree $address] 0 end-1]]
}

proc addrGetAncestors address {
    set result ""
    while {$address != ""} {
        set address [addrGetFather $address]
        if {$address != ""} {
            lappend result $address
        }
    }
    return $result
}

# This procedure creates a tree by creating the root node 
proc dTree_init {dtree} {
    upvar $dtree tree
    set tree [dict create]
    dict set tree "root" description "root of the tree"
    dict set tree "root" nodeType "node"
    dict set tree "root" children [list]
}


# This procedure adds a node named node as a children of key in the tree dtree
proc dTree_addNode {dtree key node} {
     upvar $dtree tree
    
     if {[dict exists $tree $key]} {
              
        if {[dict exists $tree "$key $node"]} {
            error "The node you are trying to add already exists ($key $node)"
        }
        set value [dict get $tree $key children]
        lappend value $node
        dict set tree $key children $value
        lappend key $node
        dict set tree $key children ""
     } else {
        #If the father doesn't exist, let's create it !
        dTree_addNode tree [lrange $key 0 end-1] [lindex $key end]
        dTree_addNode tree $key $node
        dTree_setAttribute tree "$key" "value" ""
        #error "You're trying to add a node but the father node ($key) doesn't exist"
     }
}

# used to deref multiple nodes 
proc dTree_cleanKey2 { key } {
    set key2 ""
    foreach key_elt $key {
        if {[string match item_* $key_elt]} {
            lappend key2 "item"
        } else {
            lappend key2 $key_elt
        }
    }
    return $key2
}

#  fonctionne , mais delicat a cause de l'addresse qui doit etre protégée par des {}
proc dTree_cleanKey { key } {
    foreach j [lsearch -all  $key "item_*"] {
        set key [ lset key $j "item"]
    }
    return $key
}

proc dTree_setAttribute {dtree key attribute value} {
    upvar $dtree tree
    if {[dict exists $tree $key]} {
        dict set tree $key $attribute $value 
    } else {
        #error "You're trying to set an attribute on a node which doesn't exist"
        dTree_addNode tree [lrange $key 0 end-1] [lindex $key end]
        dict set tree $key $attribute $value        
    }
}



proc dTree_getAttribute_fast {tree key attribute} {
    return [dict get $tree "$key" $attribute]
}


proc dTree_getAttribute {tree key attribute} {
    
    if {[dict exists $tree "$key" $attribute]} {
        return [dict get $tree "$key" $attribute]
    } else {
        set key_clean [dTree_cleanKey $key]
        if {[dict exists $tree $key_clean $attribute]} {
            return [dict get $tree $key_clean $attribute]
        } else {
            error "You're trying to get an attribute which doesn't exist : $key > $attribute"
        }
    }
    
}


proc dTree_tryGetAttribute_fast {tree key attribute default} {
    
    if {[dict exists $tree "$key" $attribute]} {
        return [dict get $tree "$key" $attribute]
    } else {   
        return $default
    }  
}


proc dTree_tryGetAttribute {tree key attribute default} {
    
    if {[dict exists $tree "$key" $attribute]} {
        return [dict get $tree "$key" $attribute]
    } else {
        set key_clean [dTree_cleanKey $key]
        if {[dict exists $tree $key_clean  $attribute]} {
            return [dict get $tree $key_clean  $attribute]
        } else {
            return $default
        }
    }  
}



proc dTree_getAttributes {tree node} {
    set result ""
    dict for {key value} [dict get "$tree" "$node"] {
        if {$key != "children"} {lappend result [list $key $value]}
    }
    return $result
}


proc dTree_getChildren_fast {tree key} {
    return [dict get $tree "$key" "children"]
}

proc dTree_getChildren {tree key} {
    #return [dict get $tree $key "children"]
    if {[dict exists $tree "$key" "children"]} {
        return [dict get $tree "$key" "children"]
    } else {
        set key_clean [dTree_cleanKey $key]
        if {[dict exists $tree $key_clean "children"]} {
            return [dict get $tree $key_clean "children"]
        } else {
            error "You're trying to get children from a node which doesn't exist : $key "
        }
    }
}

proc dTree_getUsedChildren {tree key} {
    # This function acts like dTree_getChildren but it returns relevant children only
    # For example, in a xor, only the selected children are returned
    # This procedure should only be used in tmpTree
    global XMLtree widgetInfo
    set cleanKey [dTree_cleanKey $key]
    if {[dTree_attrExists $XMLtree $cleanKey "nodeType"]} {
        set nodeType [dTree_getAttribute_fast $XMLtree $cleanKey "nodeType"]
        switch $nodeType {
            "xor" {
                return [dTree_getAttribute_fast $tree $key "value"]
            }
            default {
                set finalChildren ""
                set children [dTree_getChildren $tree $key]
                foreach child $children {
                    if {[info exists widgetInfo([tree2gui "$key $child"]-visible)] == 0} {
                        lappend finalChildren $child                        
                    } elseif {$widgetInfo([tree2gui "$key $child"]-visible) == 1} {                
                        lappend finalChildren $child
                    }
                }
                return $finalChildren
            }
        }
    } else {
        #warning "The node $key doesn't have a corresponding node in the XMLtree"
        #eturn [dTree_getChildren $tree $key]
        return ""
    }
 
}

proc dTree_rmAttribute {dtree key attribute} {
    upvar $dtree tree
    if {[dict exists $tree $key $attribute]} {
        dict unset tree $key $attribute 
    } else {
        error 0 "You're trying to remove an attribute which doesn't exist"
    }
}

proc dTree_rmNode {dtree key} {
    upvar $dtree tree
    if {[dict exists $tree $key]} {
        dict unset tree $key
        set father [lrange $key 0 end-1]
        set child [lrange $key end end]
        set fatherListChildren [dict get $tree $father children]
        set idx [lsearch -exact $fatherListChildren $child]
        set fatherListChildren [lreplace $fatherListChildren $idx $idx]
        dict set tree $father children $fatherListChildren
    } else {
        error "You're trying to remove a node which doesn't exist"
    }
    
}

proc dTree_rmBranch {dtree node} {
    upvar $dtree tree
    foreach child [dTree_getChildren $tree "$node"] {
        dTree_rmBranch tree "$node $child"
    }
    dTree_rmNode tree "$node"
}

# behaves like a cp 
# node source adress like : aze rty uio node1
# node destination addres like : qsd fgh jkl
# output is : qsd fgh jkl node1

# "nodeName" is the future father of the copied node
# $node_dest $nodeName" is the full address of the copied node


proc dTree_copyBranch {dtree_source node_source dtree_dest node_dest} {
    upvar $dtree_source tree1
    upvar $dtree_dest tree2
    set nodeName [lindex $node_source end]
    
    if {[dTree_nodeExists $tree1 "$node_source"]==0} {
        warning "Cannot copy branch $nodeName from $dtree_source "
        return 
    }
    
    
    
    if {[dTree_nodeExists $tree2 "$node_dest $nodeName"]} {
        dTree_rmBranch tree2 "$node_dest $nodeName"
    }
    dTree_addNode tree2 "$node_dest" $nodeName
    
    foreach attribute [dTree_getAttributes $tree1 $node_source] {
        dTree_setAttribute tree2 "$node_dest $nodeName" [lindex $attribute 0] [lindex $attribute 1]
    }
    
    foreach child [dTree_getChildren $tree1 $node_source] {
        dTree_copyBranch tree1 "$node_source $child" tree2 "$node_dest $nodeName"
    }
}

# to give to a node the same children as an other node
proc dTree_duplicateBranch {dtree_source node_source node_dest} {
    upvar $dtree_source tree
    
    set sce_path  [lrange $node_source 0 end-1 ]
    set sce_node  [lindex $node_source end ]
    set tgt_path  [lrange $node_dest 0 end-1 ]
    set tgt_node  [lindex $node_dest end ]
    
   
    set value [dTree_getAttribute $tree $node_dest "value" ]
              
    dTree_rmBranch tree "$tgt_path $tgt_node"
    dTree_addNode tree "$tgt_path"  $tgt_node
   
    
    foreach attribute [dTree_getAttributes $tree $node_source] {
        dTree_setAttribute tree $node_dest [lindex $attribute 0] [lindex $attribute 1]
    }
    dTree_setAttribute tree $node_dest "value" $value    
    
    
    foreach child [dTree_getChildren $tree $node_source] {
        dTree_copyBranch tree "$node_source $child" tree "$node_dest"
    }
}



proc dTree_copyRelevantBranch {dtree_source node_source dtree_dest node_dest} {
    # This function acts like copyBranch except, it chooses only used nodes (dtree_source should be tmpTree ...)
    upvar $dtree_source tree1
    upvar $dtree_dest tree2
    set nodeName [lindex $node_source end]
    if {[dTree_nodeExists $tree2 "$node_dest $nodeName"]} {
        dTree_rmBranch tree2 "$node_dest $nodeName"
    }
    
    dTree_addNode tree2 "$node_dest" $nodeName
    foreach attribute [dTree_getAttributes $tree1 $node_source] {
        dTree_setAttribute tree2 "$node_dest $nodeName" [lindex $attribute 0] [lindex $attribute 1]
    }
    
    foreach child [dTree_getUsedChildren $tree1 $node_source] {
        dTree_copyRelevantBranch tree1 "$node_source $child" tree2 "$node_dest $nodeName"
    }
}


proc dTree_copyRelevantBranch_ForpartialData {dtree_source node_source dtree_dest node_dest} {
    # This function acts like copyBranch except, it chooses only used nodes (dtree_source should be tmpTree ...)
    upvar $dtree_source tree1
    upvar $dtree_dest tree2
    set nodeName [lindex $node_source end]
    #set nodeFather1 "[lrange $node_source 0 end-1]"
    #set nodeFather2 "root [lrange $nodeFather1 2 end]"
    
    
    # $tree1 donnes a charger
    # $tree2 donnes cibles
    
    set node_source2 "$node_source"
    if {[string match "item_*" $nodeName ]} {
        set name1 "[dTree_getAttribute_fast $tree1 "$node_source" "value"]"
        foreach item [dTree_getChildren_fast  $tree2 $node_dest] {
            set name2 "[dTree_getAttribute_fast $tree2 "$node_dest $item" "value"]"
            if {$name1 == $name2} {
                set node_source2  "$node_dest $item"
            }
        }
        
    }
    set nodeName2 [lindex $node_source2 end]
    
    # Attributes update
    foreach attribute [dTree_getAttributes $tree1 $node_source] {
        dTree_setAttribute tree2 "$node_dest $nodeName2" [lindex $attribute 0] [lindex $attribute 1]
    }
    
    # Childs Update
    foreach child [dTree_getChildren_fast $tree1 $node_source] {
        dTree_copyRelevantBranch_ForpartialData tree1 "$node_source $child" tree2 "$node_dest $nodeName2"
    }
}





proc dTree_moveBranch {dtree_source node_source dtree_dest node_dest} {
    upvar $dtree_source tree1
    upvar $dtree_dest tree2
    dTree_copyBranch tree1 "$node_source" tree2 "$node_dest"
    dTree_rmBranch tree1 "$node_source"
}

proc dTree_searchAddress {dtree pattern} {
    set pattern "*[join [split $pattern { }] *]*"
    set addresses ""
    set result ""
    dict for {node subdict} $dtree {
        dict for {attr value} $subdict {
            lappend addresses "$node $attr"
        }
    }
    foreach address $addresses {
        if {[string match -nocase $pattern $address]} {
            lappend result $address
        }
    }
    
    return $result
}

proc dTree_searchNode {dtree pattern} {
    # Two types of pattern can be read here :
    #  -> One with no spaces, it's strict, the function will look for *pattern
    #  -> If spaces in the pattern, the function searches for all addresses which
    #     contain every element of the pattern in the order of the pattern
    
    if {[regexp {\s} $pattern]} {
        set pattern [split $pattern { }]
        set pattern "*[join [lrange $pattern 0 end-1] *]* [lindex $pattern end]"
    } else {
        set pattern "* [split $pattern {.}]"
    }
    set addresses ""
    set result ""
    dict for {node subdict} $dtree {
        lappend addresses "$node"
    }
    foreach address $addresses {
        if {[string match -nocase $pattern $address]} {
            lappend result $address
        }
    }
    return $result
}

proc dTree_searchNode_old {dtree pattern} {
    set pattern "*$pattern"
    set addresses ""
    set result ""
    dict for {node subdict} $dtree {
        lappend addresses "$node"
    }
    foreach address $addresses {
        if {[string match -nocase $pattern $address]} {
            lappend result $address
        }
    }
    return $result
}

proc dTree_nodeExists {tree key} {
    
    return [dict exists $tree $key]
    
}


proc dTree_attrExists_fast {tree key attribute} {
    if {[dict exists $tree "$key" $attribute]} {
        return 1
    } else {
        return 0
    }
}

proc dTree_attrExists {tree key attribute} {
    if {[dict exists $tree "$key" $attribute]} {
        return 1
    } else {
        if {[dict exists $tree "[dTree_cleanKey $key]" $attribute]} {
            return 1
        } else {
            return 0
        }
    }
}

proc dTree_puts {args} {
    set tree [lindex $args 0]
    set node [lindex $args 1]
    set indent [lindex $args 2]
    if {$node == ""} {set node root}
    puts "$indent ### [lindex $node end]"
    foreach attr [dTree_getAttributes $tree "$node"] {
        puts "$indent  > [lindex $attr 0]\t: [lindex $attr 1]"
    }
    set indent "$indent   "
    foreach child [dTree_getChildren_fast $tree "$node"] {
        dTree_puts $tree "$node $child" $indent
    }
}

proc dTree_compareBranch {tree1 tree2 node relevantNodes listDifferentNodes} {
    # This function compare two trees beginning from 'node'
    # If the trees are identical, it returns 1
    # If they are not, it returns 0 and the list of the nodes where the differences begin are stored in 'listDifferentNodes'
    # In listDifferentNodes, the first element stores the nodes with a difference in the node (arguments are different)
    # The second element stores the nodes existing in tree2 and not in tree1
    # The third element stores the nodes existing in tree1 and not in tree2
    # The fourth element stores the parents of the second and third ones (children are different)
    
    #Pour tester :
#    set l "{} {} {}"
#    puts [dTree_compareBranch $DStree $tmpTree "root" l]
    
#    foreach el [lindex $l 0] {puts "!= $el}    
#    foreach el [lindex $l 1] {puts "++ $el} 
#    foreach el [lindex $l 2] {puts "-- $el}     
    
    #debug "Comparing $node "    
    upvar $listDifferentNodes listNodes
    if {$listNodes == ""} {set listNodes "{} {} {} {}"}
    set status 1
    
    # Retrieve children list
    if {$relevantNodes != 1} {
        set children1 [lsort [dTree_getChildren $tree1 $node]]
        set children2 [lsort [dTree_getChildren $tree2 $node]]
    } else {
        set children1 [lsort [dTree_getUsedChildren $tree1 $node]]
        set children2 [lsort [dTree_getUsedChildren $tree2 $node]]        
    }
    
    # Retrieve arguments
    set attributes1 [lsort -index 0 [dTree_getAttributes $tree1 $node]]
    set attributes2 [lsort -index 0 [dTree_getAttributes $tree2 $node]]
    
    # Compare, if not the same, return false, no need to test the children ...
    if {$attributes1 != $attributes2 } {
        #debug "attributes1 : $attributes1"
        #debug "attributes2 : $attributes2"
        
        set tmpList [lindex $listNodes 0]
        lappend tmpList "$node"
        lset listNodes 0 $tmpList
        
        return 0
    }    
    
    if { $children1 != $children2 } {
 
        set childrenToTest ""
        
        set tmpList [lindex $listNodes 3]
        lappend tmpList "$node"
        lset listNodes 3 $tmpList        
        
        foreach child $children1 {
            if {$child ni $children2} {
                set tmpList [lindex $listNodes 2]
                lappend tmpList "$node $child"
                lset listNodes 2 $tmpList                
            } else {
                lappend childrenToTest $child
            }
        }
        
        foreach child $children2 {
            if {$child ni $children1} {
                set tmpList [lindex $listNodes 1]
                lappend tmpList "$node $child"
                lset listNodes 1 $tmpList     
                
                # lremove 
                set idx [lsearch -exact $childrenToTest $child]
                set childrenToTest [lreplace $childrenToTest $idx $idx]
            }
        }        
        
        foreach child $childrenToTest {
            lappend childrenStatus [dTree_compareBranch $tree1 $tree2 "$node $child" $relevantNodes listNodes]
        }        
        
        return 0    
    }
    
    # If no children and status is 1, comparison is OK, return is possible
    if {[llength $children1] == 0} {
        return 1
    } 
    
    # From now, nodes are the same and there are children (which are the same)
    set childrenStatus ""
    foreach child $children1 {
        lappend childrenStatus [dTree_compareBranch $tree1 $tree2 "$node $child" $relevantNodes listNodes]
    }
    
    
    return [expr min([join $childrenStatus ,])]
}

#proc saveTree_old {file tree} {
#    set fileId [open $file w]
#    puts $fileId $tree
#    close $fileId
#}

proc saveXMLnode {fileId tree currentSaveNode indentation} {
    set indentation "$indentation    "
    
    # Calculates list of children to know if there are some
    set childrenList [dTree_getChildren $tree $currentSaveNode]
    
    puts -nonewline $fileId "$indentation<[lindex $currentSaveNode end]"
    
    #foreach attribute write it
    foreach attribute [dTree_getAttributes $tree $currentSaveNode] {
        puts -nonewline $fileId " [lindex $attribute 0]=\"[lindex $attribute 1]\""
    }
    
    # makes a simple element if no children
    if {[llength $childrenList] == 0} {
        puts -nonewline $fileId " /"
    }
    puts $fileId ">"
    
    #write the XMLContent
    
    #foreach child, add indentation, write child
    foreach child $childrenList {
        set childNode "$currentSaveNode $child"
        saveXMLnode $fileId $tree $childNode $indentation
    }
    
    #close tag
    if {[llength $childrenList] > 0} {
        puts $fileId "$indentation</[lindex $currentSaveNode end]>"
    }
    
}



#proc loadTree_old {file itree} {
#    upvar $itree tree
#    set fileId [open $file r]
#    set result [read $fileId]
#    close $fileId
#    set tree $result
#    #dict for {node subdict} $tree {
#    #    dict for {attr value} $subdict {
#    #        debug "\ngenerate <<treeChange-[join $node {.}].$attr>>"
#    #        event generate . <<treeChange-[join $node {.}].$attr>>
#    #    }
#    #}
#    event generate . <<InitializeGUI>>
#}
#
#proc initializeNode {itree node} {
#    upvar $itree tree
#    global widgetInfo
#    foreach child [dTree_getChildren $tree [join [split $node "."] " "]] {
#        set address "[join $node .].$child"
#        
#        # Check if the child is a widget or a container
#        if {[array names widgetInfo -exact "$address-require"] == ""} {
#            #child is a container
#            event generate . <<treeChange-$address>>
#            initializeNode tree $address
#            
#        } else {
#            #child is a widget
#            if {$widgetInfo($address-require) == ""} {
#                #child is an independant widget
#                
#                event generate . <<treeChange-$address>>
#                initializeNode tree $address
#            } else {
#                initializeNode tree $widgetInfo($address-requirelist)
#                initializeNode tree $address
#            }
#        }
#        
#
#    }
#}
#
#proc loadXMLProject {file} {
#    global DStree
#    global metaTree
#    global loadedProject
#    global widgetInfo
#    global workingDir
#    dTree_init loadTree
#    dTree_init dummyTree    
#    
#    
#    
#}
#
#proc loadXMLDataset {file} {
#    
#}

proc xml2tree {file itree xmlType} {
    
    upvar $itree tree
    parseFile $file tree "" "DStree"    
}

proc csv2tree {file itree} {
    upvar $itree tree
    
    # open file
    set data [read [open $file]]
    set data [split $data \n]
    
    # Foreach line
    foreach line $data {
        
        set line [split $line ";"]
        if {[string trim [lindex $line 0]] == ""} { continue}
        # Create address
        set addressNode "root [join [split [lindex $line 0] {.}] { }]"
        
        # Read attribute
        set attr [join [lrange $line 1 end] ";"]
        
        # Create node
        if {[catch {dTree_addNode tree [lrange $addressNode 0 end-1] [lindex $addressNode end]} fid]} {
            error "Error reading the CSV file : Make sure all the parents of $addressNode have been set"
        }
        
        # Add attribute
        #if {$attr != ""} {
            dTree_setAttribute tree $addressNode "value" "$attr"
        #}
    }
}

proc tree2xml {file tree} {
    set fileId [open $file w]
    puts $fileId {<?xml version="1.0" encoding="UTF-8"?>}
    set indentation ""
    # save the tree under the root node
    set childrenList [dTree_getChildren $tree "root"]
    foreach child $childrenList {
        set childNode "root $child"
        saveXMLnode $fileId $tree $childNode $indentation
    }
    close $fileId
}

proc tree2csv {args} {
    if {[llength $args] == 3} {
        set file [lindex $args 0]
        set tree [lindex $args 1]
        set node [lindex $args 2]
    } elseif {[llength $args] == 2} {
        set file [lindex $args 0]
        set tree [lindex $args 1]
        set node ""       
    } else {
        error "Error with arguments ([llength $args]) : $args"
    }
    
    if {$node == ""} {
        # Begin the process
        set node root
        set fileId [open $file w]
        
        set childrenList [dTree_getChildren $tree "root"]
        foreach child $childrenList {
            set childNode "root $child"
            tree2csv $fileId $tree $childNode
        }
        
        close $fileId
    } else {
        ###### recursive process
        
        # Write node
        puts -nonewline $file "[join [lrange $node 1 end] {.}]"
        
        # Write attributes
        foreach attribute [dTree_getAttributes $tree $node] {
            puts -nonewline $file ";[lindex $attribute 1]"
        }
        puts $file ""
        
        # Write every child
        set childrenList [dTree_getChildren $tree "$node"]
        foreach child $childrenList {
            set childNode "$node $child"
            tree2csv $file $tree $childNode
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