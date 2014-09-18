#  This program is under CECILL_B licence. See footer for details.

proc genPyFromXMLtree {} {
    global XMLtree loadApplication
    
    if {[catch {
       genPyOutput "from XDR import *\n"
       genPyOutput "###########################"
       genPyOutput "# INITIALIZATION\n"
       genPyOutput "init()\n"
        
       genPyFromNode "root $loadApplication" "" "0 0" ""
        
       genPyOutput "\n\n#########################\n# FINISHING"
       genPyOutput "finish()"
    } err err2]} { 
        genPyOutput "[lindex $err2 7]"
    }
}

proc genPyOutput {txt} {
    puts $txt
} 

proc genPyConstructPattern {node scheme multiples} {
    global XMLtree
    
    set l [llength $scheme]    
    # Activating one part at a time on the address descriptor until descriptor is unique
    for {set i 1} {$i < $l} {incr i} {
        
        # Pattern construction
        set pattern ""
        for {set j 0} {$j < $l} {incr j} {
            if {[lindex $scheme $j] == 1} {
                lappend pattern [lindex $node $j]
            }
        }
        set pattern [join $pattern " "]
        
        if {[llength [dTree_searchNode $XMLtree $pattern]] == 1} {
            break
        }
        set nextId [expr {$l-$i-1}]
        if {[lsearch $multiples $nextId] > -1} {
            lset scheme $nextId 0
        } else {
            lset scheme $nextId 1
        }
    }
    set pythonArgs ""    
    foreach elt [split $pattern " "] {
        # If multiple , qqch !
        set pythonArgs [lreplace $pythonArgs 0 -1 "\"$elt\""]
    }
    
    foreach mul $multiples {
        lappend pythonArgs "multiple${mul}"
    }
    return $pythonArgs
}

proc genPyFromNode {node indent mandatory multiples} {
    global XMLtree 
    
    set newIndent ""
    set childNotDone 1
    
    #set attrs [dTree_getAttributes $XMLtree $node]
    if {[catch {set type [dTree_getAttribute $XMLtree $node "nodeType"]} err]} {
        log $err
        set type "undefined" 
    }
    
    set name [lindex $node end]
    
    
    
    # Dealing with existifs
    if {[dTree_attrExists $XMLtree $node "existif"]} {
        set scheme $mandatory
        lset scheme end 1        
        set pythonArgs [genPyConstructPattern $node $scheme $multiples]        
        genPyOutput "${indent}if nodeExists([join $pythonArgs {,}]):"
        genPyOutput "${indent}   pass"
        set indent "${indent}   "
    }
    
    #genPyOutput "${indent}[lindex $node end] is a $type"
    
    switch $type {
        tab {
            genPyOutput "\n\n${indent}###########################"
            genPyOutput "${indent}# Tab : $name "
            genPyOutput "${indent}##################\n"            
        }
        
        model {
            genPyOutput "${indent}# Model : $name"
        }
        
        
        choice -
        param {
            # Construct list of params
            set scheme $mandatory
            lset scheme end 1
#            genPyOutput "---------  $node"
#            genPyOutput "---------  $scheme"
            set pythonArgs [genPyConstructPattern $node $scheme $multiples]                        
                        
            genPyOutput "${indent}print getValue([join $pythonArgs {,}])"
        }
        
        xor {
            set childNotDone 0
            set scheme $mandatory
            lset scheme end 1            
            set pythonArgs [genPyConstructPattern $node $scheme $multiples]            
            foreach child [dTree_getChildren $XMLtree $node] {
                genPyOutput "${indent}if getValue([join $pythonArgs {,}]) == \"$child\" :\n${indent}   pass"
                genPyFromNode "$node $child" "${indent}   " "$mandatory 0" "$multiples"
            }            
        }
        
        multiple {
            set scheme $mandatory
            lset scheme end 1            
            set pythonArgs [genPyConstructPattern $node $scheme $multiples]
            genPyOutput "${indent}for multiple[llength $node] in getChildrenName([join $pythonArgs {,}]):"            
            set newIndent "   "
            lappend multiples [llength $node]

        }
        
        item {
            # Do nothing
        }
        action -
        solver -
        option -
        info {
            # Do nothing 
        }
        
        
    }
    if {$childNotDone} {
        foreach child [dTree_getChildren $XMLtree $node] {
            genPyFromNode "$node $child" "${indent}$newIndent" "$mandatory 0" "$multiples"
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