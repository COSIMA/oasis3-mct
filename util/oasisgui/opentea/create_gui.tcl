#  This program is under CECILL_B licence. See footer for details.




# hello world

# Create a GUI associated to a tree 
proc gui_create { args } {
    global widgetInfo
    global XMLtree
    global initializedSolver
    global solverPath
    global libraryPath
    global solverOrder
    
    set address "root"
    set initializedSolver ""
    
    # An initialization is mandatory for the root node but the initWidget doesn't fit requirements for the root node
    set widgetInfo($address-require) ""
    set widgetInfo($address-status) 0
    set widgetInfo($address-check) ""
    set widgetInfo($address-refresh) ""
    set widgetInfo($address-refreshStatus) ""
    
    # loop on keywords provided
    foreach { keyword argument } $args {
        # test keywords
        if {![string match -* $keyword ]} {
            tk_messageBox -icon error -message "Error : keyword must be like -*"
            return
        }
        # assign keywords
        switch -- $keyword {
            "-path" {
               set win $argument
            }
            "-solver" {
               set solver $argument
            }
            default {
                tk_messageBox -icon error -message  "Error : $keyword is not recognized"
                return
            }
        }
    }
    set widgetInfo($address-refreshStatus) ""
    
    set win_GUI [ solverframe_create -path_father $win -address "root"]
   
    # ensure that destroying the GUI frame will wipe out its associated memory
    bind $win_GUI <Destroy> "gui_destroy $solver"
    
    # start the recursive creation of widgets
    
    ######## Sort the children according to their "order" attribute
    
    # First, get children and associated orders
    foreach child [dTree_getChildren $XMLtree "root"]  {
        set order "0"
        set childAndOrder ""
        # Retrieve order from either dict solver order (set solvers_to_add) or XML if absent
        if { $child in [array names solverOrder]} {
            set order $solverOrder($child)
        } else {
            set order [dTree_tryGetAttribute $XMLtree "root $child" order "9999"]
        }
        lappend childAndOrder $order
        lappend childAndOrder $child
        lappend childrenAndOrder $childAndOrder
    }
    
    # Then, sort
    set childrenAndOrder [lsort -index 0 -integer $childrenAndOrder]
    # Finally, store them
    foreach child $childrenAndOrder {
        lappend sortedChildren [lindex $child 1]
    }

    ############ Loop over children
    foreach solv $sortedChildren {
        if {$solv == "DATA"} {
            continue
        }
        set initializedSolver $solv
        set solverPath [file join $libraryPath $solv]
        gui_addpart -address root.$solv -path_father $win_GUI -class solver
    }
    
    
    return $win_GUI
}

proc gui_addpart { args } {
    global GuiInfo
    global DStree
    global XMLtree
    global additionalWidgets
    
    # set style . If style flat, simple frames will be used instead of labelframes
    # used for labelframed widgets in modelxor or multiple
    set part_style "normal"    
    
    # loop on keywords provided
    foreach { keyword argument } $args {
        # test keywords
        if {![string match -* $keyword ]} {
            tk_messageBox -icon error -message "Error : keyword must be like -*"
            return
        }
        # assign keywords
        switch -- $keyword {
            "-address" {
                set address $argument
                set full_address_XML [split $argument "."]
                set node_XML [lindex $full_address_XML end ]
                set parent_address_XML [lrange $full_address_XML 0 end-1 ]
                set parent_address [join $parent_address_XML .]
            }
            "-path_father" {
               set path_father $argument
            }
            "-class" {
                set part_class $argument
            }
            "-style" {
                set part_style $argument
            }
            
            default {
                tk_messageBox -icon error -message  "Error : $keyword is not recognized"
                return
            }
        }
    }
        

    switch -glob $part_class {
        "solver" {
            set part_winactive [solverframe_add -path_father $path_father -address $address]
        }
        "tab" {
            set part_winactive [tabs_add -path_father $path_father -address $address]
        }
        "model" {
            set part_winactive [modelframe_create -path_father $path_father -address $address -style $part_style]
        }
        "multiple" {
            set part_winactive [multiple_create -path_father $path_father -address $address]
        }
        "xor" {
            set part_winactive [modelxor_create -path_father $path_father -address $address -style $part_style]
        }
        "action" {
            set part_winactive [action_create -path_father $path_father -address $address -style $part_style]
        }
        "info" {
            set part_winactive [info_create -path_father $path_father -address $address]
        }
        "choice" {
            set part_winactive [choice_create -path_father $path_father -address $address]
        }
        "comment" {
            set part_winactive [comment_create -path_father $path_father -address $address]
        }
        "comparator" {
            set part_winactive [comparator_create -path_father $path_father -address $address]
        }
        "status" {
            set part_winactive [status_create -path_father $path_father -address $address]
        }
        "graph" {
            set part_winactive [graph_create -path_father $path_father -address $address]
        }
        "glance" {
            set part_winactive [glance_create -path_father $path_father -address $address]
        }
        "timeline" {
            set part_winactive [timeline_create -path_father $path_father -address $address]
        }
        "option" {
            set part_winactive [choice_add -path_father $path_father -address $address]
        }
        "param" {
            set param_type [dTree_getAttribute $XMLtree  $full_address_XML "type"]
            
            switch -glob $param_type {
                "onoff" {
                    set part_winactive [switch_create -path_father $path_father -address $address]
                }
                "double*" -
                "integer*" -
                "fraction" -
                "date" -
                "complex" -
                "string*" -
                "ascii*" -
                "vector" -
                "liststring" {
                    set part_winactive [entry_create -path_father $path_father -address $address]
                }
                "list_*" {
                    set part_winactive [cluster_create -path_father $path_father -address $address]
                }
                "selection" {
                    set part_winactive [selection_create -path_father $path_father -address $address]
                }
                "file" -
                "multiple_files" -
                "h5_asciiBound" -
                "folder" {
                    set part_winactive [browser_create -path_father $path_father -address $address]
                }
                "speccompo" {
                    error "speccompo is not supported anymore"
                 #   set part_winactive [speccompo_create -path_father $path_father -address $address]
                }
                "labelimage" {
                    error "labelimage is not supported anymore"
                    #set part_winactive [labelimage_create -path_father $path_father -address $address]
                }
                default {
                    set widget_title [dTree_getAttribute $XMLtree $full_address_XML "title"]
                    set widget_status "GuiInfo($address-valid)"
                    set $widget_status 0
            
                    set part_winactive  $path_father.$node_XML
                    ttk::label $part_winactive -text "$widget_title ($param_type)"
                    pack $part_winactive
                }
            }
        }
        "viewer" {
            set part_winactive [viewer_create -path_father $path_father -address $address]
        }
        "view3d" {
            set part_winactive [viewer3d_create -path_father $path_father -address $address]
        }
        "view3d_source" {
            set part_winactive [viewer3d_add_source -path_father $path_father -address $address]
        }
        default {
            if {$part_class in $additionalWidgets} {
                set part_winactive [$part_class\_create -path_father $path_father -address $address]
            } else {
                debug "part_class Unknown : $part_class ; $address"
                set part_winactive "unknown"
            }
        }
    }
    
    # recursive GUI generation
    
    set GuiInfo($address-children) [dTree_getChildren $XMLtree $full_address_XML]
    
    set sortedChildren ""
    set childrenAndOrder ""
    
    ######## Sort the children according to their "order" attribute
    
    # First, get children and associated orders
    foreach child $GuiInfo($address-children) {
        set order "0"
        set childAndOrder ""
        # Retrieve order from attributes of the child
        set order [dTree_tryGetAttribute $XMLtree "$full_address_XML $child" order "9999"]
        lappend childAndOrder $order
        lappend childAndOrder $child
        lappend childrenAndOrder $childAndOrder
    }
    
    # Then, sort
    set childrenAndOrder [lsort -index 0 -integer $childrenAndOrder]
    
    # Finally, store them
    foreach child $childrenAndOrder {
        lappend sortedChildren [lindex $child 1]
    }
    
    set GuiInfo($address-children) $sortedChildren
    
    if {$part_class != "xor" && $part_class != "multiple"  && $part_class != "choice"} {
        foreach child $GuiInfo($address-children) {
            # Case of ";" type nodes (options)
            if {[string match "*;*" $child]} {
                set XML_node_address_child "$full_address_XML \{$child\}"
            } else {
                set XML_node_address_child [concat $full_address_XML $child]
            }
            
            if {[catch {set widget_child_class "[dTree_getAttribute $XMLtree $XML_node_address_child nodeType]"}]} {
                warning "WARNING : $XML_node_address_child doesn't have a node type ...\n Setting it to container ..."
                set widget_child_class "container"
            }
            gui_addpart -address $address.$child  -path_father $part_winactive -class $widget_child_class
        }
    }
    
    
    return $part_winactive
}




proc gui_destroy {win} {
    global GuiInfo
    foreach item [array names GuiInfo "$win-*"] {
        unset GuiInfo($item)
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