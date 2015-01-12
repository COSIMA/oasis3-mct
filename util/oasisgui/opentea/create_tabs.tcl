#  This program is under CECILL_B licence. See footer for details.

proc tabs_create { args } {
    global widgetInfo
    
    set mandatory_arguments { path_father address}
    
    read_arguments
    
    
   
    ttk::notebook $path_father.nb 
    ttk::notebook::enableTraversal $path_father.nb

    bind $path_father.nb  <<NotebookTabChanged>> [subst {
        # get the tab under focus , finishing by .vport.form
        set widgetInfo(tabfocus)  \[$path_father.nb select\]
        smartpacker_regrid
        #scrollform_upperleft "\$widgetInfo(tabfocus).sf"
    }]
    # make the gui sensitive to SetView to regrid widgets in multicolumns mode
    bind . <<SetView>> +{tabs_update}
    
    
    bind $path_father.nb <Destroy> "tabs_destroy $path_father.nb"
    
    return $path_father.nb
}

proc tabs_add { args } {
    global widgetInfo codesPath solverPath
    set mandatory_arguments { path_father address}
    
    initWidget
    
    
    
    set solver ""
    set address_solver_XML ""
    foreach level $full_address_XML {
        lappend address_solver_XML $level
        set nodeType [dTree_getAttribute $XMLtree "$address_solver_XML" "nodeType"]
        if {$nodeType=="solver"} {
            set solver $level
        }
    }
    
    set widgetInfo($address-custombutton) [dTree_tryGetAttribute $XMLtree $full_address_XML "custombutton" "Process"]
    set widgetInfo($address-starttime) "0"
    set widgetInfo($address-endtime) "0"
    

    
    set widgetInfo($address-refreshStatus) [subst {
        tab_refreshStatus $path_father $solver $name $address 
    }]
    set title "[dTree_getAttribute $XMLtree $full_address_XML title]"
    
    if {[dTree_attrExists $XMLtree $full_address_XML "script"]} {
        set script [dTree_getAttribute $XMLtree $full_address_XML "script"]
        set args [split [dTree_tryGetAttribute $XMLtree $full_address_XML "script_args" ""] ";"]
        set widgetInfo($address-args) $args
        set widgetInfo($address-scriptAddress) [file join $solverPath scripts $script]
        if {![file exists $widgetInfo($address-scriptAddress)]} {
            error "File not found : \n$widgetInfo($address-scriptAddress)"
        }
    }
    
    lappend widgetInfo($path_father-tabnames) $name
    lappend widgetInfo($path_father-tab-$name-valid) 1
    $path_father add [ttk::frame $path_father.$name] -text $title -sticky news
    
    
   
   
    
    
    set widgetInfo($address-order) [$path_father index  $path_father.$name]
    
    ttk::separator $path_father.$name.sep1 -orient horizontal
    pack $path_father.$name.sep1 -side top -fill x 

    scrollform_create $path_father.$name.sf
    
    set winreturn [scrollform_interior $path_father.$name.sf]

    #add help
    set docu_prefered_side "top"
    set docu_prefered_place $winreturn
    help_add_desc_docu_to_widget
    
    
    ttk::separator $path_father.$name.sep2 -orient horizontal
    pack $path_father.$name.sep2 -side top -fill x
    
    ## PROCESS ##
    set cmd_widget_to_tree [subst {
        #update_all_widgets $address widget_to_tree
        set widgetInfo($address-status) 1
        eval \$widgetInfo($address-refreshStatus)
    }]
    
   
   
   
    
    ttk::button $path_father.$name.buttW2T -text $widgetInfo($address-custombutton) -command [subst {
        log "\n@@@@@@@@ Process Tab '$title' @@@@@@@"
        Validate $win $address
    }]
    pack $path_father.$name.buttW2T -side right -anchor e -padx 2 -pady 2
    
    ## CANCEL ##
    set cmd_tree_to_widget [subst {
        #update_all_widgets $address tree_to_widget
        RefreshFamily $address
    }]
    ttk::button $path_father.$name.buttT2W -text "Cancel" -command $cmd_tree_to_widget
    #pack $path_father.$name.buttT2W -side right -anchor e -padx 2 -pady 2
    
    ### SCRIPT IF NEEDED ####
    if {[info exists widgetInfo($address-scriptAddress)]} {
        set widgetInfo($address-progressValue) 0
        set widgetInfo($address-cancellation) 0
        ttk::progressbar $path_father.$name.progress  -mode indeterminate 
        ttk::label $path_father.$name.status -width -20 -textvariable widgetInfo($address-status_txt) -justify left -compound left
        $path_father.$name.progress stop
        pack $path_father.$name.progress -side right -anchor e -padx 2 -pady 2    
        pack $path_father.$name.status -side right -anchor e -padx 2 -pady 2           
        
    
        set widgetInfo($address-onScriptStarts) [subst {
                CheckValidationStatus $address
                UpdateValidationStatus $address
                if {\$widgetInfo($address-status) == -1} {
                    set widgetInfo($address-cancellation) 1
                    set widgetInfo($address-status_txt) "Status issue, Cancelled ..."
                    Stop_think normal
                } else {
                    set widgetInfo($address-cancellation) 0
                    $win.progress start
                    set widgetInfo(action-callingAddress) $address     
                    set widgetInfo($address-status_txt) "Running ..."
                }
        }]
        
        set widgetInfo($address-updateProgress) [subst {
                $win.progress stop            
                $win.progress configure -mode determinate -value \$widgetInfo($address-progressValue)
                            
        }]
            
        set widgetInfo($address-onScriptStops) [subst {
             $win.progress configure -mode indeterminate            
            $win.progress stop
            set widgetInfo($address-status) -1
            set widgetInfo($address-status_txt) "Execution error : check logs. Took  \[printtime \$widgetInfo($address-starttime) \$widgetInfo($address-endtime) \]  s."                    
            eval \$widgetInfo($address-refreshStatus)
        }]
        
        set widgetInfo($address-onScriptEnds) [subst {
             $win.progress configure -mode indeterminate             
            $win.progress stop
            set duration  \[format "%0.3f" \[expr {($widgetInfo($address-endtime)-\$widgetInfo($address-starttime))*0.001}  \]\]
            set widgetInfo($address-status_txt) "Done in   \[printtime \$widgetInfo($address-starttime) \$widgetInfo($address-endtime) \] s."                    
            if {\$widgetInfo($address-status) != -1} {
                # Umbrella backward  then upward
                CheckValidationStatus $address
                UpdateValidationStatus $address
            }
        }]
    
    }
    
    
    finishWidget
    
    lappend widgetInfo(form_to_update) $path_father.$name
    
    return $winreturn
}

proc tabs_update {}  {
    global widgetInfo
    
    if {"$widgetInfo(ReGrid)" == "no"} {
         set widgetInfo(ReGrid) [after 10 smartpacker_regrid ]
    } else {
        after cancel $widgetInfo(ReGrid)
        set widgetInfo(ReGrid) [after 10 smartpacker_regrid ]
    }
}
    

# Utilities 
proc tabs_destroy {win} {
    global widgetInfo
    foreach item [array names widgetInfo "$path_father-*"] {
        unset widgetInfo($item)
    }
}


proc tab_refreshStatus {path_father solver name tab_address} {
    global widgetInfo
    
    if {$widgetInfo($tab_address-visible)} {
        $path_father add $path_father.$name
    } else {
        $path_father hide $path_father.$name
    }
    
    switch $widgetInfo($tab_address-status) {
        "-2" {
            $path_father tab $path_father.$name -image icon_void -compound left        
        }
        "-1" {
            $path_father.$name.buttT2W configure -state normal
            $path_father.$name.buttW2T configure -state normal
            $path_father tab $path_father.$name -image icon_error -compound left        
        }
        "0"  {
            $path_father.$name.buttT2W configure -state normal
            $path_father.$name.buttW2T configure -state normal
            $path_father tab $path_father.$name -image icon_question -compound left        
        }
        "1"  {
            #$path_father.$name.buttT2W configure -state disabled
            #$path_father.$name.buttW2T configure -state disabled
            $path_father tab $path_father.$name -image icon_ok -compound left        
        }
    }
}

# trigger the update of all widget to tree
# all values presently stored in widgets are written into the tree
proc update_widget_to_tree {tab_address} {
    get_all_widgets $tab_address widget_to_tree
}


# trigger the update of the tree to all widget in this tab
# all values in the tree DIFFERENT than widget are broadcasted to widget
proc update_tree_to_widget {tab_address} {
    get_all_widgets $tab_address tree_to_widget
}


# starting from the tabs address
# query the list of all widgets address in the case tree (DStree)
# this list in the fomart .root.AVBP.tab1_mesh.mesh.file will be used
# in the command "proccess" to update the DS tree according all widgets.
proc update_all_widgets {tab_address mode} {
    global  widgetInfo
    set widgetInfo($tab_address-widgets) ""
    set address_XML [split $tab_address "." ]
    
    recc_get_childs $tab_address "$address_XML" $mode
}

# search for childs in the case tree.
# if the child have a value, then
# if the value differs between tree and widget,
# the address is stored in tabsInfo
proc recc_get_childs {tab_address address_XML mode} {
    global DStree widgetInfo widgetInfo
    set children [dTree_getChildren $DStree $address_XML]
    foreach child $children {
        set child_address_XML "$address_XML $child"
        set child_address  [join $child_address_XML "."]
        
        set isvalue [dTree_attrExists $DStree $child_address_XML "value" ]
        if {$isvalue} {
            lappend widgetInfo($tab_address-widgets) $child_address
        }
        
        set tree_value [dTree_getAttribute $DStree "$child_address_XML" "value"]
        if {$tree_value != $widgetInfo($child_address-variable) && $widgetInfo($child_address-validateMode) == "onProcess"} {
            switch $mode {
                "tree_to_widget" {
                    response_queue Cancel $tab_address treeChange $child_address
                    event generate . <<treeChange-$child_address>>
                }
                "widget_to_tree" {
                    response_queue Process $tab_address widgetValidate $child_address
                    event generate . <<widgetValidate-$child_address>>
                }
            }
        }
        recc_get_childs $tab_address "$child_address_XML" $mode
    }
    
    
}



proc tabs_docu {address chin help_dir} {

    global widgetInfo XMLtree     
    set full_address_XML [split $address "."]
    
    set title [dTree_getAttribute $XMLtree $full_address_XML "title"]
    puts $chin "<h2> Tab. $title </h2>"
    
    return
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