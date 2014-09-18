#  This program is under CECILL_B licence. See footer for details.

proc cluster_create { args } {
    set mandatory_arguments { path_father address }
    
    # Initializes the widget
    initWidget

    set widgetInfo($address-status) 1
    set widgetInfo($address-renaming) 0
    set widgetInfo($address-lastclick) [clock milliseconds]
    set widgetInfo($address-ratio) [dTree_tryGetAttribute $XMLtree $full_address_XML_clean "ratio" "0.5" ] 
     
            
    if  {[dTree_attrExists $XMLtree $full_address_XML_clean "require"]==1} {
        set widgetInfo($address-clustermode) "require"
    } else {
        if  {[dTree_attrExists $XMLtree $full_address_XML_clean "fixed"]==1} {
            set widgetInfo($address-clustermode) "fixed"            
        } else {
            set widgetInfo($address-clustermode) "independant"
        }
    }
    
    
    
    ###############
    # MAIN FRAMES #
    ###############
    
    ttk::frame $win
    eval $widgetInfo(packme-$win)
    
    
    
    
    ttk::frame $win.ftv 
    ttk::frame $win.ftv.forceps -width $widgetInfo(guiSmallWidgetWidth) -height 0 
    pack $win.ftv -fill both -side left -expand 1 
    ############
    # TREEVIEW #
    ############
    
    set title [dTree_getAttribute $XMLtree $full_address_XML_clean "title"]   
    ttk::label $win.ftv.lb  -text "$title:" -anchor ne -justify right
    
    set widgetInfo($address-status) 0
    set widgetInfo($address-status_txt) ""
    ttk::label $win.ftv.status -textvariable widgetInfo($address-status_txt) -foreground "red" -justify center -compound left -anchor center
    $win.ftv.status configure -wraplength [expr { 0.6*$widgetInfo(guiSmallWidgetWidth)}]
    
    
    set widgetInfo($address-labelstype) [dTree_tryGetAttribute $XMLtree $full_address_XML_clean "labels" "ascii"]
    
    ttk::treeview $win.ftv.tv -selectmode none -show {}
    if {$widgetInfo($address-clustermode) == "independant"} {
        $win.ftv.tv configure -selectmode browse
    }
    #yscrollcommand [list $win.ftv.sby set]  
    # create the columns of the treeview    
    
    
    #$win.ftv.lb configure -width "[expr {int([expr {1.* $widgetInfo(guiSmallWidgetWidth)}])}]p"
    set colwidth [expr {int([expr {0.5* $widgetInfo(guiSmallWidgetWidth)}])}]
    
    
    $win.ftv.tv configure -columns "lbl var"
    $win.ftv.tv column #1 -width $colwidth -anchor e
    $win.ftv.tv column #2 -width $colwidth -anchor w
    
    #ttk::scrollbar $win.ftv.sby -orient vertical -command [list $win.ftv.tv yview]
    
    # avoid tab scrolling for mousewheel
    bind $win.ftv.tv <MouseWheel> {+set tabscroll 0}
    bind $win.ftv.tv  <Leave> {+set tabscroll 1}
    #bind $win.ftv.tv  <FocusOut> [subst {$win.ftv.tv selection set  {} }]
    # ce binding est inutilisable ca il bloque la deletion d'un item
    
    
    grid $win.ftv.lb -sticky ne -column 0 -row 0 
    grid columnconfigure  $win.ftv  $win.ftv.lb  -weight 1
    grid rowconfigure  $win.ftv  $win.ftv.lb  -weight 1
    grid $win.ftv.tv -sticky e -column 1 -row 0 -rowspan 2
    grid $win.ftv.forceps -sticky news -column 0 -row 3 -columnspan 2
    grid $win.ftv.status -sticky news -column 0 -row 4 -columnspan 2
    
    
    
 
    

    
    
    ############
    # CONTROLS #
    ############
    ttk::frame $win.ftv.controls
    grid $win.ftv.controls -sticky se -column 0 -row 1
    grid columnconfigure  $win.ftv  $win.ftv.controls  -weight 1
    grid rowconfigure  $win.ftv  $win.ftv.controls  -weight 1
    
    
    #icons with buttons
    ttk::button $win.ftv.controls.add -text "+" -command [subst {
        cluster_ctrl_addcomponent $win $address
        eval \$widgetInfo($address-check)   
    }]
    ttk::button $win.ftv.controls.rm  -text "-" -command [subst {
        cluster_ctrl_rmcomponent $win $address
        eval \$widgetInfo($address-check)   
    }]
    
    
    
    if {$widgetInfo($address-clustermode)=="independant"} {
       pack $win.ftv.controls.add $win.ftv.controls.rm -side right -anchor nw -padx 0 -pady 0      
    }
      
   
   
    append widgetInfo($address-refreshStatus) [subst {cluster_refreshStatus $win $address}]
    append widgetInfo($address-refresh) [subst {cluster_refresh $win $address}]
    
    
    finishWidget
    
    cluster_refresh $win $address
   
    
    # reaction to cells
    bind $win.ftv.tv <ButtonPress> [subst {+cluster_simple_trigger $win $address %x %y}]
    
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
}


# Simple Click : raise the form relative to the component
proc cluster_simple_trigger {win address x y} {
    global widgetInfo
    set row [$win.ftv.tv identify row $x $y]
    set col [$win.ftv.tv identify column $x $y]
    
    set double_click "0"
    set current_time_ms [clock milliseconds]
    set delay [expr {$current_time_ms - $widgetInfo($address-lastclick)}]
    set widgetInfo($address-lastclick) $current_time_ms
    if {$delay <300 } {
        set double_click 1
    }
    
    
    if {$col=="#1" && $widgetInfo($address-clustermode)!="independant"} { return 0}    
    if {$row!= ""} {
        
        if { $widgetInfo($address-renaming) != 0 } {
            cluster_setname $address $win $widgetInfo($address-renaming) $widgetInfo($address-renamingCol)
            set widgetInfo($address-renaming) 0
            destroy $win.ftv.tv.rename  
        }
        
        if {$double_click} {
            cluster_rename $win $address $row $col
            
        }
        
    }
}




# called by the "+" button is invoked
proc cluster_ctrl_addcomponent {win address} {
    global widgetInfo tmpTree
    
    set row [$win.ftv.tv selection ]
    if { $row != "" } {
        set rank [lsearch [cluster_component_list $address] $row]
        incr rank
    } else {
        set rank "end"
    }
    cluster_new_component $win $address $rank
    cluster_refreshStatus $win $address
}

proc cluster_ctrl_rmcomponent {win address } {
    global widgetInfo
     
    
    set row [$win.ftv.tv selection ]
    if { $row != "" } {
        set rank [lsearch [cluster_component_list $address] $row]
    } else {
        return
    }
    cluster_del_component $win $address $row
    cluster_refreshStatus $win $address
}



# called when :
# - "+" button is invoked
# - a new component is needed
proc cluster_new_component { win address rank   } {
    global widgetInfo
    set index 1
    set lbl_ok 0
    while {$lbl_ok == 0} {
        
        set rank [lsearch [cluster_component_list $address] "x_$index"]

        if {$rank == -1} {
            set lbl_ok 1
        } else {
            incr index
        }
    }
    set newcontent [ linsert [cluster_variable_to_content $address] end "x_$index void"]
    cluster_content_to_variable $address $newcontent
}


# called when :
# - "-" button is invoked
# - a  component must be deleted
proc cluster_del_component { win address component_node } {
    global widgetInfo 
    
    set rank [lsearch [cluster_component_list $address] $component_node]
    # remove from the main cluster widget
    
    
    
    set newcontent [ lreplace [cluster_variable_to_content $address] $rank $rank  ]
    cluster_content_to_variable $address $newcontent
   
}



proc cluster_rename {win address row col } {
    
    global widgetInfo
    if { $widgetInfo($address-renaming) != 0 } {
        focus $win.ftv.tv.rename.entry
        return    
    }                    
    
    set widgetInfo($address-renaming) $row                    
    set widgetInfo($address-renamingCol) $col
    

    
    
    frame $win.ftv.tv.rename -background red  -bd 2
    set bbox [$win.ftv.tv bbox $row $col]
    
    place $win.ftv.tv.rename -x [lindex $bbox 0] -y [lindex $bbox 1] -width [lindex $bbox 2] -height [lindex $bbox 3]
    
    focus $win.ftv.tv.rename
    
    set widgetInfo($address-entry) [$win.ftv.tv set $row $col]
    ttk::entry $win.ftv.tv.rename.entry -textvariable widgetInfo($address-entry) 
    pack $win.ftv.tv.rename.entry -expand 1
    $win.ftv.tv.rename.entry selection range 0 end
    $win.ftv.tv.rename.entry icursor end
    focus $win.ftv.tv.rename.entry


    bind $win.ftv.tv.rename.entry <Return>  [ subst {
        # change value
        cluster_setname $address $win $row $col
        # kill dialog
        destroy $win.ftv.tv.rename
        set widgetInfo($address-renaming) 0
    }]
    
    bind $win.ftv.tv.rename.entry <FocusOut>  [ subst {
         # change value
        cluster_setname $address $win $row $col
        # kill dialog
        destroy $win.ftv.tv.rename
        set widgetInfo($address-renaming) 0
    }]
    
    
    bind $win.ftv.tv.rename.entry <Escape>  [ subst {
        # kill dialog
        destroy $win.ftv.tv.rename
        set widgetInfo($address-renaming) 0
    }]
    
    
    #bind $win.ftv.tv.rename.entry <Tab>  [ subst {
    #    
    #    #change value
    #    cluster_setname $address $win $row $col
    #    # kill dialog
    #    destroy $win.ftv.tv.rename
    #    set widgetInfo($address-renaming) 0
    #    
    #    # find nex row
    #    set list_row \[cluster_component_list $address\]
    #    set rank \[lsearch \$list_row $row\]
    #    incr rank
    #    if {\$rank == \[llength \$list_row\]} {
    #        set rank 0
    #    }
    #    set newrow \[lindex \$list_row \$rank\]
    #    # restart dialog
    #    cluster_rename $win $address \$newrow $col
    #    puts "test"
    #}]
}



# change the label assigned 
proc cluster_setname { address win row col } {
    global widgetInfo tmpTree
    
    
   
    set new_label [stringcleanBalise $widgetInfo($address-entry)]
    set newcontent ""
    
    
    if {$col == "#1"} {
        set list_of_components [cluster_component_list $address]
       
        if {[lsearch $list_of_components $new_label ] != -1} {
            warning "The component $new_label is already existing. Use another component."
            return
        }   
    }
    
    foreach couple  [cluster_variable_to_content $address] {
        set component [lindex $couple 0]
        set vars [lrange  $couple 1 end]
        if {$component ==$row } {
            switch $col {
                "#1" {
                    set couple "$new_label $vars"
                }
                "#2" {
                    set couple "$component $new_label"
                }
            }
        }
        lappend newcontent $couple
    }
    
    cluster_content_to_variable $address $newcontent
    eval $widgetInfo($address-check)
}




proc cluster_refresh {win address} {
    global widgetInfo XMLtree
    # Reinitialize the treeview
    
    if {$widgetInfo($address-clustermode) =="require"} {
        cluster_require $win $address
    }
}


proc cluster_refreshStatus {win address} {
    global widgetInfo XMLtree
    # Reinitialize the treeview
    
  
    
    
    $win.ftv.tv delete [$win.ftv.tv children {}]
    
    set compolenght 0
    # create the lines of the treeview
    set widgetInfo($address-status_txt) ""
    set widgetInfo($address-status) 1
    
    set full_address_XML [split $address "."]
    set full_type [dTree_getAttribute $XMLtree $full_address_XML "type"]
    set type [string range $full_type  5 end ]
    
    set sum 0    
    
    foreach couple [cluster_variable_to_content $address]  {
        set component [lindex  $couple 0]
        set var [lrange  $couple 1 end]

        set cl [string length $component]
        if {$cl>$compolenght} {set compolenght $cl}
             
        set test_label [test_vartype $component $widgetInfo($address-labelstype) ]       
        set test_cluster [test_vartype $var $type ]
        if { $test_cluster == 1 && $test_label == 1} {
            $win.ftv.tv insert {} end -id $component -text "$component" -image "" -tags "true"
        } else {
            
            if { $test_cluster != 1} {
                 set widgetInfo($address-status_txt) $test_cluster
            }
           
            if { $test_label != 1} {
                 set widgetInfo($address-status_txt) $test_label
            }
            
            set widgetInfo($address-status) -1
            $win.ftv.tv insert {} end -id $component -text "$component" -image "" -tags "false"
        }
        
        if {$type == "fraction" && [string is double $var]} {
            set sum [expr {$sum + $var}] 
        }
    
        
        $win.ftv.tv set $component #1 "$component"
        $win.ftv.tv set $component #2 $var
    }
    
    if {$type == "fraction"} {
        if {$sum != 1.0 } {
            set test_cluster "sum fractions not one"
            set widgetInfo($address-status_txt) $test_cluster
            set widgetInfo($address-status) -1
        }
    }
    
    
    $win.ftv.tv tag configure false -background #ff5050
    $win.ftv.tv tag configure unknown -background #ffcd7c
    $win.ftv.tv tag configure true -background ""
    
    set colwidth1 [expr {int([expr {0.5* $widgetInfo(guiSmallWidgetWidth)  *$widgetInfo($address-ratio)  }])}]
    set colwidth2 [expr {int([expr {0.5* $widgetInfo(guiSmallWidgetWidth)}]) - $colwidth1}]
    $win.ftv.tv column #1 -width $colwidth1 -anchor w
    $win.ftv.tv column #2 -width $colwidth2 -anchor w
    
    
    set nrows  [llength [cluster_variable_to_content $address] ]
    
    $win.ftv.tv configure -height $nrows
    
    if { $widgetInfo($address-status) == -1 } { 
       $win.ftv.status configure -image icon_flag
    } else {
        $win.ftv.status configure -image ""
    }
    smartpacker_update_visibility $win $address
    
    
}






proc cluster_content_to_variable {address content} {
    global widgetInfo
    set buffer ""
    foreach couple $content {
        lappend buffer [lindex  $couple 0]
        lappend buffer [lrange  $couple 1 end]
    }
    set widgetInfo($address-variable) [join $buffer ";" ]
}


proc cluster_variable_to_content {address} {
    global widgetInfo
    set couple ""
    set content ""
    foreach item [split $widgetInfo($address-variable)  ";" ] {
        set couple "$couple $item"
        if {[llength $couple] == 2 } {
            lappend content $couple
            set couple ""
        }
    }
    return $content
}

proc cluster_component_list {address} {
    global widgetInfo
    set list_component ""
    foreach couple [cluster_variable_to_content $address] {
        set component [lindex $couple 0]
        lappend list_component $component
    }
    return $list_component
}

proc cluster_var_list {address} {
    global widgetInfo
    set list_vars ""
    foreach couple [cluster_variable_to_content $address] {
        set vars [lrange  $couple 1 end]
        lappend list_vars $vars
    }
    return $list_vars
}






# prepare the filling in a different manner if a require is waiting
proc cluster_require { win address } {
    global widgetInfo
    set reqVar $widgetInfo($address-requiredValue) 
    set default [lindex [split $widgetInfo($address-default) ";"]  1]
    if {$default==""} {set default 0 }
    set old_content [cluster_variable_to_content $address]
    set new_content ""       
    # get the present values of the components if they belong to reQvar   
    foreach actual_component $reqVar {
        set component_found 0
        foreach couple $old_content {
            set component [lindex $couple 0]
            set var [lrange  $couple 1 end]
            if {$component == $actual_component} {
                lappend new_content "$component $var"
                set component_found 1
            }
        }
        if {$component_found==0 } {lappend new_content "$actual_component $default"}
    }
    
    
    
    if {$new_content != $old_content} {
       cluster_content_to_variable $address $new_content
    } 
    
    # hide if void list
    if {[llength $new_content] == 0} {
        pack forget $win
        return
    } else {
        eval $widgetInfo(packme-$win)
    }
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