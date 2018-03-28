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
    
    frame $win.ftv.border  -bd 0 -highlightthickness 2 -background [ThemeColor 1.0] -highlightbackground [ThemeColor 1.0] -highlightcolor [FocusColor]
    bind . <<ThemeUpdate>> +[subst {$win.ftv.border configure -background \[ThemeColor 1.0\] -highlightbackground \[ThemeColor 1.0\] -highlightcolor \[FocusColor\] }]
    #pack $win.ftv.border
    
    
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
    
    ttk::treeview $win.ftv.border.tv -selectmode none -show {}
    if {$widgetInfo($address-clustermode) == "independant"} {
        $win.ftv.border.tv configure -selectmode browse
    }
    #yscrollcommand [list $win.ftv.sby set]  
    # create the columns of the treeview    
    
    
    #$win.ftv.lb configure -width "[expr {int([expr {1.* $widgetInfo(guiSmallWidgetWidth)}])}]p"
    set colwidth [expr {int([expr {0.5* $widgetInfo(guiSmallWidgetWidth)}])}]
    
    
    $win.ftv.border.tv configure -columns "lbl var"
    $win.ftv.border.tv column #1 -width $colwidth -anchor e
    $win.ftv.border.tv column #2 -width $colwidth -anchor w
    
    #ttk::scrollbar $win.ftv.sby -orient vertical -command [list $win.ftv.border.tv yview]
    
    # avoid tab scrolling for mousewheel
    bind $win.ftv.border.tv <MouseWheel> {+set tabscroll 0}
    bind $win.ftv.border.tv  <Leave> {+set tabscroll 1}
    #bind $win.ftv.border.tv  <FocusOut> [subst {$win.ftv.border.tv selection set  {} }]
    # ce binding est inutilisable ca il bloque la deletion d'un item
    
    
    grid $win.ftv.lb -sticky ne -column 0 -row 0 
    grid columnconfigure  $win.ftv  $win.ftv.lb  -weight 1
    grid rowconfigure  $win.ftv  $win.ftv.lb  -weight 1
    grid $win.ftv.border -sticky e -column 1 -row 0 -rowspan 2
    pack $win.ftv.border.tv
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
    #bind $win.ftv.border.tv <ButtonPress> [subst {+cluster_simple_trigger $win $address %x %y}]
    
    #bind $win.ftv.border.tv <Double-Button-1> [subst {
    #    cluster_focus $win $address "on"
    #    cluster_double_trigger $win $address %x %y
    #    }] 
    #
    bind $win.ftv.border.tv <ButtonPress-1> [subst {cluster_click $win $address %x %y}]
        
    bind $win.ftv.border.tv <ButtonPress-2> [subst {
        cluster_focus $win $address "on"
        cluster_context_menu $win $address %x %y %X %Y
    }] 
   
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
}

proc cluster_focus {win address onoff} {
    global widgetInfo
    switch $onoff {
        "on" {
            focus $win.ftv
            update
        }
        default {
            
            
        }
    }
    
}




proc cluster_click { win address x y} {
    global widgetInfo
    set t [clock milliseconds]
    cluster_focus $win $address "on"
    set dt [expr {$t-$widgetInfo($address-lastclick)}]    
    if { $dt < 500} {
        cluster_double_trigger $win $address $x $y
    } else {
        cluster_simple_trigger $win $address $x $y
    }
    set widgetInfo($address-lastclick) $t
}


# Simple Click : raise the form relative to the component
proc cluster_simple_trigger {win address x y} {
    global widgetInfo
    set row [$win.ftv.border.tv identify row $x $y]
    set col [$win.ftv.border.tv identify column $x $y]
    if {$col=="#1" && $widgetInfo($address-clustermode)!="independant"} { return 0}    
    if {$row!= ""} {
        if { $widgetInfo($address-renaming) != 0 } {
            cluster_setname $address $win $widgetInfo($address-renaming) $widgetInfo($address-renamingCol)
            set widgetInfo($address-renaming) 0
            destroy $win.ftv.border.tv.rename  
        }
    }
}

# Simple Click : raise the form relative to the component
proc cluster_double_trigger {win address x y} {
    global widgetInfo
    set row [$win.ftv.border.tv identify row $x $y]
    set col [$win.ftv.border.tv identify column $x $y]
    
    
    
    if {$col=="#1" && $widgetInfo($address-clustermode)!="independant"} { return 0}    
    if {$row!= ""} {
        cluster_rename $win $address $row $col
    }
}




proc cluster_context_menu {win address x y globalX globalY} {
    global widgetInfo
    
    
    
    $win.ftv.border configure -background [FocusColor] -highlightbackground [FocusColor] -highlightthickness 0 -bd  2
    update
    
    
    catch {destroy $win.ftv.border.tv.cmenu }
    set popMenu [menu  $win.ftv.border.tv.cmenu ]

    $popMenu add command -label "Copy to clipbooard" -command [ subst {cluster_save_to_clipboard $win $address
       
    }]
    $popMenu add command -label "Paste from clipboard" -command [ subst {cluster_paste_from_clipboard $win $address}] 
   
    tk_popup $popMenu $globalX $globalY
    
    $win.ftv.border configure -background  [ThemeColor 1.0] -highlightbackground [ThemeColor 1.0] -highlightthickness 2 -bd  0

}

proc cluster_save_to_clipboard {win address } {
    global widgetInfo
    clipboard clear
    clipboard append $widgetInfo($address-variable)
    log  "Data  stored in clipboard : \n $widgetInfo($address-variable) "
    
}

proc cluster_paste_from_clipboard {win address } {
    global widgetInfo
    
    if {[catch {clipboard get} contents]} {
        warning "Clipboard is empty"
        return
    }
    
    if {[llength [split $contents  ";" ]] <2} {
        warning "Clipboard data  -$contents- is not compatible with this widget"
        return
    }
    
    
    if {$widgetInfo($address-clustermode) =="require"} {
        set component_list [cluster_component_list $address]
        
        set content_component_list ""
        set couple ""
        foreach item [split $contents  ";" ] {
            lappend couple $item
            if {[llength $couple] == 1 } {
                lappend content_component_list $item
                
            }
            if {[llength $couple] == 2 } {
                set couple ""
            }
        }
        
        foreach component $content_component_list {
            if {$component ni $component_list} {
                warning "Clipboard data  -$contents- is not compatible with this widget\n clipboard : -$content_component_list-\n  widget : -$component_list-"    
                return
            }
        }
    }
    set widgetInfo($address-variable) $contents
    eval $widgetInfo($address-check)
    #cluster_refreshStatus $win $address
}


# called by the "+" button is invoked
proc cluster_ctrl_addcomponent {win address} {
    global widgetInfo tmpTree
    
    set row [$win.ftv.border.tv selection ]
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
     
    
    set row [$win.ftv.border.tv selection ]
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
        focus $win.ftv.border.tv.rename.entry
        return    
    }                    
    
    set widgetInfo($address-renaming) $row                    
    set widgetInfo($address-renamingCol) $col
    

    
    
    frame $win.ftv.border.tv.rename -background red  -bd 2
    set bbox [$win.ftv.border.tv bbox $row $col]
    
    place $win.ftv.border.tv.rename -x [lindex $bbox 0] -y [lindex $bbox 1] -width [lindex $bbox 2] -height [lindex $bbox 3]
    
    focus $win.ftv.border.tv.rename
    
    set widgetInfo($address-entry) [$win.ftv.border.tv set $row $col]
    ttk::entry $win.ftv.border.tv.rename.entry -textvariable widgetInfo($address-entry) 
    pack $win.ftv.border.tv.rename.entry -expand 1
    $win.ftv.border.tv.rename.entry selection range 0 end
    $win.ftv.border.tv.rename.entry icursor end
    focus $win.ftv.border.tv.rename.entry


    bind $win.ftv.border.tv.rename.entry <Return>  [ subst {
        # change value
        cluster_setname $address $win $row $col
        # kill dialog
        destroy $win.ftv.border.tv.rename
        set widgetInfo($address-renaming) 0
    }]
    
    bind $win.ftv.border.tv.rename.entry <FocusOut>  [ subst {
         # change value
        cluster_setname $address $win $row $col
        # kill dialog
        destroy $win.ftv.border.tv.rename
        set widgetInfo($address-renaming) 0
    }]
    
    
    bind $win.ftv.border.tv.rename.entry <Escape>  [ subst {
        # kill dialog
        destroy $win.ftv.border.tv.rename
        set widgetInfo($address-renaming) 0
    }]
    
    
   
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
    
  
    
    
    $win.ftv.border.tv delete [$win.ftv.border.tv children {}]
    
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
            $win.ftv.border.tv insert {} end -id $component -text "$component" -image "" -tags "true"
        } else {
            
            if { $test_cluster != 1} {
                 set widgetInfo($address-status_txt) $test_cluster
            }
           
            if { $test_label != 1} {
                 set widgetInfo($address-status_txt) $test_label
            }
            
            set widgetInfo($address-status) -1
            $win.ftv.border.tv insert {} end -id $component -text "$component" -image "" -tags "false"
        }
        
        if {$type == "fraction" && [string is double $var]} {
            set sum [expr {$sum + $var}] 
        }
    
        
        $win.ftv.border.tv set $component #1 "$component"
        $win.ftv.border.tv set $component #2 $var
    }
    
    if {$type == "fraction"} {
        if {$sum != 1.0 } {
            set test_cluster "sum fractions not one : $sum"
            set widgetInfo($address-status_txt) $test_cluster
            set widgetInfo($address-status) -1
        }
    }
    
    
    $win.ftv.border.tv tag configure false -background #ff5050
    $win.ftv.border.tv tag configure unknown -background #ffcd7c
    $win.ftv.border.tv tag configure true -background ""
    
    set colwidth1 [expr {int([expr {0.5* $widgetInfo(guiSmallWidgetWidth)  *$widgetInfo($address-ratio)  }])}]
    set colwidth2 [expr {int([expr {0.5* $widgetInfo(guiSmallWidgetWidth)}]) - $colwidth1}]
    $win.ftv.border.tv column #1 -width $colwidth1 -anchor w
    $win.ftv.border.tv column #2 -width $colwidth2 -anchor w
    
    
    set nrows  [llength [cluster_variable_to_content $address] ]
    
    $win.ftv.border.tv configure -height $nrows
    
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