#  This program is under CECILL_B licence. See footer for details.

proc multiple_create { args } {
    set mandatory_arguments { path_father address }
    
    # Initializes the widget
    initWidget

    
    set widgetInfo($address-listChildren) [dTree_getChildren_fast $XMLtree $full_address_XML_clean ]
    set widgetInfo($address-renaming) 0
    set widgetInfo($address-content) ""
    set widgetInfo($address-name) $name
    set widgetInfo($address-status) 1
    set widgetInfo($address-status_txt) ""
    set widgetInfo($address-copy) "none"
    set widgetInfo($address-copied) ""
    set widgetInfo($address-sorting) "none"
    
    set widgetInfo($address-lastclick) [clock milliseconds]
    
    set widgetInfo($address-full_address_XML_clean) $full_address_XML_clean
    set widgetInfo($address-full_address_XML) $full_address_XML
    
    
    if  {[dTree_attrExists $XMLtree $full_address_XML_clean "require"]==1} {
        set widgetInfo($address-multiplemode) "require"
    } else {
        set widgetInfo($address-multiplemode) "independant"
    }
    
    set size_v 10
    set size_h 1.
    set size [split [dTree_tryGetAttribute $XMLtree $full_address_XML_clean "size" "1;1" ] ";"]
    set size_h [expr {[lindex $size 0]*1.*$size_h}]
    set size_v [expr {int([lindex $size 1]*1.*$size_v)}]

    set subcolumns  [dTree_tryGetAttribute $XMLtree $full_address_XML_clean "subcolumns" "1"]  
    set widgetInfo($address-subcolumns) $subcolumns
    
    set label_name [dTree_tryGetAttribute $XMLtree $full_address_XML_clean "custom_label" "Label"]  

    ###############
    # MAIN FRAMES #
    ###############
    
    ttk::frame $win 
    ttk::frame $win.forceps -width $widgetInfo(guiBigWidgetWidth) -height 0 
    ttk::frame $win.ftv -relief groove -borderwidth 2   
    ttk::frame $win.children -relief groove -borderwidth 2
    ttk::frame $win.children.forceps -width [expr {int($subcolumns*$widgetInfo(guiSmallWidgetWidth))}] -height 0     
    switchform_create $win.children.flipbook
      
      
      
    eval $widgetInfo(packme-$win)
    pack $win.forceps -side top
    help_add_desc_docu_to_widget
    
   
    pack $win.ftv -fill both -side left -expand 1 
    pack $win.children -in $win -fill both -side top -expand 1 
    pack $win.children.forceps -side top
    
    ############
    # TREEVIEW #
    ############
    
    set title [dTree_getAttribute_fast $XMLtree $full_address_XML_clean "title"]   
    #ttk::label $win.ftv.lb -text $title -style "Multiple.TLabel"
    
    ttk::treeview $win.ftv.tv -selectmode browse -show { headings} -yscrollcommand [list $win.ftv.sby set] -height 15
    # paint upon background tags
    $win.ftv.tv tag configure false -background #ff5050
    $win.ftv.tv tag configure unknown -background #ffcd7c
    $win.ftv.tv tag configure true -background ""
    #-xscrollcommand [list $win.ftv.sbx set]
    # create the columns of the treeview    
    set widgetInfo($address-listChildren) [dTree_getChildren_fast $XMLtree "$full_address_XML_clean item" ]
    
    set ncols [expr {1+[llength $widgetInfo($address-listChildren)]}]
    
    set columns_labels "lbl"
    set visible_cols 1
    foreach child $widgetInfo($address-listChildren) {
        
        set child_visible [dTree_tryGetAttribute $XMLtree "$full_address_XML_clean item $child" "visibility"  ""]
        if {$child_visible in "hidden not_in_table"} {
           # pass
        } else {
           incr visible_cols
        }
        lappend columns_labels $child    
    }
    set widgetInfo($address-colwidth) [expr {int([expr {$size_h*1.* $widgetInfo(guiSmallWidgetWidth)/$visible_cols}])}]
    $win.ftv.tv configure -columns $columns_labels
    $win.ftv.tv column #1 -width $widgetInfo($address-colwidth) 
    $win.ftv.tv heading lbl -text $label_name -command [subst {multiple_newsorting $win $address #0}]
    set colid 1
    
    foreach child $widgetInfo($address-listChildren) {
        incr colid
        set child_title [dTree_getAttribute_fast $XMLtree "$full_address_XML_clean item $child" "title"]
        set child_visible [dTree_tryGetAttribute $XMLtree "$full_address_XML_clean item $child" "visibility"  ""]    
       
            
        if {$child_visible in "hidden not_in_table"} {
            $win.ftv.tv column #$colid -width 0 -minwidth 0 -anchor s
            $win.ftv.tv heading $child -text "" 
        } else {
            $win.ftv.tv column #$colid -width $widgetInfo($address-colwidth) 
            $win.ftv.tv heading $child -text "$child_title" -command [subst {multiple_newsorting $win $address #$colid}]
        }
        
    }        
    
    ttk::scrollbar $win.ftv.sby -orient vertical -command [list $win.ftv.tv yview]
    
    # avoid tab scrolling for mousewheel
    #bind $win.ftv.tv <MouseWheel> {+set tabscroll 0}
    #bind $win.ftv.tv  <Leave> {+set tabscroll 1}
    
    
    #ttk::scrollbar $win.ftv.sbx -orient horizontal -command [list $win.ftv.tv xview]
    
    #grid $win.ftv.lb -sticky w -column 0 -row 0
    grid $win.ftv.tv -sticky news -column 0 -row 2 
    grid $win.ftv.sby -sticky news -column 1 -row 2
    #grid $win.ftv.sbx -sticky news -column 0 -row 3
    

    
    
    ############
    # CONTROLS #
    ############
    ttk::frame $win.ftv.controls
    grid $win.ftv.controls -sticky news -column 0 -row 4
    ttk::label $win.ftv.controls.status -textvariable widgetInfo($address-status_txt) -foreground "red" -justify left -compound left
    pack $win.ftv.controls.status -side bottom 
    
    
    #icons with buttons
    ttk::button $win.ftv.controls.add -text "add $title" -compound left -command [subst {
        set newSelItem  \[multiple_ctrl_addchild $win $address\]
        eval \$widgetInfo($address-check)
        $win.ftv.tv selection set \$newSelItem
        multiple_rename $win $address  \$newSelItem #1
    }]
    balloon $win.ftv.controls.add  "Add a new row, after the row selected if any"
    
    ttk::button $win.ftv.controls.rm  -text "del $title"  -compound left -command [subst {
        set newSelItem \[multiple_ctrl_rmchild $win $address\] 
        eval \$widgetInfo($address-check)
    }]
    balloon $win.ftv.controls.rm  "Remove the selected  row."
    
    
    ttk::button $win.ftv.controls.load -text "load" -command [subst {
        multiple_ctrl_loadfromproject $win $address
       
    }]      
    
    
    if {$widgetInfo($address-multiplemode)=="independant"} {
       pack $win.ftv.controls.add $win.ftv.controls.rm -side left -anchor nw -padx 0 -pady 0      
    }
    
    pack   $win.ftv.controls.load -side left -anchor nw -padx 0 -pady 0      
    
   
   
    append widgetInfo($address-refresh) [subst {multiple_refresh $win $address}]
    append widgetInfo($address-check) [subst { multiple_check $win $address}]
    append widgetInfo($address-refreshStatus) [subst {multiple_refreshStatus $win $address}]
    
    
    finishWidget
    
    

    
    # reaction to cells
    
    bind $win.ftv.tv <<TreeviewSelect>> +[subst { multiple_sync $win $address "current_selection"}]
  
  
    bind $win.ftv.tv <ButtonPress-2> [subst { multiple_context_menu $win $address %x %y %X %Y}] 
    
    #bind $win.ftv.tv <Double-Button-1> [subst {multiple_double_trigger $win $address %x %y}] 
    
    bind $win.ftv.tv <ButtonPress-1> [subst { multiple_click $win $address %x %y }]
    bind $win.ftv.tv <Control-ButtonPress-1> [subst { multiple_trytocopy $win $address %x %y }]
    bind $win.ftv.tv <Control-B1-Motion> [subst { multiple_trytocopy $win $address %x %y}]
    
    
    bind $win.ftv.tv <ButtonRelease> [subst { set widgetInfo($address-copied) "" }]
    
    bind $win.ftv <Enter> [subst { set tabscroll 0 }]
    bind $win.ftv <Leave> [subst {
        set tabscroll 1
        destroy $win.ftv.tv.rename
        set widgetInfo($address-renaming) 0
    }]
    
    
    set widgetInfo($address-initialized) 0
    
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win.children
}


#





proc multiple_click { win address x y} {
    global widgetInfo
    set t [clock milliseconds]
    set dt [expr {$t-$widgetInfo($address-lastclick)}]    
    if { $dt < 500} {
        multiple_double_trigger $win $address $x $y
    } else {
        multiple_simple_trigger $win $address $x $y
    }
    set widgetInfo($address-lastclick) $t
}



proc multiple_context_menu {win address x y globalX globalY} {
    global widgetInfo
    set row [$win.ftv.tv identify row $x $y]
    set col [$win.ftv.tv identify column $x $y]
    #debug "B2 simple , -$row-$col- "
    if {$row!= ""} {
        $win.ftv.tv selection set $row
        multiple_sync $win $address "current_selection"
        
        
        update
        catch {destroy $win.ftv.tv.cmenu }
        set popMenu [menu  $win.ftv.tv.cmenu ]
    
        $popMenu add command -label "Copy" -command [ subst {multiple_ctrl_copychild $win $address}]
        $popMenu add command -label "Paste (or Ctrl-B1)" -command [ subst {multiple_trytocopy $win $address $x $y}] 
       
        tk_popup $popMenu $globalX $globalY
    }
    
}

# Simple Click : raise the form relative to the child
proc multiple_simple_trigger {win address x y} {
    global widgetInfo
    set row [$win.ftv.tv identify row $x $y]
    set col [$win.ftv.tv identify column $x $y]
    #debug "B1 simple , -$row-$col- "
    
}



# Double Click : raise the form relative to the child
proc multiple_double_trigger {win address x y} {
    global widgetInfo
    set row [$win.ftv.tv identify row $x $y]
    set col [$win.ftv.tv identify column $x $y]
    #debug "B1 double , -$row-$col- "
     
     
    if {$row!= ""} {
        switch $col {
            "#0" {
            
            }
            "#1" {
                if {$widgetInfo($address-multiplemode) !="require"} {
                    multiple_rename $win $address $row $col
                }
            }
            default {
                set child [  $win.ftv.tv column $col -id]
                
                # case of a menubutton
                if {[winfo exists $win.children.flipbook.$row.$child.mb]} {
                    set winmb $win.children.flipbook.$row.$child.mb
                    set xmb [winfo rootx $winmb]
                    set ymb [expr [winfo rooty $winmb] + [winfo height $winmb]]
                    $win.children.flipbook.$row.$child.mb.xor post $xmb $ymb
                }
                
                # case of a checkbutton
                if {[winfo exists $win.children.flipbook.$row.$child.cb]} {
                    focus $win.children.flipbook.$row.$child.cb
                    $win.children.flipbook.$row.$child.cb invoke
                }
                # case of an entry
                if {[winfo exists $win.children.flipbook.$row.$child.entry]} {
                    $win.children.flipbook.$row.$child.entry selection range 0 end
                    focus $win.children.flipbook.$row.$child.entry 
                }               
            }
        }
    }
}

proc multiple_newsorting {win address col} {
    global widgetInfo 
    set widgetInfo($address-sorting) $col
    multiple_sortrows $win $address 
}


proc multiple_sortrows {win address} {
    global widgetInfo
    
    if {$widgetInfo($address-sorting) =="none" } {return}
    
    set col $widgetInfo($address-sorting)
    set child  [$win.ftv.tv column $col -id]
    
    
    set sortlist ""
    foreach couple $widgetInfo($address-content) {
        set child_node [lindex $couple 0]
        set child_label [join [lrange $couple 1 end ]]
        switch $col {
            "#0" {
                set name $child_node
            }
            "#1" {
                set name $widgetInfo($address.$child_node-variable)
            }
            default {
                set child_address "$address.$child_node.$child"
                set name  $widgetInfo($child_address-variable)
            }
        }
        lappend sortlist "$child_node $name"
    }
    set sortlist [lsort -dictionary -decreasing -index 1 $sortlist]
    #puts "$sortlist"
    foreach pair $sortlist {
        $win.ftv.tv move [lindex $pair 0] {} 0
    }

}



proc multiple_trytocopy {win address x y} {
    global widgetInfo tmpTree
    
    #debug "TryToCopy $widgetInfo($address-copy)"
    # exclude if no copy 
    if {$widgetInfo($address-copy) == "none" } {return}
    
    set row [$win.ftv.tv identify row $x $y]
    
    # exclude if no target
    if {$row == ""} {return}

    # exclude if target == source
    if {$row == $widgetInfo($address-copy)} {return}
    
    # exclude if target already copied
    if {[lsearch $widgetInfo($address-copied) $row ] != -1 } {
        return
    } else {
        lappend widgetInfo($address-copied) $row
    }
    
    # copy
    set target_address $address.$row
    set source_address $address.$widgetInfo($address-copy)
    
    # copy paste in the tmpTree
    dTree_duplicateBranch tmpTree [gui2tree $source_address] [gui2tree $target_address]
    
    #keep name
    set name $widgetInfo($target_address-variable)
    # the the addresses of data to paste
    set widgetInfo_source_adr [array names widgetInfo $source_address\* ]
    # clean widget info array before all
    array unset widgetInfo $target_address\*
    # get the lenght of the common root in addresses
    set lroot [string length $source_address ]
    # loop on the elements to paste
    foreach element $widgetInfo_source_adr {
        # trim elements to get only the variable part at the end
        set element [string range $element $lroot end]
        # update the content of the element if it involves the name of the paste node
        regsub -all $widgetInfo($address-copy) $widgetInfo($source_address$element) $row newvalue
        # paste
        set widgetInfo($target_address$element) $newvalue
    }
    # override the name of the pasted node 
    set widgetInfo($target_address-variable) $name
    foreach  xor_former_var_adr [array names widgetInfo "$target_address\*to_reset" ] {
        set widgetInfo($xor_former_var_adr) 1
    }
    
    # call recursively the check method to update tmpTree (and the gui behind)
    CheckFamily $target_address
    
}


# renaming dialog
proc multiple_rename {win address row col } {
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
        multiple_setname $address $win $row $col
        destroy $win.ftv.tv.rename
        set widgetInfo($address-renaming) 0
    }]
    
    
    bind $win.ftv.tv.rename.entry <FocusOut>  [ subst {
        multiple_setname $address $win $row $col
        destroy $win.ftv.tv.rename
        set widgetInfo($address-renaming) 0
    }]    
    
    bind $win.ftv.tv.rename.entry <Escape>  [ subst {
        destroy $win.ftv.tv.rename
        set widgetInfo($address-renaming) 0
    }]
}

# change the label assigned 
proc multiple_setname { address win row col } {
    global widgetInfo tmpTree

    
    set current_nodes ""
    set current_labels ""
    foreach couple $widgetInfo($address-content) {
        lappend current_nodes [lindex  $couple 0]
        lappend current_labels [join [lrange  $couple 1 end]]
    }                
    
    set node_tochange $row
    set rank_tochange [lsearch $current_nodes $row]
    set old_label [lindex $current_labels $rank_tochange]
    set new_label [ stringcleanBalise $widgetInfo($address-entry)]
    
    
    if {$old_label != $new_label} {
        
        if { [lsearch $current_labels $new_label]==-1} {
            # update the content
            set widgetInfo($address-content) [ lreplace $widgetInfo($address-content) $rank_tochange $rank_tochange "$node_tochange $new_label" ]
            # update the variable and check
            set child_address $address.$node_tochange
            set widgetInfo($child_address-variable)  $new_label
            eval $widgetInfo($child_address-check)
            $win.ftv.tv set $row $col $new_label
    
        } else {
            set widgetInfo($address-status_txt) "Label $widgetInfo($address-entry) already in use "
        }
    }
    return
}


# called by the "+" button is invoked
proc multiple_ctrl_addchild {win address} {
    global widgetInfo tmpTree
    
    set widgetInfo($address-copy)  "none"
    multiple_ctrl_copychild $win $address
    
    set row [$win.ftv.tv selection ]
    # find where to insert the child         
    set list_nodes ""
    foreach couple $widgetInfo($address-content) {
        lappend list_nodes [lindex $couple 0]
    }
    if { $row != "" } {
        set rank [lsearch $list_nodes $row]
        incr rank
        
    } else {
        set rank "end"
        set row 0
    }
    set childname [ multiple_new_child $win $address $rank ]
    
    return $childname
}

proc multiple_ctrl_rmchild {win address } {
    global widgetInfo
    
    set widgetInfo($address-copy)  "none"
    multiple_ctrl_copychild $win $address    
    
    set row [$win.ftv.tv selection ]
    # find where to delete the child
    set list_nodes ""
    foreach couple $widgetInfo($address-content) {
        lappend list_nodes [lindex $couple 0]
    }
    if { $row != "" } {
        set rank [lsearch $list_nodes $row]
        set newSelItem [lindex $list_nodes [expr {$rank+1}]]

    } else {
        return
    }
    # abort if its the last item
    #if {[llength "$widgetInfo($address-content)"] == 1 } {return}
    
   
    multiple_del_child $win $address $row
    return $newSelItem
}


proc multiple_ctrl_copychild {win address } {
    global widgetInfo
    
    set source_child [$win.ftv.tv selection ]
    #debug "multiple_ctrl_copychild -$source_child-"
    if {$source_child ==""} {return}
    set widgetInfo($address-copy) $source_child
    
    #debug "multiple_ctrl_copychild ### - $widgetInfo($address-copy)-"
   
    set source_name $widgetInfo($address.$source_child-variable)
    
}

proc multiple_ctrl_loadfromproject {win address } {
    global widgetInfo DStree tmpTree
    
    set filename [tk_getOpenFile -title "Load the \"$widgetInfo($address-name)\" setup of an other project" ]
    if {$filename == ""} {return}
    
    log "Try to load file $filename"
    dTree_init SourceDStree
    parseFile $filename SourceDStree "" "DStree"
    dTree_copyRelevantBranch_ForpartialData SourceDStree [linsert  [gui2tree $address] 1 "dataset" ]  DStree [gui2tree [addrGetFather $address]] 
    RefreshFamily $address
    eval $widgetInfo($address-check)   
}



# called when :
# - "+" button is invoked
# - a new child is needed
proc multiple_new_child { win address rank  { child_label "none"} } {
    global widgetInfo
        
    #set index [string range [clock clicks] end-6 end]
    set index [info cmdcount]
    set child_node "item_$index"
    # in the case of unknown name
    if {$child_label=="none"} {
        #set child_label "$widgetInfo($address-name)_$index"
        set child_label "no label"
    }
    # add to the main multiple widget
    set widgetInfo($address-content) [ linsert $widgetInfo($address-content) $rank  "$child_node  $child_label"]
    return $child_node
}


# called when :
# - "-" button is invoked
# - a  child must be deleted
proc multiple_del_child { win address child_node } {
    global widgetInfo 
    
    set list_nodes ""
    foreach couple $widgetInfo($address-content) {
        lappend list_nodes [lindex $couple 0]
    }    
    set rank [lsearch $list_nodes $child_node]
    
    # remove from the main multiple widget
    set widgetInfo($address-content) [ lreplace $widgetInfo($address-content) $rank $rank  ]
   
}




proc multiple_refresh {win address} {
    global widgetInfo  DStree
    set content ""
    
    
    
    # instead of using widgetInfo(variable), this is using the childs, because we want no value in the widget multiple, only childs
    if {[dTree_nodeExists $DStree $widgetInfo($address-full_address_XML)]} {
            foreach child [dTree_getChildren_fast $DStree $widgetInfo($address-full_address_XML)] {
                lappend content [list $child [dTree_getAttribute_fast $DStree "$widgetInfo($address-full_address_XML) $child" "value"]]
            }
    }
    set widgetInfo($address-content) $content      
    #
    
  
        
    #add the check/refresh procedure to the bindings of the variable
    if {$widgetInfo($address-multiplemode) =="require"} {
        multiple_require $win $address
    }      
}




proc multiple_refreshStatus {win address} {
    global widgetInfo
    
    # some reset to kepp the multiple tidy
    destroy $win.ftv.tv.rename 
    set widgetInfo($address-renaming) 0
    
   
    set widgetInfo($address-status_txt) ""
    
    set current_nodes ""
    set current_labels ""
    foreach couple $widgetInfo($address-content) {
        set node [lindex  $couple 0]
        set lbl [join [lrange  $couple 1 end]]
        
        
        #set widgetInfo($address.$node-status) 0
        
        
        if {$lbl in $current_labels} {
            set widgetInfo($address-status_txt) "Some items have the same labels"       
        }
        
        lappend current_nodes $node
        lappend current_labels $lbl
        
        if {$lbl == "no label"} {
            set widgetInfo($address-status_txt) "Some items have no label"   
        }
        
        
    }            
    
    
    # Reinitialize the treeview
    set tvselection [$win.ftv.tv selection]
    ###############
    # CRITICAL AD
    $win.ftv.tv delete [$win.ftv.tv children {}]
    #foreach tvitem [$win.ftv.tv children {}] {
    #    if {$tvitem ni $current_nodes } {
    #        $win.ftv.tv delete $tvitem
    #    }
    #}
    ##############
    
    
    # create the lines of the treeview
    foreach current_node $current_nodes {
        $win.ftv.tv insert {} end -id $current_node -text "$current_node" -image ""
        
         # fill each column by value
        $win.ftv.tv set $current_node lbl $widgetInfo($address.$current_node-variable)
        set colid 1
        foreach child $widgetInfo($address-listChildren) {
            incr colid
            set child_address "$address.$current_node.$child"
            $win.ftv.tv set $current_node #$colid $widgetInfo($child_address-variable)
            
        }
    
        if {$widgetInfo($address.$current_node-status) == "-1" && $widgetInfo($address-status_txt) == ""} {
            set widgetInfo($address-status_txt) "One or more items are false" 
        }
        
        
        
        #change background tags if needed
        switch $widgetInfo($address.$current_node-status) {
            "0" {
                $win.ftv.tv item $current_node -tags "unknown"
            }
            "-1" {
                $win.ftv.tv item $current_node -tags "false"
            }
            default {
                $win.ftv.tv item $current_node -tags "true"
            }
        }
        
    }
    

    smartpacker_update_visibility $win $address
    
    # catched because if the item is deleted, this selection cannot be done
    catch {$win.ftv.tv selection set $tvselection} err
    multiple_sortrows $win $address 
    multiple_sync $win $address "current_selection"
    
}

proc multiple_sync {win address row } {
    global widgetInfo

    #warning "SYNC"
    if {$row == "current_selection"} {
        set row [$win.ftv.tv selection]
    }    
    if {$row == ""} {
        switchform_blank $win.children.flipbook 
    } else {
        switchform_raise $win.children.flipbook $row 
    }
}
 




# fills the table of multiple_selection widget
# in accordance with widgetInfo($address-content) ONLY
# this should depend of NOTHING ELSE due to the refresh status


#
proc multiple_check {win address } {
    global widgetInfo tmpTree XMLtree DStree loading 
    
    #INITIALIZATIONS
    set full_address_XML $widgetInfo($address-full_address_XML)
    set addressesToRefresh ""
    # get nodes and labels in the widgetInfo(*-content)
    set current_nodes ""
    set current_labels ""
    foreach couple $widgetInfo($address-content) {
        lappend current_nodes [lindex  $couple 0]
        lappend current_labels [join [lrange  $couple 1 end]]
    }
    # get nodes and labels in the tmpTree
    set tree_nodes ""
    set tree_labels ""
    foreach child [dTree_getChildren_fast $tmpTree $widgetInfo($address-full_address_XML) ] {
        set name [dTree_getAttribute_fast $tmpTree "$widgetInfo($address-full_address_XML) $child" value ]
        lappend tree_nodes $child
        lappend tree_labels $name
    }
    
    
    # remove all prexistent subnodes in the Tree that are not in the present variable
    # and the associated switchbooks
    # update the lists about the tmpTree
    foreach tree_child $tree_nodes {
        if {[lsearch $current_nodes $tree_child ]==-1} {
            dTree_rmBranch tmpTree "[split $address.$tree_child "."]"
            switchform_del $win.children.flipbook $tree_child
            
            set rank [lsearch $tree_nodes $tree_child ]
            set tree_nodes [lreplace  $tree_nodes $rank $rank  ]
            set tree_labels [lreplace $tree_labels $rank $rank  ]
        }
    }
    
    
    
    # loop on the names of the variable searching for non existing nodes
    set rank_in_current 0
    foreach  current_node $current_nodes {
        set current_label [lindex $current_labels $rank_in_current]
    
        
        
        # create the multiple sub_nodes if they do not exists
        if {[lsearch $tree_nodes $current_node] ==-1} {
            
            # tmpTree , node creation
            multiple_add_subnode $win $address $current_node $current_label
            
            # switchForm
            switchform_add $win.children.flipbook $current_node
            switchform_raise $win.children.flipbook $current_node
            
            ttk::style configure Titled.TLabel -font "helvetica 16 bold" -foreground [ThemeColor 0.50]
            ttk::label  $win.children.flipbook.$current_node.title -textvariable widgetInfo($address.$current_node-variable) -style "Titled.TLabel"
            
            
            
            pack $win.children.flipbook.$current_node.title -side top
            set ListOfPackedWidgets ""
            set ListOfAllWidgets ""
            set colid 1
            foreach child $widgetInfo($address-listChildren) {
                incr colid
                set current_nodeType [dTree_getAttribute_fast $XMLtree "$widgetInfo($address-full_address_XML_clean) item $child" "nodeType"]
                set child_address "$address.$current_node.$child"
                # theen -recursively- continue the packing
                gui_addpart -address $child_address -path_father $win.children.flipbook.$current_node -class $current_nodeType
                
                set child_win $win.children.flipbook.$current_node.$child
                
                # this one is fun...
                # prefill the check-callback, before asking the building of the widgets
                append widgetInfo($child_address-check) [subst {
                    $win.ftv.tv set $current_node #$colid \$widgetInfo($child_address-variable)
                }]
                
                
                lappend ListOfAllWidgets $child_address
                if {$widgetInfo(packme-$child_win) != ""} {
                    lappend ListOfPackedWidgets $child_address
                }
                
                lappend addressesToRefresh $child_address
            }
            
            
            set maxcols $widgetInfo($address-subcolumns)
            
            
            
            if {$maxcols > 1 } {
                
                # forget
                pack forget $win.children.flipbook.$current_node.title
                foreach wa $ListOfPackedWidgets {
                    pack forget $widgetInfo($wa-win)
                }
                
                # grid title
                grid $win.children.flipbook.$current_node.title  -column 0 -row 0 -columnspan $maxcols
                
                # grid forceps
                for {set subcol 0 } {$subcol < $maxcols} {incr subcol } {
                    ttk::frame $win.children.flipbook.$current_node.forceps$subcol -width [expr {int(1.*$widgetInfo(guiSmallWidgetWidth))}] -height 0
                    grid $win.children.flipbook.$current_node.forceps$subcol  -row 100 -column $subcol
                }
   
                
                # change callbacks for packing and unpacking
                set subcol 0
                set subrow  1
                set colid 1
                
                foreach wa $ListOfAllWidgets {
                    incr colid
                    if  {$wa in $ListOfPackedWidgets} {
                    
                        # here cant use widgetInfo($address-visibility) because it is not created for children yet
                        
                        if {$widgetInfo($wa-visibility) in "hidden not_in_table"} {
                            set width 0
                            set minwidth 0
                        } else {
                            set width $widgetInfo($address-colwidth)
                            set minwidth 10
                        }
                    
                        set widget_win $widgetInfo($wa-win)
                        set widgetInfo(packme-$widget_win) [subst {
                            set widgetInfo(fixedview) 1
                            grid $widget_win -row $subrow -column $subcol -sticky news
                            event generate . <<SetView>>
                            set widgetInfo(fixedview) 0
                            $win.ftv.tv column #$colid -width $width -minwidth $minwidth
                        }]
                        set widgetInfo(unpackme-$widget_win) [subst {
                            set widgetInfo(fixedview) 1
                            grid forget $widget_win 
                            event generate . <<SetView>>
                            set widgetInfo(fixedview) 0
                            $win.ftv.tv column #$colid -width 0 -minwidth 0 
                        }]
                        eval $widgetInfo(packme-$widget_win)
                        incr subcol
                        if {$subcol == $maxcols} {
                            set subcol 0
                            incr subrow 
                        }
                    }
                }
            }
           
            
        }
        
        incr rank_in_current
    }
    
    #set rows  [expr {max([llength $current_nodes],7)}]
    #$win.ftv.tv configure -height $rows
    
    # refresh the familly of the multiple
    set widgetInfo($address-status) 1
    foreach addr $addressesToRefresh {
        RefreshFamily $addr
    }    
        
    set widgetInfo($address-initialized) 1
    
    set widgetInfo($address-variable) ""
}

proc multiple_add_subnode {win address child_node child_label} {
    global widgetInfo tmpTree
    #Each subnode acts like a widget and should get everything from a proper node
    #puts "creating node $child_node $child_label"
    set args "-path_father $win -address $address.$child_node"
    set mandatory_arguments { path_father address }
    initWidget
    # caution : $address has changed!!!
    finishWidget
    set widgetInfo($address-variable) $child_label
    ##### toChange
    dTree_setAttribute tmpTree [gui2tree $address] "value" $widgetInfo($address-variable)    
    #eval $widgetInfo($address-check)
}

# prepare the filling in a different manner if a require is waiting
proc multiple_require { win address } {
    global widgetInfo
 
    set reqVar $widgetInfo($address-requiredValue)
    # get the present values of the subnodes
    set list_labels ""
    foreach couple $widgetInfo($address-content) {
        set child_node [lindex $couple 0]
        set child_label [ join [lrange  $couple 1 end] ]
        lappend list_labels $child_label
    }
    
    
    # SPECIAL CASE , when widget 
    # remove nodes if their label cannot match the required list
    if {$widgetInfo($address-initialized)} {
    foreach couple $widgetInfo($address-content) {
            set child_node [lindex $couple 0]
            set child_label [join [lrange  $couple 1 end]]
	    if {[lsearch $reqVar $child_label]==-1} {
                multiple_del_child $win $address $child_node
		
            }
        }
    }
    
    # add new nodes if they do not belong to the variable
    foreach lbl $reqVar {
        if {[lsearch $list_labels $lbl]==-1} {
            multiple_new_child $win $address "end"  $lbl 
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