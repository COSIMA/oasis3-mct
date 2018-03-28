#  This program is under CECILL_B licence. See footer for details.

proc selection_create { args } {
    set mandatory_arguments { path_father address }
    
    # Initializes the widget
    initWidget
    
    
 
    
    ttk::frame $win
    
    eval $widgetInfo(packme-$win)
    
    
    ttk::frame $win.forceps -width $widgetInfo(guiSmallWidgetWidth) -height 0     
    set title [dTree_getAttribute $XMLtree $full_address_XML "title"]
    
    set widgetInfo($address-headings) [dTree_tryGetAttribute $XMLtree $full_address_XML "headings" ""]
    set widgetInfo($address-controls) [split [dTree_tryGetAttribute $XMLtree $full_address_XML "controls" "toggle"] ";"]
    set widgetInfo($address-selection) [dTree_tryGetAttribute $XMLtree $full_address_XML "selection" "multiple"] 
    set widgetInfo($address-filter) ""
    set widgetInfo($address-col_order) "0"
    set widgetInfo($address-status) 1
    set widgetInfo($address-mode) ""   
    if {[dTree_attrExists $XMLtree $full_address_XML "require"]} {
        set widgetInfo($address-mode) "require"        
    }
    
    
    
    # controls
    ttk::frame $win.c
    grid $win.c -sticky e -column 0 -row 0
    ttk::label $win.c.lb -text "$title:" -justify right
    ttk::button $win.c.button -text "on" -image icon_checkedunchecked -text "Toggle all" -compound left -command [subst {selection_toggle_all $win $address}]
    
    ttk::label $win.c.flb -text "Filter" -justify right
    ttk::entry $win.c.fle -textvariable widgetInfo($address-filter) -justify right
    bind $win.c.fle <KeyRelease> [subst {selection_filter $win $address}]
    

    pack $win.c.lb -side top -fill x    
    if {"toggle" in $widgetInfo($address-controls)} {
        pack $win.c.button  -side top -fill x
    }
    if {"filter" in $widgetInfo($address-controls)} {
        pack $win.c.flb -side top -fill x
        pack $win.c.fle -side top -fill x
    } 
    
    # treeview
    ttk::treeview  $win.tv -selectmode browse -show {tree} -yscrollcommand [list $win.sby set] -height 10
    ttk::scrollbar $win.sby -orient vertical -command [list $win.tv yview]
    grid $win.tv -sticky e -column 1 -row 0
    grid $win.sby -sticky news -column 2 -row 0
    
    
    
    
    
    bind $win.tv <Button-1> [subst {selection_toggle $win $address \%x \%y} ]
    bind $win.tv <Enter> [subst { set tabscroll 0 }]
    bind $win.tv <Leave> [subst { set tabscroll 1 }]
    
    
    #add the check/refresh procedure to the bindings of the variable
    append widgetInfo($address-check) [ subst {
        selection_check $win $address
    }]
    append widgetInfo($address-refresh) [ subst {
        selection_refresh $win $address
    }]
    finishWidget
    
    # to update content if called by a modelXOR
    selection_refresh $win $address
    
    return $win
}

proc selection_filter { win address } {
    global widgetInfo
    
    # remove the filtered values
    set new_content ""  
    foreach keyvalue [keyvalue_listcsv $widgetInfo($address-variable)]  {
        set key "[lindex $keyvalue 0]"
        set value  [lindex $keyvalue 1]
        
        if {$widgetInfo($address-filter) != "" && ![string match "*$widgetInfo($address-filter)*" $key]} {
            set value 0
        }
        lappend new_content $key
        lappend new_content $value
    }
    
    set widgetInfo($address-variable) [join $new_content ";" ]
    selection_refresh $win $address
    
    
}

proc selection_toggle { win address x y} {
    global widgetInfo
    
    set row [$win.tv identify row $x $y]
    set col [$win.tv identify column $x $y]
    
    
    if {$row != ""} {
        set new_variable ""
        foreach keyvalue [keyvalue_listcsv $widgetInfo($address-variable)]  {
            set key [lindex $keyvalue 0]
            
            if {$widgetInfo($address-selection)== "single"} {
                set value 0    
            } else {
                set value  [lindex $keyvalue 1]
            }
            
            if {$key == $row } {
                if {$value == 1} {
                    set value 0
                } else {
                    set value 1   
                }
            }
            lappend new_variable $key
            lappend new_variable $value
        }
        set widgetInfo($address-variable) [join $new_variable ";"]
        selection_refresh $win $address
        eval $widgetInfo($address-check)
        
        
    } else {
        
        set widgetInfo($address-col_order) [string trim $col \"#\"]
        selection_refresh $win $address
       
    }
}



proc selection_toggle_all { win address } {
    global widgetInfo
    set all 0
    set selected 0
    foreach keyvalue [keyvalue_listcsv $widgetInfo($address-variable)]  {
        set value  [lindex $keyvalue 1]
        incr all
        if {$value == 1} {incr selected}
    }
    if {$all == 0} {return}
    
    if {[expr {1.*$selected/$all}]>0.5} {
        set toggle 0
    } else {
        set toggle 1
    }
    
    set new_variable ""
    foreach keyvalue [keyvalue_listcsv $widgetInfo($address-variable)]  {
        set key [lindex $keyvalue 0]
        lappend new_variable $key
        
        if {$widgetInfo($address-filter) == "" || [string match "*$widgetInfo($address-filter)*" $key]} {
            lappend new_variable $toggle
        } else {
            lappend new_variable 0
        }
        
    }
    
    set widgetInfo($address-variable) [join $new_variable ";"]
    
    selection_refresh $win $address
    eval $widgetInfo($address-check)
}



proc selection_refresh {win address} {
    global widgetInfo
    #debug "refresh"

    regsub -all {\s+} $widgetInfo($address-variable) "_" widgetInfo($address-variable)    
    
    if {$widgetInfo($address-mode) =="require"} {
        selection_require $win $address
    }
    
    # Removing old treeview content
    foreach item [$win.tv children ""] {
        $win.tv delete $item
    }
    
    
    
    # name and width of the first column
    #$win.tv  configure -columns " "
    # get columns
    set nb_column 1
    foreach keyvalue [keyvalue_listcsv $widgetInfo($address-variable)]  {
        set nvals [llength [split [lindex $keyvalue 0] "#"]]
        if {$nb_column < $nvals } { set nb_column $nvals}
    }
    set lbl_column ""
    
    
    
    
    if {$nb_column > 1} {
      
        $win.tv  configure -show {tree headings}
        $win.forceps configure -width $widgetInfo(guiBigWidgetWidth)
        set colwidth [expr {int(0.5* $widgetInfo(guiSmallWidgetWidth)) }]
        
        for {set i 0} {$i < $nb_column} {incr i} {
            if {$i>0} {
                lappend lbl_column col_$i
            }
            $win.tv configure -columns $lbl_column
#            $win.tv column #$i -width $colwidth 
        }
        
    } else {
        $win.tv  configure -show {tree}
        $win.forceps configure -width $widgetInfo(guiSmallWidgetWidth)
        set colwidth [expr {int(0.5* $widgetInfo(guiSmallWidgetWidth)) }]
        $win.tv column #0 -width $colwidth
        $win.tv configure -columns ""
    }
     
    set headings [split $widgetInfo($address-headings) ";"]
    if {$headings != ""} {
        if {[llength $headings] >= $nb_column  } {
             for {set i 0} {$i < $nb_column} {incr i} {
                $win.tv heading #$i -text [lindex $headings $i ]
            }
        } else {
            puts "error, found $nb_column columns with to few headings:  $headings"
        }
    }
    
    
     
    set tvheight [llength [keyvalue_listcsv $widgetInfo($address-variable)]]
    $win.tv configure -height [expr {min($tvheight,10)}]
    
    
    
    
    
    # Reconstructing and filtering
    foreach keyvalue [keyvalue_listcsv $widgetInfo($address-variable)]  {
        set key "[lindex $keyvalue 0]"
        set value  [lindex $keyvalue 1]
        
        set colid 0
        set head "" 
        if {$widgetInfo($address-filter) == "" || [string match "*$widgetInfo($address-filter)*" $key]} {
            foreach elmt [split $key "#"] {
                
                if {$colid == 0} {
                    set head $elmt
                    if $value {
                        $win.tv insert {} end -id "$key" -text "$head" -image icon_checked
                    } else {
                        $win.tv insert {} end -id "$key" -text "$head" -image icon_unchecked
                    }
                } else {
                    $win.tv set "$key" #$colid "$elmt"
                }
                incr colid
            }
        }
    }
    
    
    # reorderin
    set sortlist ""
    set colid $widgetInfo($address-col_order)
    
    foreach keyvalue [keyvalue_listcsv $widgetInfo($address-variable)]  {
        set key [lindex $keyvalue 0]
        if {$widgetInfo($address-filter) == "" || [string match "*$widgetInfo($address-filter)*" $key]} {
            set keylist [split $key "#"]
            set elmt [string trim [lindex  $keylist $colid] ]
            if {$elmt ==""} {set elmt "ZZZZZZ" }
            lappend sortlist "$key $elmt"
        }
    }
    set sortlist [lsort -dictionary -decreasing -index 1 $sortlist]
    foreach pair $sortlist {
        $win.tv move [lindex $pair 0] {} 0
    }
}



proc selection_check {win address} {
    global widgetInfo
    
}


# prepare the filling in a different manner if a require is waiting
proc selection_require { win address } {
    global widgetInfo
    
    #debug "require"
    
    set reqVar $widgetInfo($address-requiredValue) 
    set default [lindex [split $widgetInfo($address-default) ";"]  1]
    if {$default==""} {set default 0.0 }
    
    set new_content ""       
    # get the present values of the components if they belon to reQvar   
    foreach actual_component $reqVar {
        set component_found 0
        foreach keyvalue [keyvalue_listcsv  $widgetInfo($address-variable)]  {
            set key [lindex $keyvalue 0]
            set value [lindex  $keyvalue 1 ]
            
            if {$key == $actual_component} {
                lappend new_content $key
                lappend new_content $value
                set component_found 1
            }
        }
        if {$component_found==0 } {
            lappend new_content $actual_component
            lappend new_content $default
        }
    }
    
    
    set widgetInfo($address-variable) [join $new_content ";" ]
    
    # hide id void list
    if {[llength $new_content] == 0} {
        pack forget $win
        return
    } else {
        eval $widgetInfo(packme-$win)
    }
    
    #debug "req : $widgetInfo($address-requiredValue) "
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