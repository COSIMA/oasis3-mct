#  This program is under CECILL_B licence. See footer for details.

proc dynlist_create { args } {
    set mandatory_arguments { path_father address }
    
    initWidget
    
    set title [dTree_getAttribute $XMLtree $full_address_XML "title"]
    set widgetInfo($address-type) [dTree_getAttribute $XMLtree $full_address_XML "type"]
    set widgetInfo($address-renaming) 0
    set widgetInfo($address-status) 1
    ttk::frame $win
    
    #title
    ttk::label $win.title  -text $title

    # listbox and scrollbar
    ttk::frame $win.dl
    listbox $win.dl.lb -listvariable widgetInfo($address-content) -yscrollcommand [list $win.dl.sby set] -height 5 -activestyle none 
    $win.dl.lb  selection set 0
    # avoid tab scrolling for mousewheel
    bind $win.dl.lb <MouseWheel> {+set tabscroll 0}
    bind $win.dl.lb <Leave> {+set tabscroll 1}
    
    
    
    
    ttk::scrollbar $win.dl.sby -orient vertical -command [list $win.dl.lb yview]
    # avoid tab scrolling for mousewheel
    bind $win.dl.sby <MouseWheel> {+set tabscroll 0}
    bind $win.dl.sby <Leave> {+set tabscroll 1}
    
    grid $win.dl.lb -sticky news -column 0 -row 0
    grid $win.dl.sby -sticky news -column 1 -row 0
    
    # controls
    ttk::frame $win.controls
    ttk::label $win.controls.add -width 1 -image icon_plus -relief raised
    ttk::label $win.controls.rm  -width 1 -image icon_minus -relief raised
    pack $win.controls.add $win.controls.rm  -side left -anchor nw -padx 0 -pady 0
    
    #status
    ttk::label $win.status -textvariable widgetInfo($address-status_txt) -foreground  red
    
    #packing
    pack $win -pady 5
    pack $win.status -side bottom -anchor nw
    pack $win.title -side top -anchor nw
    pack $win.dl -anchor nw
    pack $win.controls -anchor nw

    # bindings
    
    # kill rename dialog if leaving widget
    bind $win <Leave> +[subst {
        destroy $win.dl.lb.rename
        set widgetInfo($address-renaming) 0
    }]
        
    
    #double click
    bind $win.dl.lb <Double-1> [subst { dynlist_rename_existing $win $address %y }]
    #simple click
    bind $win.dl.lb <ButtonPress> [subst {+
        # cancel renaming if necessary
        if { \$widgetInfo($address-renaming) != 0 } {
            destroy $win.dl.lb.rename
            set widgetInfo($address-renaming) 0
        }    
    }]
    
    # addition
    bind $win.controls.add  <ButtonPress> [subst {
        $win.controls.add configure -relief sunken
        dynlist_add $win $address 
    }]
    
    bind $win.controls.add  <ButtonRelease> [subst {$win.controls.add configure -relief raised}]
    
    
    # deletion
    bind $win.controls.rm  <ButtonPress> [subst {
        $win.controls.rm configure -relief sunken
        dynlist_del $win $address 
        
    }]
    bind $win.controls.rm  <ButtonRelease> [subst {$win.controls.rm configure -relief raised}]
    
    
    append widgetInfo($address-refresh) [subst { dynlist_refresh $win $address }]
    append widgetInfo($address-check) [ subst { dynlist_check $win $address }]
    finishWidget
    
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
}


# after variable> tmpTree
proc dynlist_check {win address} {
    global widgetInfo
#    set type [lindex [ split $widgetInfo($address-type) "_" ] 1 ]
#    set test_dl 1
#    foreach elt $widgetInfo($address-content) {
#        set test_dl [expr {[ test_vartype $elt $type ] * $test_dl}]
#    }
#    
#    if {$test_dl == 1} {
#        set widgetInfo($address-status) 1
#        set widgetInfo($address-status_txt) ""
#        #set widgetInfo($address-content) [lreplace $widgetInfo($address-content) $index $index $widgetInfo($address-entry)]
#    } else {  
#        set widgetInfo($address-status) -1
#        set widgetInfo($address-status_txt) $test_dl
#    }
    
    set widgetInfo($address-variable) [join $widgetInfo($address-content) ";"]
}

# after DStree >variable
proc dynlist_refresh {win address} {
    global widgetInfo
    set widgetInfo($address-content) [split $widgetInfo($address-variable) ";"]
}



# DYNLIST specific procedures

# addition of one element to the list
proc dynlist_add {win address} {
    global widgetInfo tmpTree
    
    # cancel renaming if on
    if { $widgetInfo($address-renaming) != 0} {
        destroy $win.dl.lb.rename
        set widgetInfo($address-renaming) 0
    }    
    
    # get where to insert the item
    set rank [$win.dl.lb curselection]
    if {$rank ==""} {
        set rank end
    } else {
        incr rank
    }
    
    
    # find what to insert
    set type [lindex [ split $widgetInfo($address-type) "_" ] 1 ]
    switch $type {
        "double" {
            set guess "0.0"
            while {[lsearch "$widgetInfo($address-content)" "$guess"] != -1} {
                set guess [expr (guess+1.0)]
            }
        }
        "integer" {
            set guess "0"
            while {[lsearch "$widgetInfo($address-content)" "$guess"] != -1} {
                incr guess
            }
        }
        default {
            set guess "0"
            while {[lsearch "$widgetInfo($address-content)" "item_$guess"] != -1} {
                incr guess
            }
            set guess "item_$guess"
        }
    }
    
    set widgetInfo($address-content) [linsert $widgetInfo($address-content) $rank $guess]
    
    #call rename dialog
    dynlist_rename $win $address $rank
    
    eval $widgetInfo($address-check)
   
}


# renaming an element in the list
proc dynlist_rename_existing {win address y} {
    global widgetInfo tmpTree
    dynlist_rename $win $address [$win.dl.lb nearest $y]  
    eval $widgetInfo($address-check)
}


# renaming dialog
proc dynlist_rename {win address rank } {
    global widgetInfo
    
    if { $widgetInfo($address-renaming) != 0 } {
        focus $win.dl.lb.rename.entry
        return    
    }
    
    set widgetInfo($address-renaming) $rank
    
    $win.dl.lb see $rank
    set bbox [$win.dl.lb bbox $rank]
    set width [expr ([winfo width $win.dl.lb]-3)]
    
    
    frame $win.dl.lb.rename -background [ThemeColor 1.]
    place $win.dl.lb.rename -x [lindex $bbox 0] -y [lindex $bbox 1] -width $width -height [lindex $bbox 3]
    set widgetInfo($address-entry) [$win.dl.lb get $rank]
    ttk::entry $win.dl.lb.rename.entry -textvariable widgetInfo($address-entry) 
    pack $win.dl.lb.rename.entry -expand 1 -fill both
    $win.dl.lb.rename.entry selection range 0 end
    $win.dl.lb.rename.entry icursor end  
    focus $win.dl.lb.rename.entry
     
    bind $win.dl.lb.rename.entry <Return>  [ subst {
        dynlist_setvar $win $address $rank
        destroy $win.dl.lb.rename
        set widgetInfo($address-renaming) 0
    }]
     
    bind $win.dl.lb.rename.entry <Escape>  [ subst {
        destroy $win.dl.lb.rename
        set widgetInfo($address-renaming) 0
    }]

}

# insertion of the new item in the list
proc dynlist_setvar {win address index} {
    global widgetInfo tmpTree
    set type [lindex [ split $widgetInfo($address-type) "_" ] 1 ]
    
    set test_dl [ test_vartype $widgetInfo($address-entry) $type ] 
    if {$test_dl == 1} {
        set widgetInfo($address-status) 1
        set widgetInfo($address-status_txt) ""        
        set widgetInfo($address-content) [lreplace $widgetInfo($address-content) $index $index $widgetInfo($address-entry)]
    } else {  
        set widgetInfo($address-status) -1
        set widgetInfo($address-status_txt) $test_dl
    }
    eval $widgetInfo($address-check)
}


proc dynlist_del {win address} {
    global widgetInfo tmpTree
    
    # cancel renaming if on
    if { $widgetInfo($address-renaming) != 0 } {
        destroy $win.dl.lb.rename
        $win.dl.lb selection set $widgetInfo($address-renaming)
        set widgetInfo($address-renaming) 0
    }    
     # get where to delete the item
    set rank [$win.dl.lb curselection]
    if {$rank ==""} {return}
    
    # remove if it is not the last element
    if {[llength $widgetInfo($address-content)] > 1} {
         set widgetInfo($address-content) [lreplace $widgetInfo($address-content) $rank $rank ]
         incr rank -1
         focus $win.dl.lb
         $win.dl.lb see $rank
         $win.dl.lb selection clear 0 end          
         $win.dl.lb selection set $rank    
    }
    
    eval $widgetInfo($address-check)
}


proc dynlist_update_entry {win address} {
    global widgetInfo tmpTree
        set widgetInfo($address-entry) [  $win.dl.lb get [ $win.dl.lb curselection]]
        eval $widgetInfo($address-check)
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