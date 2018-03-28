#  This program is under CECILL_B licence. See footer for details.

###############################################
# Scrolling form, with only vertical scrolling
# Bindings of scrollbars are redefined to fix a -multiple match binding- problem
# A binding on mouseWheel is added on the form and its children
################################################

# creation of the scrollform
proc scrollform_create {win } {
    global ScrollInfo widgetInfo
    
    
    set ScrollInfo($win-y0) 0
    
    set ScrollInfo(count) 0
    
    # main frame
    ttk::frame $win
    pack $win -side top -expand 1 -fill both 
    
    ttk::scrollbar $win.sbary -orient vertical -command [list $win.vport yview]
    ttk::scrollbar $win.sbarx -orient horizontal -command [list $win.vport xview]
    
    
    #bind $win.sbary <ButtonPress> [subst { puts "sby $win" }]
    
    canvas $win.vport  -xscrollcommand  [list $win.sbarx set] -yscrollcommand  [list $win.sbary set]  -borderwidth 0 -highlightthickness 0 -insertborderwidth 0 -background [ThemeColor 0.9]
   
    set guiWidth $widgetInfo(guiwidth)
    set guiHeight $widgetInfo(guiheight)
   
    #$win.vport create oval 0 0 $widgetInfo(guiwidth) $widgetInfo(guiheight) -outline white -width 4 
    
    grid $win.vport -sticky news -column 0 -row 0
    grid columnconfigure $win 0 -weight 1
    grid rowconfigure $win 0 -weight 1

    grid $win.sbary -sticky news -column 1 -row 0
    grid $win.sbarx -sticky news -column 0 -row 1
    
    
    $win.vport configure -background [ThemeColor 1.0]
    bind . <<ThemeUpdate>> +[subst {$win.vport configure -background \[ThemeColor 1.0\]}]
    
    # form to be scrolled
    ttk::frame $win.vport.form
 
   
    
   
    $win.vport create window 0 0 -window $win.vport.form  
    #-width 2000 -height 2000
        
    #bind $win.vport.form <Visibility>  [subst { scrollform_upperleft $win}]
    
    bind $win.vport.form <Visibility>  [subst { scrollform_resize $win}]
    
    
    bind . <<InitializeGUI>>  +[subst { scrollform_upperleft $win}]
    
    return $win
}


# procedure for scrolling using the drag and drop binding
proc scrollform_scroll {win x y} {
    global ScrollInfo
    
    set gap [expr ($y - $ScrollInfo($win-fy))]
    
    if { $gap > 0} {
        $win.vport yview scroll 1 units
    }
    if { $gap < 0} {
        $win.vport yview scroll -1 units
    }
    set ScrollInfo($win-fy) $y
}

# procedure for scrolling using the MouseWheel binding
# warning, delta is negative with respect to canvas corrdinate
proc scrollform_scrollwheel {win delta} {
    global tabscroll
    
    if {$tabscroll} {    
        set scrollamount [expr int(-$delta)]
        $win.vport yview scroll $scrollamount units
        #if {[expr abs($scrollamount) > 3]} {
        #    set delta [expr int($delta*0.3)]
        #     scrollform_scrollwheel $win $delta 
        #}
    }
}

# procedure for scrolling using the MouseWheel binding
# warning, delta is negative with respect to canvas corrdinate
proc scrollform_lateralscroll {win delta} {
    global tabscroll
    if {$tabscroll} {    
        set scrollamount [expr int(-$delta)]
        $win.vport xview scroll $scrollamount units
        #if {[expr abs($scrollamount) > 3]} {
        #    set delta [expr int($delta*0.33)]
        #     scrollform_lateralscroll $win $delta 
        #}
    }
}


# resise the scrollform
# update the scrollbar visibility
# propagate mousewheel binding

proc scrollform_resize {win} {
    global ScrollInfo
    set frac_x [lindex [$win.sbarx  get] 0 ]
    set frac_y [lindex [$win.sbary  get] 0 ]
    set bbox [$win.vport bbox all]
    
    $win.vport configure -scrollregion $bbox
    
    bind . <MouseWheel>  [subst {scrollform_scrollwheel $win %D}]
    bind . <Shift-MouseWheel>  [subst {scrollform_lateralscroll $win %D}]
    
    $win.vport yview moveto $frac_y
    $win.vport xview moveto $frac_x
    
    
    # get geometry of form
    set height [winfo height $win.vport.form]
    set width [winfo width $win.vport.form]
    
    # get the bbox of canvas
    set bbox [$win.vport bbox all]
    
    # toggle y scrollbar
    set win_height [winfo height $win.vport]
    if {$height > $win_height && [winfo viewable $win.vport.form ] } {
        grid  $win.sbary -sticky news -column 1 -row 0
        $win.vport configure -scrollregion $bbox 
        bind .  <MouseWheel> [subst {scrollform_scrollwheel $win %D}]
    } else {
        $win.vport yview moveto 0.0
        grid forget $win.sbary
        bind . <MouseWheel> ""
    }
    # toggle x scrollbar
    set win_width [winfo width $win.vport]
    if {$width > $win_width} {
        grid $win.sbarx -sticky news -column 0 -row 1
        $win.vport configure -scrollregion $bbox
        bind .  <Shift-MouseWheel> [subst {scrollform_lateralscroll $win %D}]
    } else {
        $win.vport xview moveto 0.0
        grid forget $win.sbarx
        bind . <Shift-MouseWheel> ""
    }

    return $win
}

# Only return where child widget must be packed
proc scrollform_interior { win } {
    return $win.vport.form

}

# Only return where child widget must be packed
proc scrollform_upperleft { win } {
    global widgetInfo
    scrollform_resize $win
    
    $win.vport xview moveto 0.0
    $win.vport yview moveto 0.0
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