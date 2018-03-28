#  This program is under CECILL_B licence. See footer for details.




# COMPARATOR CREATION

proc comparator_create { args } {
    set mandatory_arguments { path_father address }
    
    # Initializes the widget
    initWidget  
    
    set widgetInfo($address-status) 0
    set widgetInfo($address-boxsize) 15
    set widgetInfo($address-statustxt) ""
    set title [dTree_getAttribute $XMLtree $full_address_XML "title"]   
    set widgetInfo($address-folder)  [dTree_getAttribute $XMLtree $full_address_XML "folder"]
    ttk::labelframe $win -text "$title" 
    eval $widgetInfo(packme-$win)
   
    
    #ttk::label $win.lb -text "$title" 
    #grid $win.lb -column 0 -row 0 -sticky news
    
    
    #frame for picker
    ttk::frame $win.t 
    
  
    
    # size of widget
    set size_v [expr 0.3*$widgetInfo(guiBigWidgetWidth)]
    set size_h [expr 0.3*$widgetInfo(guiBigWidgetWidth)]
    canvas $win.t.can -background [ThemeColor 1.0]  -highlightthickness 1 -highlightbackground [ThemeColor 0.5] -yscrollcommand [list $win.t.sby set] -xscrollcommand [list $win.t.sbx set]   -width $size_h -height $size_v 
    ttk::scrollbar $win.t.sby -orient vertical -command [list $win.t.can yview]
    ttk::scrollbar $win.t.sbx -orient horizontal -command [list $win.t.can xview]
    grid $win.t.can -row 1 -column 0 -sticky news 
    
    
    
    # frame for graphs   
    ttk::frame $win.gr
    #pack $win.gr -side top -pady {0 5}
    
    set size_v [expr 0.3*$widgetInfo(guiBigWidgetWidth)]
    set size_h [expr 0.8*$widgetInfo(guiBigWidgetWidth)]    
    canvas $win.gr.can  -background [ThemeColor 1.0]  -highlightthickness 1 -highlightbackground [ThemeColor 0.5] -yscrollcommand [list $win.gr.sby set] -xscrollcommand [list $win.gr.sbx set]   -width $size_h -height $size_v
    ttk::scrollbar $win.gr.sby -orient vertical -command [list $win.gr.can yview]
    ttk::scrollbar $win.gr.sbx -orient horizontal -command [list $win.gr.can xview]
    
    ttk::frame $win.gr.c
    ttk::button $win.gr.c.zoomplus -image icon_magnifierplus -command [subst {grapher_zoom $win.gr.can  1.1}]
    ttk::button $win.gr.c.zoomminus -image icon_magnifierminus -command [subst {grapher_zoom $win.gr.can  0.9}]
    ttk::button $win.gr.c.dump -text "Dump" -command [subst {comparator_dump $win $address }]
    ttk::label $win.gr.c.statustxt -textvariable widgetInfo($address-statustxt) -compound left
    pack $win.gr.c.zoomplus -side left 
    pack $win.gr.c.zoomminus -side left
    pack $win.gr.c.dump -side left
    pack $win.gr.c.statustxt -side left
    
    
    
    grid $win.gr.can -row 1 -column 0 -sticky news 
    #grid $win.gr.sby -row 1 -column 1 -sticky ns
    #grid $win.gr.sbx -row 2 -column 0 -sticky ew
    grid $win.gr.c -row 3 -column 0 -sticky ew
    
  
    
     # frame for treeview   
    ttk::frame $win.tv

    
    ttk::treeview $win.tv.tv  -yscrollcommand [list $win.tv.sby set] -columns "lvalue rvalue" -height 8
    $win.tv.tv column #0 -width [expr {int(0.3*$widgetInfo(guiBigWidgetWidth))}]
    $win.tv.tv column 0 -width [expr {int(0.4*$widgetInfo(guiBigWidgetWidth))}]
    $win.tv.tv column 1 -width [expr {int(0.4*$widgetInfo(guiBigWidgetWidth))}]
    
    
    #$win.tv.tv insert {} 0 -id "dataset" -text "." -open "true"
    
    ttk::scrollbar $win.tv.sby -orient vertical -command [list $win.tv.tv yview]
   
   
   
    
    grid $win.tv.tv -row 1 -column 0 -sticky news 
    grid $win.tv.sby -row 1 -column 1 -sticky ns
 

    
    grid $win.t  -column 0 -row 0 -sticky news
    grid $win.gr -column 1 -row 0  -sticky news
    grid $win.tv -column 0 -row 1 -sticky news -columnspan 2
    
    
    
    #add the check/refresh procedure to the bindings of the variable
    append widgetInfo($address-refresh) [ subst { comparator_refresh $win $address}]
    append widgetInfo($address-check) [subst { comparator_check $win $address}]
    
    finishWidget
    
    #trick?
    #bind $win.t.can <ButtonPress-1> {+; focus %W}
    bind $win.t.can <Motion> [subst {comparator_showlocation $win  $address}]   
    #  scroll
    bind $win.t.can   <ButtonPress> [subst {$win.t.can scan mark %x %y}]
    bind $win.t.can   <B1-Motion> [subst {$win.t.can scan dragto %x %y 1}]
    
    
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
}

proc comparator_refresh {win address} {
    global widgetInfo
    comparator_update $win $address
}

proc comparator_update {win address} {
    global widgetInfo
    
    $win.t.can delete all
    
    set list_runs  $widgetInfo($address-requiredValue) 

    # remove selected comparisons from the variable if this run is no more available
    set current_variable [split $widgetInfo($address-variable) ";"]
    foreach selectedpair $current_variable {
        set run1 [lindex [split $selectedpair "@"] 0]
        set run2 [lindex [split $selectedpair "@"] 1]
        if {[lsearch $list_runs $run1 ] == -1 } {
            set current_variable [lremove current_variable $selectedpair]
        } elseif {[lsearch $list_runs $run2 ]==-1} {
            set current_variable [lremove current_variable $selectedpair]
        }
    }
    
    set boxsize $widgetInfo($address-boxsize)
    
    #title up
    set col 0
    foreach run $list_runs {
        set title_x [expr { $boxsize*(0.5+$col)}]
        set title_y [expr { $boxsize*(-0.0)}]
        set run_sim [lindex [split $run "#"] 0]
        set run_proj [lindex [split $run "#"] 1]
        set run_run [lindex [split $run "#"] 2]
        canvas_text_vector $win.t.can  $title_x $title_y "$run_proj $run_run" sw 8 -45 [comparator_getcolor $run_sim] titleup
        incr col
    }
    #title left
    set col 0
    foreach run $list_runs {
        set title_x [expr { $boxsize*(-0.2+$col)}]
        set title_y [expr { $boxsize*(0.2+$col)}]
        set run_sim [lindex [split $run "#"] 0]
        set run_proj [lindex [split $run "#"] 1]
        set run_run [lindex [split $run "#"] 2]
        canvas_text_vector $win.t.can $title_x $title_y "$run_proj $run_run" nw 8 0 [comparator_getcolor $run_sim] titleup
        incr col
    }
    
    set row 0
    set col 0
    foreach run1 $list_runs {
        foreach run2 $list_runs {
            set run1_sim [lindex [split $run1 "#"] 0]
            set run2_sim [lindex [split $run2 "#"] 0]
            set run1_proj [lindex [split $run1 "#"] 1]
            set run2_proj [lindex [split $run2 "#"] 1]
            set run1_run [lindex [split $run1 "#"] 2]
            set run2_run [lindex [split $run2 "#"] 2]
            if  {$col >= $row} {
                 if {$col == $row } {
                    set shade 0.0
                 } elseif {$run1_sim == $run2_sim} {
                    set shade 0.5
                 } else {
                    set shade 0.75
                 }
                 
                set couple "$run1@$run2"
                # ul_x--------------
                # ul_y             |
                # |                |
                # |  ul2_x---|     |
                # |  ul2_Y   |     |
                # |  |       |     |
                # |  |      lr2_x  |
                # |  |------lr2_y  |
                # |                |
                # |                |
                # |             lr_x
                # |-------------lr_y
                set ul_x [expr {$col*$boxsize}]
                set ul_y [expr {$row*$boxsize}]
    
                set lr_x [expr {$ul_x+$boxsize}]
                set lr_y [expr {$ul_y+$boxsize}]
                
                set ul2_x [expr {($col+0.2)*$boxsize}]
                set ul2_y [expr {($row+0.2)*$boxsize}]
    
                set lr2_x [expr {$ul_x+0.8*$boxsize}]
                set lr2_y [expr {$ul_y+0.8*$boxsize}]
                
                $win.t.can create polygon $ul_x $ul_y $ul_x $lr_y $lr_x $lr_y $ul_x $ul_y -fill [shadeColor [comparator_getcolor $run1_sim] $shade] -tags "$couple"
                $win.t.can create polygon $ul_x $ul_y $lr_x $ul_y $lr_x $lr_y $ul_x $ul_y -fill [shadeColor [comparator_getcolor $run2_sim] $shade] -tags "$couple"
                $win.t.can create line $ul_x $ul_y $lr_x $ul_y $lr_x $lr_y $ul_x $lr_y  $ul_x $ul_y -fill black
                
                if {[lsearch $current_variable $couple] != -1} {
                    $win.t.can create oval $ul2_x $ul2_y  $lr2_x $lr2_y  -outline white -width 2 -tags "$couple"
                }
                $win.t.can bind $couple <ButtonPress> [subst {comparator_clickrun $win $address $couple}]
            }
            incr col
        }
        set col 0
        incr row
    }
    
    set widgetInfo($address-status) 0
    set widgetInfo($address-statustxt) "Comparisons data need to be dumped..."
    $win.gr.c.statustxt configure -image icon_question
    
    $win.t.can configure -scrollregion [ $win.t.can bbox all]
    
    smartpacker_update_visibility $win $address
    
    
    set widgetInfo($address-variable) [join $current_variable ";"]
}

proc comparator_clickrun {win address selectedpair} {
    global widgetInfo
    
    
    set current_variable [split $widgetInfo($address-variable) ";"]
    $win.gr.can delete all
    if {[lsearch $current_variable $selectedpair] != -1 } {
        # remove a comparison
        set current_variable [lremove current_variable $selectedpair]
        set file1   [ lindex [ split $selectedpair "@"] 0 ]
        set file2   [ lindex [ split $selectedpair "@"] 1 ] 
        #debug "Removes $file1 $file2"
        
        #$win.tx.txt configure -state normal
        #$win.tx.txt delete 0.0 end
        #$win.tx.txt configure -state disabled
        
        
    } else {
        # add a comparison
        lappend current_variable $selectedpair
        set file1  [ lindex [ split $selectedpair "@"] 0 ] 
        set file2  [ lindex [ split $selectedpair "@"] 1 ] 
        grapher_create $win $address $win.gr.can $win.tv.tv [file join {*}$widgetInfo($address-folder)] "$file1" "$file2" 0
        #debug "Add  $file1 $file2"
    }
    
    set widgetInfo($address-variable) [join $current_variable ";"]
    
    comparator_update $win $address
    
    eval $widgetInfo($address-check)
    return
}


proc comparator_getcolor {sim} {
    set color "black"
    switch $sim {
        "Sim.1" {
            set color "black"
        }
        "Sim.2" {
            set color "red"
        }
        "Sim.3" {
            set color "blue"
        }
        "Sim.4" {
            set color "green4"
        }  
    }
    return $color

}

proc comparator_dump {win address} {
    global widgetInfo
    set current_variable [split $widgetInfo($address-variable) ";"]
    
    #cleaning
    foreach filename [glob -nocomplain [file join {*}$widgetInfo($address-folder) *.gif] ] {
        file delete -force $filename
    }
    foreach filename [glob -nocomplain [file join {*}$widgetInfo($address-folder) *.html] ] {
        file delete -force $filename
    }
    foreach filename [glob -nocomplain [file join {*}$widgetInfo($address-folder) *.txt] ] {
        file delete -force $filename
    }
    
    # saving comparisons
    set index 1
    set items [llength $current_variable]
    foreach selectedpair $current_variable {
        set widgetInfo($address-statustxt) "Dumping $index/$items..."
        update idletasks
        
        set file1 [join [ split [ lindex [ split $selectedpair "@"] 0 ] "#" ] "_"]
        set file2 [join [ split [ lindex [ split $selectedpair "@"] 1 ] "#" ] "_"]
        grapher_create $win $address $win.gr.can $win.tv.tv [file join {*}$widgetInfo($address-folder)] "$file1" "$file2" 1
        incr index
    }
    
    # saving selection
    set widgetInfo($address-statustxt) "Dumping Selection Array..."
    update idletasks
    canvas_makegif $win.t.can  [file join {*}$widgetInfo($address-folder) "$widgetInfo($address-name).gif"]
    
    set widgetInfo($address-status) 1
    set widgetInfo($address-statustxt) "Comparisons data are saved!"
    $win.gr.c.statustxt configure -image icon_ok
}

proc comparator_showlocation {win address} {
    global widgetInfo
    $win.t.can delete "pointer"   
    set x [$win.t.can canvasx [expr {[winfo pointerx $win.t.can] - [winfo rootx $win.t.can]}]]
    set y [$win.t.can canvasy [expr {[winfo pointery $win.t.can] - [winfo rooty $win.t.can]}]]
    
    if {$x < 0} {return}
    if {$y < 0} {return}
    
    set boxsize $widgetInfo($address-boxsize)
    set col [expr {int($x*1.0/$boxsize)}]
    set row [expr {int($y*1.0/$boxsize)}]
    
    set maxcol [expr {[llength $widgetInfo($address-requiredValue) ]-1}]
    
    if {$col < $row} {return}
    if {$col > $maxcol} {return}
    if {$row > $maxcol} {return}
    
    set runcol [lindex $widgetInfo($address-requiredValue) $col]
    set runrow [lindex $widgetInfo($address-requiredValue) $row]  
    if {$col == $row} {
        set widgetInfo($address-position) "$runcol"
    } else {
        set widgetInfo($address-position) "$runrow \n .vs. \n $runcol "
    }
    canvas_text_highlighted $win.t.can $x $y  $widgetInfo($address-position) "pointer" 
}



proc comparator_check {win address} {
    global widgetInfo
    
    #canvas_makegif $win.t.can $address "$widgetInfo($address-name).gif"
    
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