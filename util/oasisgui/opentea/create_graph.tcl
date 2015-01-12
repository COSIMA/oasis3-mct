#  This program is under CECILL_B licence. See footer for details.




# GRAPH CREATION

proc graph_create { args } {
    set mandatory_arguments { path_father address }
    
    # Initializes the widget
    initWidget  
    
    set widgetInfo($address-status) 1
    set widgetInfo($address-message) ""
    set widgetInfo($address-accepted_keys) {"name" "xtitle" "ytitle" "color" "type" "size"}
    
    ttk::frame $win 
    eval $widgetInfo(packme-$win)

    set title [dTree_getAttribute $XMLtree $full_address_XML "title"]   
    ttk::label $win.lb -text "$title" 
    ttk::frame $win.t 
    
    pack $win.lb -side top -pady 3
    pack $win.t -side top -pady {0 5}
    
    set size_v [expr { (0.6)*$widgetInfo(guiSmallWidgetWidth)}] 
    set size_h [expr { (1)*$widgetInfo(guiSmallWidgetWidth)}] 
    set size [split [dTree_tryGetAttribute $XMLtree $full_address_XML_clean "size" "1;1" ] ";"]
    set size_h [expr {int([lindex $size 1]*1.*$size_h)}]
    set size_v [expr {int([lindex $size 1]*1.*$size_v)}]
    
    set widgetInfo($address-width) $size_h
    set widgetInfo($address-height) $size_v
    set widgetInfo($address-widthcanvas) $size_h
    set widgetInfo($address-heightcanvas) $size_v
    
    
    # graph part
    canvas $win.t.can  -yscrollcommand [list $win.t.sby set] -xscrollcommand [list $win.t.sbx set]   -width $size_h -height $size_v 
    ttk::scrollbar $win.t.sby -orient vertical -command [list $win.t.can yview]
    ttk::scrollbar $win.t.sbx -orient horizontal -command [list $win.t.can xview]
    
    grid $win.t.can -row 1 -column 0 -sticky news 
    grid $win.t.sby -row 1 -column 1 -sticky ns
    grid $win.t.sbx -row 2 -column 0 -sticky ew
    
    
    
    # controls part
    ttk::frame $win.c
    pack $win.c -side top -fill x
    # export csv
    ttk::button $win.c.csv  -image icon_clipboard -command [subst {
        clipboard clear
        clipboard append \[graph_var_to_csv $win $address\]
        success "Content of graph copied into clipboard in CSV format."
    }]
    balloon $win.c.csv  "Export CSV to clipboard"
    pack $win.c.csv -side left
    
    # export xmgrace
    ttk::button $win.c.xmgr  -image icon_graph -command [subst {
        clipboard clear
        clipboard append \[graph_var_to_xmgrace_project $win $address\]
        log "Try to open graph into XMGR"
    }]
    balloon $win.c.xmgr "Open in XMGRACE"
    pack $win.c.xmgr -side left
    
    
    # message label
    ttk::label $win.c.message -textvariable widgetInfo($address-message)
    pack $win.c.message -side left
    
    #add the check/refresh procedure to the bindings of the variable
   
    append widgetInfo($address-refresh) [ subst { graph_refresh $win $address}]
    append widgetInfo($address-check) [subst { graph_check $win $address}]
    finishWidget
    
    #trick?
    bind $win.t.can <ButtonPress-1> {+; focus %W}
    
    bind $win.t.can <Motion> [subst {graph_showlocation $win  $address}]
    
    #  zoom binding
    #bind $win.t.can <KeyPress-x> [ subst {graph_zoom $address $win 0.8 1.0}]
    #bind $win.t.can <KeyPress-X> [ subst {graph_zoom $address $win 1.2 1.0}]
    #bind $win.t.can <KeyPress-y> [ subst {graph_zoom $address $win 1.0 0.8}]
    #bind $win.t.can <KeyPress-Y> [ subst {graph_zoom $address $win 1.0 1.2}]
    bind $win.t.can <KeyPress-z> [ subst {graph_zoom $address $win 0.8 0.8}]
    bind $win.t.can <KeyPress-Z> [ subst {graph_zoom $address $win 1.2 1.2}]
    
    # ctrl move
    bind $win.t.can <Control-Motion> [ subst {graph_move $win $address %x %y }]
    
    graph_scrollupdate $win $address 1.0 1.0
    
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
}


proc graph_refresh {win address} {
    global widgetInfo
    #$win.t.can create rect 0 0 $widgetInfo($address-widthcanvas) $widgetInfo($address-heightcanvas)  -fill white -tags "xscalable yscalable"
   
    $win.t.can delete all
    
    set xmin 1000000000
    set xmax -1000000000
    set ymin 1000000000
    set ymax -1000000000
    set xtitle ""
    set ytitle ""
    
    # first pass, to get bounds
    foreach dataset [split $widgetInfo($address-variable) "#" ] {
        set ds_name [graph_parsecontent $address $dataset name]
        set ds_xtitle [graph_parsecontent $address $dataset xtitle]
        set ds_ytitle [graph_parsecontent $address $dataset ytitle]
        set ds_color [graph_parsecontent $address $dataset color]
        set ds_type [graph_parsecontent $address $dataset type]
        set dsraw [graph_parsecontent $address $dataset raw]
        set ds_length  [graph_parsecontent $address $dataset length]
        
        if {$ds_length <= 1} {
            set msg_err "Void datased in graph $address"
            warning $msg_err
            return 
        }
        
        
        lappend xtitle $ds_xtitle 
        lappend ytitle $ds_ytitle 
        
        for {set i 1} {$i <= $ds_length} {incr i} {
            set x [lindex $dsraw [expr {2*$i-2}]]
            set y [lindex $dsraw [expr {2*$i-1}]]
            if {$x>$xmax} {set xmax $x}
            if {$x<$xmin} {set xmin $x}
            if {$y>$ymax} {set ymax $y}
            if {$y<$ymin} {set ymin $y}
        }
        
    }

    
    set xtitle [join [lsort -unique $xtitle] ", "]
    set ytitle [join [lsort -unique $ytitle] ", "]
    
    if {$ymin == $ymax} {
        set ymin [expr {$ymin-1.0}]
        set ymax [expr {$ymax+1.0}]
    }
    if {$xmin == $xmax} {
        set xmin [expr {$xmin-1.0}]
        set xmax [expr {$xmax+1.0}]
    }
    
    
    # initialize bounds and scaling
    set widgetInfo($address-xmin) $xmin
    set widgetInfo($address-xmax) $xmax
    set widgetInfo($address-ymin) $ymin
    set widgetInfo($address-ymax) $ymax 
    
    set widgetInfo($address-xscale) [expr {0.75 *$widgetInfo($address-widthcanvas) /($widgetInfo($address-xmax)-$widgetInfo($address-xmin))}]
    set widgetInfo($address-yscale) [expr {0.75*$widgetInfo($address-heightcanvas)/($widgetInfo($address-ymax)-$widgetInfo($address-ymin))}]
   
    
   
   
    # draw frame an midlines
    set xmin_pix [graph_pix_x $address $xmin]
    set xmax_pix [graph_pix_x $address $xmax]
    set ymin_pix [graph_pix_y $address $ymin]
    set ymax_pix [graph_pix_y $address $ymax]
    set xzero_pix [graph_pix_x $address 0.0]
    set yzero_pix [graph_pix_y $address 0.0]
             
    
    # draw zero axis if any
    if {[expr {$xmin*$xmax}] < 0} {
         $win.t.can create line $xzero_pix $ymin_pix $xzero_pix $ymax_pix -width 2 -fill black -tags "xscalable yscalable axis"
    }
    if {[expr {$ymin*$ymax}] < 0} {
         $win.t.can create line $xmin_pix $yzero_pix $xmax_pix $yzero_pix -width 2 -fill black -tags "xscalable yscalable axis"
    }
    
    
    #$win.t.can create text $xmax_pix $ymin_pix -text "$xmax, $ymin" -anchor "se" -tags "xscalable yscalable"
    #$win.t.can create text $xmin_pix $ymax_pix -text "$xmin, $ymax" -anchor "nw" -tags "xscalable yscalable"
    foreach frac {0 0.2 0.4 0.6 0.8 1.0 } {
        set xfrac  [expr {$frac * $xmin + (1.-$frac)*$xmax}]
        set yfrac  [expr {$frac * $ymin + (1.-$frac)*$ymax}]
        set xfrac_pix [graph_pix_x $address $xfrac]
        set yfrac_pix [graph_pix_y $address $yfrac]
        $win.t.can create line $xfrac_pix  $ymin_pix $xfrac_pix $ymax_pix -fill grey80 -tags "xscalable yscalable axis"
        canvas_text_vector $win.t.can $xfrac_pix $ymin_pix [format %.4g $xfrac] nw 11 45 black  "xscalable yscalable axis"    
        $win.t.can create line $xmin_pix  $yfrac_pix $xmax_pix $yfrac_pix -fill grey80 -tags "xscalable yscalable axis"
        canvas_text_vector $win.t.can $xmin_pix $yfrac_pix [format %.4g $yfrac] sw 11 45 black  "xscalable yscalable axis"
    }
    
    
    set xtit [expr {0.5 * $xmin + 0.5*$xmax}]
    set ytit [expr {0.5 * $ymin + 0.5*$ymax}]
    set xtit_pix [graph_pix_x $address $xtit]
    set ytit_pix [graph_pix_y $address $ytit]
    #canvas_text_vector $win.t.can $xtit_pix $ymin_pix "$xtitle" nw 11 45 black  "xscalable yscalable axis" 
    #canvas_text_vector $win.t.can $xmin_pix $ytit_pix "$ytitle" sw 11 45  black "xscalable yscalable axis" 
    
    canvas_text_vector $win.t.can $xmax_pix $ymin_pix "$xtitle" sw 11 -90 black  "xscalable yscalable axis" 
    canvas_text_vector $win.t.can $xmin_pix $ymax_pix "$ytitle" se 11 0  black "xscalable yscalable axis" 
        
    
    
# second pass pass, to draw 
    foreach dataset [split $widgetInfo($address-variable) "#" ] {
        
        set ds_name [graph_parsecontent $address $dataset name]
        set ds_tag [string map {" " "_"} $ds_name]
        set ds_xtitle [graph_parsecontent $address $dataset xtitle]
        set ds_ytitle [graph_parsecontent $address $dataset ytitle]
        set ds_color [graph_parsecontent $address $dataset color]
        set ds_type [graph_parsecontent $address $dataset type]
        set dsraw [graph_parsecontent $address $dataset raw]
        set ds_length  [graph_parsecontent $address $dataset length]
        set size  [graph_parsecontent $address $dataset size]
       
       
        
        set ds_rawpix ""
        
        set tags "xscalable yscalable $ds_tag"
        set h [expr {$size*5}]    
        for {set i 1} {$i <= $ds_length} {incr i} {
            set shade [expr {1.0*($ds_length-$i)/($ds_length-1)}]
            set x [lindex $dsraw [expr {2*$i-2}]]
            set y [lindex $dsraw [expr {2*$i-1}]]
            set xpix [graph_pix_x $address $x]
            set ypix [graph_pix_y $address $y]
            
            
            # Symbols
            if {"circle" in $ds_type} {
                $win.t.can create oval [expr {$xpix-$h}] [expr {$ypix-$h}] [expr {$xpix+$h}] [expr {$ypix+$h}] -fill [shadeColor $ds_color $shade]   -width 0 -tags $tags -activewidth 3
                $win.t.can create oval [expr {$xpix-$h}] [expr {$ypix-$h}] [expr {$xpix+$h}] [expr {$ypix+$h}] -outline  $ds_color -tags $tags -activewidth 3
            }
            if {"square" in $ds_type} {
                $win.t.can create rectangle [expr {$xpix-$h}] [expr {$ypix-$h}] [expr {$xpix+$h}] [expr {$ypix+$h}] -fill [shadeColor $ds_color $shade] -width 0 -tags $tags -activewidth 3
                $win.t.can create rectangle [expr {$xpix-$h}] [expr {$ypix-$h}] [expr {$xpix+$h}] [expr {$ypix+$h}]  -outline  $ds_color -tags $tags -activewidth 3
                
            }
            if {"triangle" in $ds_type} {
                $win.t.can create polygon [expr {$xpix-$h}] [expr {$ypix-$h}] [expr {$xpix+$h}] [expr {$ypix-$h}] [expr {$xpix}] [expr {$ypix+$h}] -fill [shadeColor $ds_color $shade] -width 0  -tags $tags -activewidth 3
                $win.t.can create line [expr {$xpix-$h}] [expr {$ypix-$h}] [expr {$xpix+$h}] [expr {$ypix-$h}] [expr {$xpix}] [expr {$ypix+$h}] [expr {$xpix-$h}] [expr {$ypix-$h}] -fill $ds_color -tags $tags -activewidth 3
                
            }
            if {"diamond" in $ds_type} {
                $win.t.can create polygon $xpix [expr {$ypix-$h}] [expr {$xpix+$h}] $ypix $xpix [expr {$ypix+$h}] [expr {$xpix-$h}] $ypix  -fill [shadeColor $ds_color $shade] -width 0  -tags $tags -activewidth 3
                $win.t.can create line $xpix [expr {$ypix-$h}] [expr {$xpix+$h}] $ypix $xpix [expr {$ypix+$h}] [expr {$xpix-$h}] $ypix  $xpix [expr {$ypix-$h}]  -fill $ds_color -tags $tags -activewidth 3
                
            }           
            # line to zero
            if {"toy0" in $ds_type} {
                $win.t.can create line [expr {$xpix}] [expr {$yzero_pix}] [expr {$xpix}] [expr {$ypix}] -fill $ds_color  -tags $tags -activewidth 3
            }
            
            # labels 
            if {"lblall" in $ds_type} {
                canvas_text_vector $win.t.can $xpix $ypix "$x, $y" se 9 45 $ds_color $tags
            }
            
            # line
            if { "line" in $ds_type} {
                lappend ds_rawpix $xpix $ypix
            }
        }
        
        #last label
        if {"lbllast" in $ds_type} {
                canvas_text_vector $win.t.can $xpix $ypix "$x, $y" se 9 45 $ds_color $tags
        }
        
        # line
        if {"line" in $ds_type} {
            $win.t.can create line $ds_rawpix -fill $ds_color -width 2 -activewidth 3 -tags $tags
        }
        
        $win.t.can bind $ds_tag <Enter> [subst {
            set widgetInfo($address-message) "Dataset : $ds_name"
        }]
        $win.t.can bind $ds_tag <Leave> [subst {set widgetInfo($address-message) ""}]
        
        
        
    }
   
    $win.t.can lower "axis"
   
    $win.t.can configure -scrollregion [ $win.t.can bbox all]
    
    smartpacker_update_visibility $win $address
    
    #canvas_makegif $win.t.can "$widgetInfo($address-name).gif"
    
}


proc graph_parsecontent {address curve_data arg} {
    global widgetInfo
    
    set ds [split $curve_data ";"]
    set dsi(name) "no name"
    set dsi(xtitle) "no xtitle"
    set dsi(ytitle) "no ytitle"
    set dsi(color) "black"
    set dsi(type) "circle_line"
    set dsi(size) "1"
    
    set dsi(raw) ""
    foreach item $ds {
        if {[string match "*=*" $item] == 1} {
            set key [lindex [split $item "="]  0]
            set value [lindex [split $item "="] 1]
            if {$key in $widgetInfo($address-accepted_keys)} {
                set dsi($key)  $value
            } else {
                debug "Graph key $key not recognized among $widgetInfo($address-accepted_keys)"
            }
        } else {
            if {[string is double $item]} {
                lappend dsi(raw) $item
            } {
                log "Warning : graph $address ; item $item not understood"
            }
        }
    }
    set dsi(type) [split $dsi(type) "_"]
    set dsi(length) [expr {0.5*[llength $dsi(raw)]}]
    return $dsi($arg)
}

proc graph_pix_x { address x } {
    global widgetInfo
    set xpix [expr {($x -$widgetInfo($address-xmin)) * $widgetInfo($address-xscale)}]
    return $xpix
}
proc graph_pix_y { address y} {
    global widgetInfo
    set ypix [expr {($widgetInfo($address-ymax) -$y) * $widgetInfo($address-yscale)}]
    return $ypix
    
}
proc graph_pix_realx { address xpix} {
    global widgetInfo
    set x [expr {$widgetInfo($address-xmin) + $xpix/$widgetInfo($address-xscale)}]
    return $x
}
proc graph_pix_realy { address ypix} {
    global widgetInfo
    set y [expr {$widgetInfo($address-ymax) - $ypix/$widgetInfo($address-yscale)}]
    return $y
}

proc graph_showlocation {win address} {
    global widgetInfo
    $win.t.can delete "pointer"
    set x [$win.t.can canvasx [expr {[winfo pointerx $win.t.can] - [winfo rootx $win.t.can]}]]
    set y [$win.t.can canvasy [expr {[winfo pointery $win.t.can] - [winfo rooty $win.t.can]}]]
    set realx [graph_pix_realx  $address $x  ]
    set realy [graph_pix_realy  $address $y  ]
    set widgetInfo($address-position)  "[format %+.4f $realx]\n[format %+.4f $realy]"
    canvas_text_highlighted $win.t.can $x $y  $widgetInfo($address-position) "pointer"
}



proc graph_scrollupdate { win address xzoom yzoom} {
    global widgetInfo
    set width $widgetInfo($address-width)
    set height $widgetInfo($address-height)
    set widthcanvas $widgetInfo($address-widthcanvas)
    set heightcanvas $widgetInfo($address-heightcanvas)
    
    set widthcanvas [expr  $widthcanvas*$xzoom ]
    set heightcanvas [expr $heightcanvas*$yzoom ]
    
    set widgetInfo($address-widthcanvas) $widthcanvas
    set widgetInfo($address-heightcanvas) $heightcanvas
    
    set scroll_factor_x [expr 1.0/$width*(1.0-$width*1.0/$widthcanvas)]
    if { $width > $widthcanvas} { set scroll_factor_x 0}
    set widgetInfo($address-scroll_factor_x) $scroll_factor_x
    set scroll_factor_y [expr 1.0/$height*(1.0-$height*1.0/$heightcanvas)]
    if { $height > $heightcanvas} { set scroll_factor_y 0}
    set widgetInfo($address-scroll_factor_y) $scroll_factor_y
    
}

# to move the canvases using Control_move binding
proc graph_zoom { address win xzoom yzoom} {
    global widgetInfo
     set y0 0
     set x0 0
     $win.t.can scale xscalable $x0 $y0 $xzoom 1
     $win.t.can scale yscalable $x0 $y0 1 $yzoom
     
     set widgetInfo($address-xscale) [expr {$widgetInfo($address-xscale) * $xzoom} ]
     set widgetInfo($address-yscale) [expr {$widgetInfo($address-yscale) * $yzoom} ]
     
     $win.t.can configure -scrollregion [ $win.t.can bbox all]
     graph_scrollupdate $win $address $xzoom $yzoom
   
}


proc graph_var_to_csv {win address} {
    global widgetInfo
    # first pass, to get bounds
    set maxrow 0
    set col 0
    set colp1 1
    
    foreach dataset [split $widgetInfo($address-variable) "#" ] {
        
        set ds_name [graph_parsecontent $address $dataset name]
        set ds_xtitle [graph_parsecontent $address $dataset xtitle]
        set ds_ytitle [graph_parsecontent $address $dataset ytitle]
        set dsraw [graph_parsecontent $address $dataset raw]
        set ds_length  [graph_parsecontent $address $dataset length]
        
        
        set csv_data(0-$col) $ds_name
        set csv_data(1-$col) $ds_xtitle
        set csv_data(1-$colp1) $ds_ytitle     
        set row 2
        foreach {x y} $dsraw {
            set csv_data($row-$col) $x
            set csv_data($row-$colp1) $y
            incr row
        }
        
        incr col 2
        incr colp1 2
        set maxrow [expr {max($row,$maxrow)}]
        
    }
    set result ""
    
    set cell_list [array names csv_data]
    for {set r 0 } {$r < $maxrow } {incr r} {
        set line ""
        for {set c 0 } {$c < $colp1 } {incr c} {
            set cell "$r-$c"
            if {$cell in $cell_list} {
                lappend line $csv_data($cell)
            } else {
                lappend line {}
            }
        }
        set result "$result [join $line ";" ] \n"
    }
    
    array unset csv_data
    return  $result
}


proc graph_var_to_xmgrace_project {win address} {
    global widgetInfo
    
    set result ""
    lappend result "# Grace project file"
    lappend result "# "
    lappend result "@with g0"
    lappend result "@    legend on"
    
    
    # first loop to setup the graphs
    set ds_id 0
    foreach dataset [split $widgetInfo($address-variable) "#" ] {
        set ds_name [graph_parsecontent $address $dataset name]
        set ds_xtitle [graph_parsecontent $address $dataset xtitle]
        set ds_ytitle [graph_parsecontent $address $dataset ytitle]
        set ds_color [graph_parsecontent $address $dataset color]
        set ds_type [graph_parsecontent $address $dataset type]
        
        # convert color
        set xm_col 1
        switch $ds_color {
            "black" {
                set xm_col 1
            }
            "red" {
                set xm_col 2
            }
            "blue" {
                set xm_col 4
            }
            "green4" {
                set xm_col 15
            }
        }
        # convert symbol
        set xm_sym 0
        if {"circle" in $ds_type} {set xm_sym 1}
        if {"square" in $ds_type} {set xm_sym 2}
        if {"triangle" in $ds_type} {set xm_sym 6}
        if {"diamond" in $ds_type} {set xm_sym 3}
        
        lappend result "@    xaxis  label \"$ds_xtitle\""
        lappend result "@    yaxis  label \"$ds_ytitle\""
        lappend result "@    s$ds_id  symbol $xm_sym"
        lappend result "@    s$ds_id  symbol color $xm_col"
        lappend result "@    s$ds_id  symbol fill color $xm_col"
        lappend result "@    s$ds_id  symbol fill pattern 8"
        lappend result "@    s$ds_id  line color $xm_col"
        lappend result "@    s$ds_id  baseline off"
        lappend result "@    s$ds_id  dropline off"
        lappend result "@    s$ds_id  legend \"$ds_name\""
        
        incr ds_id
    }
    
     # second loop to fill the data
    set ds_id 0
    foreach dataset [split $widgetInfo($address-variable) "#" ] {
        set dsraw [graph_parsecontent $address $dataset raw]
        lappend result "@target G0.S$ds_id"
        lappend result "@type xy"
        foreach {x y} $dsraw {
            lappend result "$x $y"
        }
        lappend result "&"
        incr ds_id
    }
    
    
    set result [join $result "\n"]
    array unset xmgr_data
    set xmgracefile [open "./tmp.agr" w+]
    puts $xmgracefile $result
    close $xmgracefile
    exec_script "xmgrace ./tmp.agr"
    return  
}


# to move the canvases using Control_move binding
proc graph_move { win address x y} {
    global widgetInfo
    set xv [expr $x * $widgetInfo($address-scroll_factor_x)]
    set yv [expr $y * $widgetInfo($address-scroll_factor_y)]
    $win.t.can xview moveto $xv
    $win.t.can yview moveto $yv
}


proc graph_check {win address} {
    global widgetInfo
   
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