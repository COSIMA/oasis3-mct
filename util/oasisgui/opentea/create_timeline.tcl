#  This program is under CECILL_B licence. See footer for details.




# TIMELINE CREATION

proc timeline_create { args } {
    set mandatory_arguments { path_father address }
    
    # Initializes the widget
    initWidget  
    
    set widgetInfo($address-status) 1
    set widgetInfo($address-message) ""
    set widgetInfo($address-accepted_keys) {"name" "run" "start" "end" "color" "comment" "sce"}
    
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
    
    
    # timeline part
    canvas $win.t.can  -yscrollcommand [list $win.t.sby set] -xscrollcommand [list $win.t.sbx set]   -width $size_h -height $size_v 
    ttk::scrollbar $win.t.sby -orient vertical -command [list $win.t.can yview]
    ttk::scrollbar $win.t.sbx -orient horizontal -command [list $win.t.can xview]
    
    grid $win.t.can -row 1 -column 0 -sticky news 
    grid $win.t.sby -row 1 -column 1 -sticky ns
    grid $win.t.sbx -row 2 -column 0 -sticky ew
    
    
    
    # controls part
    ttk::frame $win.c
    pack $win.c -side top -fill x
    
    
    
    # message label
    ttk::label $win.c.message -textvariable widgetInfo($address-message)
    pack $win.c.message -side left
    
    #add the check/refresh procedure to the bindings of the variable
   
    append widgetInfo($address-refresh) [ subst { timeline_refresh $win $address}]
    append widgetInfo($address-check) [subst { timeline_check $win $address}]
    finishWidget
    
    #trick?
    bind $win.t.can <ButtonPress-1> {+; focus %W}
    
    bind $win.t.can <Motion> [subst {timeline_showlocation $win  $address}]
    
    #  zoom binding
    #bind $win.t.can <KeyPress-x> [ subst {timeline_zoom $address $win 0.8 1.0}]
    #bind $win.t.can <KeyPress-X> [ subst {timeline_zoom $address $win 1.2 1.0}]
    #bind $win.t.can <KeyPress-y> [ subst {timeline_zoom $address $win 1.0 0.8}]
    #bind $win.t.can <KeyPress-Y> [ subst {timeline_zoom $address $win 1.0 1.2}]
    bind $win.t.can <KeyPress-z> [ subst {timeline_zoom $address $win 0.8 0.8}]
    bind $win.t.can <KeyPress-Z> [ subst {timeline_zoom $address $win 1.2 1.2}]
    
    # ctrl move
    bind $win.t.can <Control-Motion> [ subst {timeline_move $win $address %x %y }]
    
    timeline_scrollupdate $win $address 1.0 1.0
    
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
}


proc timeline_refresh {win address} {
    global widgetInfo
    #$win.t.can create rect 0 0 $widgetInfo($address-widthcanvas) $widgetInfo($address-heightcanvas)  -fill white -tags "xscalable yscalable"
   
    $win.t.can delete all
    
    set xmin 1000000000
    set xmax -1000000000
    set ymin 0
    set ymax 0
    set xtitle ""
    set ytitle ""
    
    # first pass, to get bounds
    foreach dataset [split $widgetInfo($address-variable) "#" ] {
        set ds_start [timeline_parsecontent $address $dataset "start"]
        set ds_end [timeline_parsecontent $address $dataset "end"]      
        if {$ds_end>$xmax} {set xmax $ds_end}
        if {$ds_start<$xmin} {set xmin $ds_start}
        incr ymax    
        
    }

    
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
    
    set widgetInfo($address-xscale) [expr {0.8 *$widgetInfo($address-widthcanvas) /($widgetInfo($address-xmax)-$widgetInfo($address-xmin))}]
    set widgetInfo($address-yscale) [expr {0.8*$widgetInfo($address-heightcanvas)/($widgetInfo($address-ymax)-$widgetInfo($address-ymin))}]
   
    
   
   
    # draw frame an midlines
    set xmin_pix [timeline_pix_x $address $xmin]
    set xmax_pix [timeline_pix_x $address $xmax]
    set ymin_pix [timeline_pix_y $address $ymin]
    set ymax_pix [timeline_pix_y $address $ymax]
    set xzero_pix [timeline_pix_x $address 0.0]
             
    
    #$win.t.can create text $xmax_pix $ymin_pix -text "$xmax, $ymin" -anchor "se" -tags "xscalable yscalable"
    #$win.t.can create text $xmin_pix $ymax_pix -text "$xmin, $ymax" -anchor "nw" -tags "xscalable yscalable"
    foreach frac {0 0.2 0.4 0.6 0.8 1 } {
        set xfrac  [expr {$frac * $xmin + (1.-$frac)*$xmax}]
        set xfrac_pix [timeline_pix_x $address $xfrac]
        $win.t.can create line $xfrac_pix  $ymin_pix $xfrac_pix $ymax_pix -fill grey80 -tags "xscalable yscalable axis"
        canvas_text_vector $win.t.can $xfrac_pix $ymin_pix [format %.4g $xfrac] se 11 45 black  "xscalable yscalable axis"    
    }
    
    
    set xtit [expr {0.5 * $xmin + 0.5*$xmax}]
    set xtit_pix [timeline_pix_x $address $xtit]
    canvas_text_vector $win.t.can $xtit_pix $ymin_pix "Time \[s\]" se 11 45 black  "xscalable yscalable axis" 
   
        
    
    
# second pass pass, to draw
    set ystart 0
    foreach dataset [split $widgetInfo($address-variable) "#" ] {
        
        set ds_name [timeline_parsecontent $address $dataset "name"]
        set ds_color [timeline_parsecontent $address $dataset "color"]
        set ds_run [timeline_parsecontent $address $dataset "run"]
        set ds_start [timeline_parsecontent $address $dataset "start"]
        set ds_end [timeline_parsecontent $address $dataset "end"]
        set ds_comment [timeline_parsecontent $address $dataset "comment"]
        set ds_sce  [timeline_parsecontent $address $dataset "sce"]
  
        
            
        set xpix1 [timeline_pix_x $address $ds_start]
        set xpix2 [timeline_pix_x $address $ds_end]
        set ypixm [timeline_pix_y $address [expr {$ystart +0.5}]]
        set ypix1 [timeline_pix_y $address [expr {$ystart +0.2}]]
        set ypix2 [timeline_pix_y $address [expr {$ystart +0.8}]]
        if {[expr {$ypix2-$ypix1}] > 20} {
            set ypix1 [expr {$ypixm - 10}]
            set ypix2 [expr {$ypixm + 10}]
        }
        
        incr ystart 
        
        set shorttitle  "$ds_name#$ds_run"
        set description  "$ds_name $ds_run $ds_comment"
        
       
        set tags "xscalable yscalable $shorttitle "
        set h 5
        
        $win.t.can create rectangle $xpix1 $ypix1 $xpix2 $ypix2 -fill $ds_color -outline black -width 1 -tags $tags -activewidth 3
        $win.t.can create oval [expr {$xpix1-$h}] [expr {$ypixm-$h}] [expr {$xpix1+$h}] [expr {$ypixm+$h}] -fill black -outline black -width 1 -tags $tags -activewidth 3
        canvas_text_vector $win.t.can $xpix2 $ypixm "$shorttitle" e 11 0 black  "xscalable yscalable $shorttitle"        
        $win.t.can  bind $shorttitle <Enter> [subst {set widgetInfo($address-message) "$description" }]
        $win.t.can  bind $shorttitle <Leave> [subst {set widgetInfo($address-message) ""}]
        
    }
   
    $win.t.can lower "axis"
   
    $win.t.can configure -scrollregion [ $win.t.can bbox all]
    
    smartpacker_update_visibility $win $address
    
    #canvas_makegif $win.t.can "$widgetInfo($address-name).gif"
    
}


proc timeline_parsecontent {address curve_data arg} {
    global widgetInfo
    
    set ds [split $curve_data ";"]
    set dsi(name) "NAME???"
    set dsi(run) "RUN_???"
    set dsi(color) "grey"
    set dsi(comment) ""
    set dsi(start) "0.0"
    set dsi(end) "0.0"
    set dsi(sce) "0"
    
    set dsi(raw) ""
    
    foreach item $ds {
        if {[string match "*=*" $item] == 1} {
            set key [lindex [split $item "="]  0]
            set value [lindex [split $item "="] 1]
            if {$key in $widgetInfo($address-accepted_keys)} {

                if {$key in {"start" "end"} } {
                    if {![string is double $value]} {
                        set value "0.0"
                    }
                }
                set dsi($key)  $value
            } else {
                debug "Timeline key $key not recognized among $widgetInfo($address-accepted_keys)"
            }
        } else {
            lappend dsi(raw) $item
        }
    
    }
    
    
    if {$dsi(end) < $dsi(start)} {
        set dsi(end) $dsi(start)
    }
    
    return $dsi($arg)
}

proc timeline_pix_x { address x } {
    global widgetInfo
    set xpix [expr {($x -$widgetInfo($address-xmin)) * $widgetInfo($address-xscale)}]
    return $xpix
}
proc timeline_pix_y { address y} {
    global widgetInfo
    set ypix [expr {($y - $widgetInfo($address-ymax) ) * $widgetInfo($address-yscale)}]
    return $ypix
    
}
proc timeline_pix_realx { address xpix} {
    global widgetInfo
    set x [expr {$widgetInfo($address-xmin) + $xpix/$widgetInfo($address-xscale)}]
    return $x
}
proc timeline_pix_realy { address ypix} {
    global widgetInfo
    set y [expr {$widgetInfo($address-ymax) - $ypix/$widgetInfo($address-yscale)}]
    return $y
}

proc timeline_showlocation {win address} {
    global widgetInfo
    $win.t.can delete "pointer"
    set x [$win.t.can canvasx [expr {[winfo pointerx $win.t.can] - [winfo rootx $win.t.can]}]]
    set y [$win.t.can canvasy [expr {[winfo pointery $win.t.can] - [winfo rooty $win.t.can]}]]
   
    set realx [timeline_pix_realx  $address $x  ]
    set widgetInfo($address-position)  "[format %+.4f $realx]"
    canvas_text_highlighted $win.t.can $x $y  $widgetInfo($address-position) "pointer"
}



proc timeline_scrollupdate { win address xzoom yzoom} {
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
proc timeline_zoom { address win xzoom yzoom} {
    global widgetInfo
     set y0 0
     set x0 0
     $win.t.can scale xscalable $x0 $y0 $xzoom 1
     $win.t.can scale yscalable $x0 $y0 1 $yzoom
     
     set widgetInfo($address-xscale) [expr {$widgetInfo($address-xscale) * $xzoom} ]
     set widgetInfo($address-yscale) [expr {$widgetInfo($address-yscale) * $yzoom} ]
     
     $win.t.can configure -scrollregion [ $win.t.can bbox all]
     timeline_scrollupdate $win $address $xzoom $yzoom
   
}





# to move the canvases using Control_move binding
proc timeline_move { win address x y} {
    global widgetInfo
    set xv [expr $x * $widgetInfo($address-scroll_factor_x)]
    set yv [expr $y * $widgetInfo($address-scroll_factor_y)]
    $win.t.can xview moveto $xv
    $win.t.can yview moveto $yv
}


proc timeline_check {win address} {
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