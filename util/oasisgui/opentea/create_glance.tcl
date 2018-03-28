#  This program is under CECILL_B licence. See footer for details.




# GLANCE CREATION

proc glance_create { args } {
    set mandatory_arguments { path_father address }
    
    # Initializes the widget
    initWidget  
    
    
    
    
    # TACKLE SIZES
    set size_v [expr { (0.9)*$widgetInfo(guiSmallWidgetWidth)}] 
    set size_h [expr { (0.9)*$widgetInfo(guiSmallWidgetWidth)}] 
    set size [split [dTree_tryGetAttribute $XMLtree $full_address_XML_clean "size" "1;1" ] ";"]
    set size_h [expr {int([lindex $size 0]*1.*$size_h)}]
    set size_v [expr {int([lindex $size 1]*1.*$size_v)}]
    
    set widgetInfo($address-width) $size_h
    set widgetInfo($address-height) $size_v
    set widgetInfo($address-widthcanvas) $size_h
    set widgetInfo($address-heightcanvas) $size_v
    
    set widgetInfo($address-activeselection) ""
    set widgetInfo($address-selection) ""
    set widgetInfo($address-dataids) ""
    set widgetInfo($address-showdiam) 0
        
    
    
    
    
    
    
    set widgetInfo($address-status) 1
    set widgetInfo($address-zoom) "Zoom: 100"
    
    set widgetInfo($address-showglance) 0
    set widgetInfo($address-message) "Hello world!"
    set widgetInfo($address-rendermessage) ""
    
    set widgetInfo($address-dataids) ""
    
    
    set widgetInfo($address-directions) "x y z" 
    
    set listproj "{x y} {x z} {z y}"
    set listcolor "x y z"
    set listrender "nodes cells"
    set widgetInfo($address-projection) [lindex $listproj 0]
    set widgetInfo($address-color) [lindex $listcolor 0]
    set widgetInfo($address-colormin) 0
    set widgetInfo($address-colormax) 1
    set widgetInfo($address-render) [lindex $listrender 0]
    set widgetInfo($address-clip) [lindex $widgetInfo($address-directions) 0]
    set widgetInfo($address-clipmin) 0
    set widgetInfo($address-clipmax) 1
    
    
    ttk::frame $win -relief sunken 
    eval $widgetInfo(packme-$win)

    set title [dTree_getAttribute $XMLtree $full_address_XML "title"]   
    ttk::checkbutton $win.cb -text "$title (on/off)" -variable widgetInfo($address-showglance) -command [subst {glance_show_onoff $win $address}]
    pack $win.cb -side top -pady 3
    
    
    # fenetre de texte
    ttk::frame $win.txt -width $size_h
   
    
    ttk::label $win.txt.messagehead -text "Selection"
    pack $win.txt.messagehead -side top 
    
    ttk::separator $win.txt.sep 
    pack $win.txt.sep -side top -fill x
    
    # message label
    ttk::label $win.txt.message -textvariable widgetInfo($address-message) -wraplength $size_h -width 60
    pack $win.txt.message -side top -padx 2
    
    
    # fenetre de visu
    ttk::frame $win.t 
   
    # fenetre de controle de vie
    ttk::frame $win.t.v
    
    ttk::button $win.t.v.zoomplus -text "Zoom+" -command [subst { glance_zoom $win $address 2. }]
    pack $win.t.v.zoomplus -side top
    ttk::button $win.t.v.zoommin -text "Zoom-" -command [subst { glance_zoom $win $address 0.5  }]
    pack $win.t.v.zoommin -side top
    ttk::button $win.t.v.zoomreset -text "Zoom auto" -command [subst { glance_zoom $win $address 1 }]
    pack $win.t.v.zoomreset -side top
    ttk::label  $win.t.v.zlbl -textvariable widgetInfo($address-zoom)
    pack $win.t.v.zlbl -side top
    ttk::label $win.t.v.mess -textvariable widgetInfo($address-rendermessage)
    pack $win.t.v.mess -side bottom
    
    
    
    # GLANCE PART
    canvas $win.t.can  -yscrollcommand [list $win.t.sby set] -xscrollcommand [list $win.t.sbx set]   -width $size_h -height $size_v 
    ttk::scrollbar $win.t.sby -orient vertical -command [list $win.t.can yview]
    ttk::scrollbar $win.t.sbx -orient horizontal -command [list $win.t.can xview]
    #bind $win.t.can   <ButtonPress> [subst {$win.t.can scan mark %x %y}]
    #bind $win.t.can   <B1-Motion> [subst {$win.t.can scan dragto %x %y 1}]
    # render label
    
    
    
    grid $win.t.v -row 1 -column 0 -sticky news 
    grid $win.t.can -row 1 -column 1 -sticky news 
    grid $win.t.sby -row 1 -column 2 -sticky ns
    grid $win.t.sbx -row 2 -column 1 -sticky ew
    
    
    
    
    
    
    # controls part
    ttk::frame $win.c
    
    
    # col 1
    set column 0
    set row 0
    # projection
    ttk::label $win.c.projlabel -text "Projection"
    grid $win.c.projlabel -column $column -row $row
    incr row
    
    ttk::combobox $win.c.projection -textvariable widgetInfo($address-projection) -values $listproj -state readonly
    grid $win.c.projection -column $column -row $row
    incr row
    bind  $win.c.projection <<ComboboxSelected>> [subst {
        set widgetInfo($address-zoom) "Zoom: 100"
        glance_reset_bounds $win $address
        glance_render $win $address
    }]
    
    
    # render
    ttk::label $win.c.rendlabel -text "Render"
    grid $win.c.rendlabel -column $column -row $row
    incr row
    ttk::combobox $win.c.render -textvariable widgetInfo($address-render) -values $listrender -state readonly
    
    grid $win.c.render -column $column -row $row
    incr row
    bind  $win.c.render <<ComboboxSelected>> [subst {
        glance_render $win $address
    }]
    
    
    
    # col 2
    set column 1
    set row 0
    # color
    ttk::label $win.c.colorlabel -text "Color"
    grid $win.c.colorlabel -column $column -row $row
    incr row
    ttk::combobox $win.c.color -textvariable widgetInfo($address-color) -values $listcolor -state readonly
    grid $win.c.color -column $column -row $row
    incr row
    bind  $win.c.color <<ComboboxSelected>> [subst {
        glance_resets $win $address "color"
        glance_render $win $address
    }]
    
    # colormin
    ttk::label $win.c.colorminlabel -text "Min"
    grid $win.c.colorminlabel -column $column -row $row
    incr row
    ttk::entry $win.c.colorvmin -textvariable widgetInfo($address-colormin)
    
    bind  $win.c.colorvmin <Return> [subst {
        if {\$widgetInfo($address-colormin) < \$widgetInfo($address-colormax)} {
            glance_render $win $address
        } else {
            warning "Color min must be smaller than color max"
            set widgetInfo($address-colormin) \$widgetInfo($address-colormax)
        } 
    }]
    
    grid $win.c.colorvmin -column $column -row $row
    incr row
    
    # colormax
    ttk::label $win.c.colormaxlabel -text "Max"
    grid $win.c.colormaxlabel -column $column -row $row
    incr row
    ttk::entry $win.c.colorvmax -textvariable widgetInfo($address-colormax)
    bind  $win.c.colorvmax <Return> [subst {
        if {\$widgetInfo($address-colormin) < \$widgetInfo($address-colormax)} {
            glance_render $win $address
        } else {
            warning "Color min must be smaller than color max"
            set widgetInfo($address-colormax) \$widgetInfo($address-colormin)
        } 
    }]
    grid $win.c.colorvmax -column $column -row $row
    incr row
    # colorreset
    ttk::button $win.c.colorreset -text "Color Reset"  -command [ subst { glance_resets $win $address "color"}]
    grid $win.c.colorreset -column $column -row $row
    incr row
    
    
    # col 3
    set column 2
    set row 0
    # clip dir 
    ttk::label $win.c.cliplabel -text "Clip direction"
    grid $win.c.cliplabel -column $column -row $row
    incr row
    ttk::combobox $win.c.clip -textvariable widgetInfo($address-clip) -values "" -state readonly
    grid $win.c.clip -column $column -row $row
    incr row
    bind  $win.c.clip <<ComboboxSelected>> [subst {
        set widgetInfo($address-zoom) "Zoom: 100"
        glance_resets $win $address "clip"
        glance_render $win $address
    }]
    
    # clipmin
    ttk::label $win.c.clipminlabel -text "Min"
    grid $win.c.clipminlabel -column $column -row $row
    incr row
    ttk::entry $win.c.clipmin -textvariable widgetInfo($address-clipmin)
    bind  $win.c.clipmin <Return> [subst {
        if {\$widgetInfo($address-clipmin) < \$widgetInfo($address-clipmax)} {
            set widgetInfo($address-zoom) "Zoom: 100"
            glance_render $win $address
        } else {
            warning "Clip min must be smaller than clip max"
            set widgetInfo($address-clipmin) \$widgetInfo($address-clipmax)
        } 
    }]
    grid $win.c.clipmin -column $column -row $row
    incr row
    
    # clipmax
    ttk::label $win.c.clipmaxlabel -text "Max"
    grid $win.c.clipmaxlabel -column $column -row $row
    incr row
    ttk::entry $win.c.clipmax -textvariable widgetInfo($address-clipmax)
    bind  $win.c.clipmax <Return> [subst {
        if {\$widgetInfo($address-clipmin) < \$widgetInfo($address-clipmax)} {
            set widgetInfo($address-zoom) "Zoom: 100"
            glance_render $win $address
        } else {
            warning "Clip min must be smaller than clip max"
            set widgetInfo($address-clipmax) \$widgetInfo($address-clipmin)
        } 
    }]
    grid $win.c.clipmax -column $column -row $row
    incr row
    
    # clipreset
    ttk::button $win.c.clipreset -text "Clip Reset" -command  [ subst {
        glance_resets $win $address "clip"
    }]
    grid $win.c.clipreset -column $column -row $row  
    incr row
    
    
    
    
    
    
  
    
    
    
    
    #add the check/render procedure to the bindings of the variable
    append widgetInfo($address-refresh) [ subst { glance_refresh $win $address}]
    append widgetInfo($address-check) [subst { glance_check $win $address}]
    finishWidget
    
    #trick?
    bind $win.t.can <ButtonPress-1> {+; focus %W}
    
    
    
    # ctrl move
    bind $win.t.can <Control-Motion> [ subst {glance_move $win $address %x %y }]
    
    
    glance_scrollupdate $win $address 1.0 1.0
    
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
}


proc glance_show_onoff {win address} {
    global widgetInfo

    switch $widgetInfo($address-showglance) {
        1 {
            pack $win.c -side bottom -fill x -padx 3 -pady 3
            pack $win.txt -side right -anchor n -padx 3 -pady 3
            pack $win.t -padx 3 -pady 3
        }
        0 {
            glance_purge $win $address
            pack forget $win.txt 
            pack forget $win.t 
            pack forget $win.c
            
            foreach dataid $widgetInfo($address-dataids) {
                array unset widgetInfo "$address-$dataid*"
                lremove widgetInfo($address-dataids) $dataid
                set  widgetInfo($address-message) "$widgetInfo($address-message) Removing dataset $dataid \n"
            }
            
        }
    }
    event generate . <<SetView>>
    glance_refresh $win $address
}


proc glance_refresh {win address} {
    global widgetInfo
    # clean dataids if not in the variable
    
    if { $widgetInfo($address-showglance) == 0 } {
        return
    }
    set widgetInfo($address-message) " Refreshing data... \n\n"
    
    foreach dataid $widgetInfo($address-dataids) {
        array unset widgetInfo "$address-$dataid*"
        lremove widgetInfo($address-dataids) $dataid
        set  widgetInfo($address-message) "$widgetInfo($address-message) Removing dataset $dataid \n"
    }
    
    set i 0
    # parse dataids (skip first which is time)
    foreach item $widgetInfo($address-variable)  {
        if {$i ==1} {
            set dataid $item
        }
        if {$i ==2} {
            set filename $item
            set  widgetInfo($address-message) "$widgetInfo($address-message) Loading dataset $dataid from $filename \n"
            set duration [glance_parsecontent $win $address $dataid $filename]
            
            set  widgetInfo($address-message) "$widgetInfo($address-message) $duration s. \n"
            set i 1
        }
        incr i
    }
    
    
    
    # update visualisation
    glance_reset_bounds $win $address
    glance_render $win $address
}

proc glance_render {win address} {
    set start_time [clock milliseconds]
    global widgetInfo
    glance_purge $win $address
       
   
    
    
    set xtitle [lindex $widgetInfo($address-projection) 0]
    set ytitle [lindex $widgetInfo($address-projection) 1]
    set ctitle $widgetInfo($address-color)

    
    set rscale [colorscale_define rainbow]
    
    
    
    foreach dataid $widgetInfo($address-dataids) {
        foreach part $widgetInfo($address-$dataid-root.children) {
            set range [llength $widgetInfo($address-$dataid-root.$part.$xtitle)]
            
            # prepare data to show
            set node_components ""
            set header ""
            foreach compo $widgetInfo($address-$dataid-root.$part.children) {
                set lcompo [llength $widgetInfo($address-$dataid-root.$part.$compo)]
                if {$lcompo == $range} {
                    lappend node_components $compo
                }
                if {$lcompo == 1} {
                   lappend header "$compo : $widgetInfo($address-$dataid-root.$part.$compo)"
                }
            }
        
        
        
        
        
            if {$widgetInfo($address-render) == "cells" && $widgetInfo($address-$dataid-list_conn) != ""} {
                # show by cells
                set i 0
                set color 0
                set show 1
                
                
                foreach node $widgetInfo($address-$dataid-root.$part.conn) {
                    
                    incr i
                    # clip
                    set rclip [lindex $widgetInfo($address-$dataid-root.$part.$widgetInfo($address-clip)) $node-1]
                    if {$rclip >  $widgetInfo($address-clipmax)} {set show 0}
                    if {$rclip <  $widgetInfo($address-clipmin)} {set show 0}
                    
                    # information
                    if {$show == 1} {
                        set face(px-$i) [glance_pix_x $address [lindex $widgetInfo($address-$dataid-root.$part.$xtitle) $node-1]]
                        set face(py-$i) [glance_pix_y $address [lindex $widgetInfo($address-$dataid-root.$part.$ytitle) $node-1]]
                        if {[info exists widgetInfo($address-$dataid-root.$part.$ctitle)]} {
                            set col [lindex $widgetInfo($address-$dataid-root.$part.$ctitle)  $node-1]
                            set col [expr {($col-$widgetInfo($address-colormin))/($widgetInfo($address-colormax)-$widgetInfo($address-colormin))}]
                            set color [colorscale_get $rscale $col]
                        } else {
                            set color black
                        }
                        set outline_color $color
                    }
                    
                    
                    # show triangles
                    if {$i == 3} {
                        if {$show == 1} {
                            # data to show
                            set details $header
                            foreach compo $node_components {
                                lappend details "$compo : [lindex $widgetInfo($address-$dataid-root.$part.$compo) $node-1]"
                            }
                          
                            #if {"$dataid.$part" in $widgetInfo($address-selection)} {
                            #     set outline_color grey50
                            #}
                            #if {"$dataid.$part" in $widgetInfo($address-activeselection)} {
                            #     set outline_color red
                            #}
                            $win.t.can create polygon $face(px-1) $face(py-1) $face(px-2) $face(py-2) $face(px-3) $face(py-3) -fill $color -outline $outline_color -activewidth 3 -activeoutline black -tags "$dataid.$part $dataid.$part.$node"
                            $win.t.can bind "$dataid.$part.$node" <Motion> [subst {
                                set widgetInfo($address-message) "$dataid.$part.$node \n[join $details "\n"] "
                            }]
                            
                        }
                        set i 0
                        set color 0
                        set show 1
                    }
                    
                }
            } else {
                # show by nodes
                for {set i 0} {$i < $range} {incr i} {
                    
                    # clip
                    set rclip [lindex $widgetInfo($address-$dataid-root.$part.$widgetInfo($address-clip)) $i]
                    if {$rclip >  $widgetInfo($address-clipmax)} {continue}
                    if {$rclip <  $widgetInfo($address-clipmin)} {continue}
                    
                    #position
                    set rx [lindex $widgetInfo($address-$dataid-root.$part.$xtitle) $i]
                    set ry [lindex $widgetInfo($address-$dataid-root.$part.$ytitle) $i]
                    set px [glance_pix_x $address $rx]
                    set py [glance_pix_y $address $ry]
                    
                    #colors
                    if {[info exists widgetInfo($address-$dataid-root.$part.$ctitle)]} {
                        set col [lindex $widgetInfo($address-$dataid-root.$part.$ctitle) $i]
                        set col [expr {($col-$widgetInfo($address-colormin))/($widgetInfo($address-colormax)-$widgetInfo($address-colormin))}]
                        set color [colorscale_get $rscale $col]
                    } else {
                        set color black
                    }
                    set outline_color $color
                    
                    # data to show
                    set details $header
                    foreach compo $node_components {
                        lappend details "$compo : [lindex $widgetInfo($address-$dataid-root.$part.$compo) $i]"
                    }
                   
                   
                   
                    # show selection                   
                    #if {"$dataid.$part" in $widgetInfo($address-selection)} {
                    #     set outline_color grey50
                    #}
                    #if {"$dataid.$part" in $widgetInfo($address-activeselection)} {
                    #     set outline_color red
                    #}
                    
                    
                    if {$widgetInfo($address-showdiam)} {
                        set diam [expr {[lindex $widgetInfo($address-$dataid-root.$part.epargne) $i] + 0.5*[lindex $widgetInfo($address-$dataid-root.$part.diam) $i]}]
                        set h [expr {[glance_pix_x $address $diam]-[glance_pix_x $address 0]}]
                    } else {
                        set h 1
                    }
                    
                    # render
                    $win.t.can create oval [expr {$px -$h}] [expr {$py -$h}]  [expr {$px +$h}] [expr {$py +$h}] -outline $outline_color -activeoutline black -fill $color -activewidth 4 -tags "$dataid.$part $dataid.$part.$i"
                    # binding local
                    $win.t.can bind "$dataid.$part.$i" <Motion> [subst {
                        set widgetInfo($address-message) "$dataid.$part.$i \n[join $details "\n"] "
                    }]
                }  
            }
            #binding global
            $win.t.can bind "$dataid.$part" <ButtonPress> [subst {glance_select $win $address  "$dataid.$part"}]
        }
    }

   
   

   
    # draw frame an midlines
    set xmin_pix [glance_pix_x $address $widgetInfo($address-xmin)]
    set xmax_pix [glance_pix_x $address $widgetInfo($address-xmax)]
    set ymin_pix [glance_pix_y $address $widgetInfo($address-ymin)]
    set ymax_pix [glance_pix_y $address $widgetInfo($address-ymax)]
    set xzero_pix [glance_pix_x $address 0.0]
    set yzero_pix [glance_pix_y $address 0.0]
             
    
    # draw zero axis if any
    if {[expr { $widgetInfo($address-xmin)* $widgetInfo($address-xmax)}] < 0} {
         $win.t.can create line $xzero_pix $ymin_pix $xzero_pix $ymax_pix -width 2 -fill black -tags "axis"
    }
    if {[expr { $widgetInfo($address-ymin)* $widgetInfo($address-ymax)}] < 0} {
         $win.t.can create line $xmin_pix $yzero_pix $xmax_pix $yzero_pix -width 2 -fill black -tags "axis"
    }
    
    
    foreach frac {0 0.33 0.66 1 } {
        set xfrac  [expr {$frac*$widgetInfo($address-xmin) + (1.-$frac)*$widgetInfo($address-xmax)}]
        set yfrac  [expr {$frac*$widgetInfo($address-ymin) + (1.-$frac)*$widgetInfo($address-ymax)}]
        set xfrac_pix [glance_pix_x $address $xfrac]
        set yfrac_pix [glance_pix_y $address $yfrac]
        $win.t.can create line $xfrac_pix  $ymin_pix $xfrac_pix $ymax_pix -fill grey80 -tags "axis"
        canvas_text_vector $win.t.can $xfrac_pix $ymin_pix [format %.4g $xfrac] nw 11 45 black  "axis"    
        $win.t.can create line $xmin_pix  $yfrac_pix $xmax_pix $yfrac_pix -fill grey80 -tags "axis"
        canvas_text_vector $win.t.can $xmin_pix $yfrac_pix [format %.4g $yfrac] sw 11 45 black  "axis"
    }
    
    
    set xtit [expr {0.5*$widgetInfo($address-xmin)+ 0.5*$widgetInfo($address-xmax)}]
    set ytit [expr {0.5*$widgetInfo($address-ymin)+ 0.5*$widgetInfo($address-ymax)}]
    set xtit_pix [glance_pix_x $address $xtit]
    set ytit_pix [glance_pix_y $address $ytit]
    canvas_text_vector $win.t.can $xtit_pix $ymin_pix "$xtitle" nw 11 45 black  "axis" 
    canvas_text_vector $win.t.can $xmin_pix $ytit_pix "$ytitle" sw 11 45  black "axis" 
    
        
    
    $win.t.can lower "axis"
   
    $win.t.can configure -scrollregion [ $win.t.can bbox all]
    
    smartpacker_update_visibility $win $address
    set stop_time [clock milliseconds]
    set widgetInfo($address-rendermessage) "Render time:\n[format "%0.3f" [expr { ($stop_time - $start_time)*0.001 }]] s."
    
}





proc glance_reset_bounds {win address} {
    global widgetInfo
    
    set xtitle [lindex $widgetInfo($address-projection) 0]
    set ytitle [lindex $widgetInfo($address-projection) 1]
    
    
    # get bounds
    set boundx [glance_getbounds $win $address $xtitle]
    set xmin [lindex $boundx 0]
    set xmax [lindex $boundx 1]
    
    set boundy [glance_getbounds $win $address $ytitle]
    set ymin [lindex $boundy 0]
    set ymax [lindex $boundy 1]
    
   
    # avoid null ranges
    if {$ymin == $ymax} {
        return "Cannot show data \n dimension $ytitle goes from $ymin to $ymax. "
    }
    if {$xmin == $xmax} {
        return "Cannot show data \n dimension $xtitle goes from $xmin to $xmax. "
    }
    
   
    
    
    
    set xrange [expr {$xmax-$xmin} ]
    set yrange [expr {$ymax-$ymin} ]
    
    
    # initialize bounds and scaling
    set widgetInfo($address-xmin) $xmin
    set widgetInfo($address-xmax) $xmax
    set widgetInfo($address-ymin) $ymin
    set widgetInfo($address-ymax) $ymax 
    
    
    
    if {"theta" ni $widgetInfo($address-projection) } {
        if { $xrange > $yrange} {
            set maxrange $xrange
        } else {
            set maxrange $yrange
        }
        set widgetInfo($address-xscale) [expr {0.8*$widgetInfo($address-widthcanvas)/$maxrange}]
        set widgetInfo($address-yscale) [expr {0.8*$widgetInfo($address-heightcanvas)/$maxrange}]
    } else {
        set widgetInfo($address-xscale) [expr {0.8*$widgetInfo($address-widthcanvas)/$xrange}]
        set widgetInfo($address-yscale) [expr {0.8*$widgetInfo($address-heightcanvas)/$yrange}]
    }
    return 1
    
}

proc glance_select {win address tag} {
    global widgetInfo
    if  {$tag in $widgetInfo($address-selection)} {
        set widgetInfo($address-activeselection) ""
        lremove widgetInfo($address-selection)  $tag
        set widgetInfo($address-message) "Removing  $tag from selection : \n -[join $widgetInfo($address-selection)  "\n -"] "
    } else {
        set widgetInfo($address-activeselection) "$tag"
        lappend widgetInfo($address-selection) $tag
        set widgetInfo($address-selection) [ lsort $widgetInfo($address-selection)]
        set widgetInfo($address-message) "Adding  $tag to selection : \n -[join $widgetInfo($address-selection)  "\n -"]"
    }
    glance_render $win $address
    
}


proc glance_getbounds {win address  component} {
    global widgetInfo
    set min +1e40
    set max -1e40
    
    foreach dataid  $widgetInfo($address-dataids) {
        foreach part $widgetInfo($address-$dataid-root.children) {
            if {[info exists widgetInfo($address-$dataid-root.$part.$component)]} {    
                if {[string is double [lindex  $widgetInfo($address-$dataid-root.$part.$component)  0]] == 0} {return "nobounds"}
                set res [list_bounds  $widgetInfo($address-$dataid-root.$part.$component) $min $max]
                set min [lindex $res 0]
                set max [lindex $res 1]
            } else {
                log "$dataid-root.$part.$component not found"
            }
        }
    }
    return "$min $max"
}

proc glance_parsecontent {win address dataid filename} {
    set start_time [clock milliseconds]
    
    global widgetInfo
    
    lappend widgetInfo($address-dataids) $dataid
    
    
    set widgetInfo($address-$dataid-filename) $filename
    set chout [ open $filename r ]
    set out [ split [ read $chout] "\n" ]
    close $chout

    set readin 0
    set path "none"
    
    set data ""
    foreach line $out {
        if {[string index $line end] == ":"} {
            set path [string range $line 0 end-2]
        } else {
            set data "$data $line"
        }
        if {$line == ""} {
            set data [string map {" " ""} $data ]
            set data [string trimright $data ","]
            set data [split $data ","]
            set widgetInfo($address-$dataid-$path) $data
            set data ""
            set path "none"
        }
    }
    
    
    set list_components ""
    set list_conn ""
    
    set list_proj ""
    foreach part $widgetInfo($address-$dataid-root.children) {
        foreach compo $widgetInfo($address-$dataid-root.$part.children) {
            if {$compo ni "conn id"} {
                lappend list_components $compo
            }
            if {$compo in "conn"} {
                lappend list_conn $compo
            }
            
        }
    }
    set list_components [lsort -unique -dictionary $list_components ]
    
    foreach proj {"x y" "x z" "z y" "theta x"  "x r"  "theta r" "y curvi" "z curvi" "theta curvi" } {
        set ok 1
        foreach dir $proj {
            if {$dir ni $list_components} {set ok 0}
        }
        
        if {$ok } {lappend list_proj $proj}
    }
    
    
    if {"epargne" in $list_components} {
        set widgetInfo($address-showdiam) 1
    } else {
        set widgetInfo($address-showdiam) 0
        
    }
    
    
    set widgetInfo($address-$dataid-listcomponents) $list_components
    set widgetInfo($address-$dataid-list_proj) $list_proj
    set widgetInfo($address-$dataid-list_conn) $list_conn
    
    glance_updateproj $win $address
    glance_updatecomponents $win $address $dataid
    
    
    
    glance_resets $win $address "clip"
    glance_resets $win $address  "color"
    
    set stop_time [clock milliseconds]
    return "Loading time :  [format "%0.3f" [expr { ($stop_time - $start_time)*0.001 }]] s."
}

proc glance_updatecomponents {win address dataid} {
    global widgetInfo

    set list_components $widgetInfo($address-$dataid-listcomponents)
    
    $win.c.color configure -values $list_components
    if {$widgetInfo($address-color) ni $list_components} {
        set widgetInfo($address-color) [lindex $list_components 0]
    }
    
    $win.c.clip configure -values $list_components
    if {$widgetInfo($address-clip) ni $list_components} {
        set widgetInfo($address-clip) [lindex $list_components 0]
    }
      
}

proc glance_updateproj {win address} {
    global widgetInfo
        
    set list_proj ""
    foreach dataid  $widgetInfo($address-dataids) {
        if {$list_proj == ""} {set list_proj $widgetInfo($address-$dataid-list_proj)}
        
        if {$list_proj != $widgetInfo($address-$dataid-list_proj)} {
            set new_list ""
            foreach proj $list_proj {
                if {$proj in $widgetInfo($address-$dataid-list_proj)} {
                    lappend new_list $proj
                }
            }
            set list_proj $new_list
        }
        
        
    }
    
    $win.c.projection configure -values $list_proj
    if {$widgetInfo($address-projection) ni $list_proj} {
        set widgetInfo($address-projection) [lindex $list_proj 0]
    }
}


proc glance_resets { win address what} {
    global widgetInfo
    
    switch $what {
        "clip" {
            set res [glance_getbounds $win $address  $widgetInfo($address-clip)]
            if {$res == "nobounds"} {
                warning " Cannot clip using  $widgetInfo($address-clip)"
                set widgetInfo($address-clip) "x"
                return
                #glance_resets $win $address what
            }
            set widgetInfo($address-clipmin) [lindex $res 0 ]
            set widgetInfo($address-clipmax) [lindex $res 1 ]
                    
        }
        "color" {
            set res [glance_getbounds $win $address  $widgetInfo($address-color)]
            if {$res == "nobounds"} {
                warning " Cannot color using  $widgetInfo($address-color)"
                set widgetInfo($address-color) "x"
                return
                #glance_resets $win $address what
            }
            set widgetInfo($address-colormin) [lindex $res 0 ]
            set widgetInfo($address-colormax) [lindex $res 1 ]
            if {$widgetInfo($address-colormin) == $widgetInfo($address-colormax)} {
                set widgetInfo($address-colormax) [expr {$widgetInfo($address-colormin) + 1.}]
            }
                    
        }
    }
    return 1
}

proc glance_pix_x { address x } {
    global widgetInfo
    set xpix [expr {($x -$widgetInfo($address-xmin)) * $widgetInfo($address-xscale)}]
    return $xpix
}
proc glance_pix_y { address y} {
    global widgetInfo
    set ypix [expr {($widgetInfo($address-ymax) -$y) * $widgetInfo($address-yscale)}]
    return $ypix
    
}
proc glance_pix_realx { address xpix} {
    global widgetInfo
    set x [expr {$widgetInfo($address-xmin) + $xpix/$widgetInfo($address-xscale)}]
    return $x
}
proc glance_pix_realy { address ypix} {
    global widgetInfo
    set y [expr {$widgetInfo($address-ymax) - $ypix/$widgetInfo($address-yscale)}]
    return $y
}

proc glance_showlocation {win address} {
    global widgetInfo
    $win.t.can delete "pointer"
    set x [$win.t.can canvasx [expr {[winfo pointerx $win.t.can] - [winfo rootx $win.t.can]}]]
    set y [$win.t.can canvasy [expr {[winfo pointery $win.t.can] - [winfo rooty $win.t.can]}]]
    set realx [glance_pix_realx  $address $x  ]
    set realy [glance_pix_realy  $address $y  ]
    set widgetInfo($address-position)  "[format %+.4f $realx]\n[format %+.4f $realy]"
    canvas_text_highlighted $win.t.can $x $y  $widgetInfo($address-position) "pointer"
}



proc glance_scrollupdate { win address xzoom yzoom} {
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
proc glance_zoom { win address zoomfactor} {
    
    
    
    global widgetInfo
    
    set zoom [lindex [split $widgetInfo($address-zoom) " "] 1]
    log "zoom $zoom"
    if {$zoomfactor == 1 } {
        set zoomfactor [expr {100./$zoom}]
        set zoom 100
     } else {
        set zoom [expr {$zoom * $zoomfactor} ]
     }
     set widgetInfo($address-zoom) "Zoom: $zoom"
     set widgetInfo($address-xscale) [expr {$widgetInfo($address-xscale) * $zoomfactor} ]
     set widgetInfo($address-yscale) [expr {$widgetInfo($address-yscale) * $zoomfactor} ]
     
     glance_render $win $address
     $win.t.can configure -scrollregion [ $win.t.can bbox all ]
     
   
}

# to move the canvases using Control_move binding
proc glance_move { win address x y} {
    global widgetInfo
    
    set xv [expr $x * $widgetInfo($address-scroll_factor_x)]
    set yv [expr $y * $widgetInfo($address-scroll_factor_y)]
    $win.t.can xview moveto $xv
    $win.t.can yview moveto $yv
}

proc glance_purge {win address } {
    global widgetInfo
    destroy $win.t.can
    canvas $win.t.can  -yscrollcommand [list $win.t.sby set] -xscrollcommand [list $win.t.sbx set] -width $widgetInfo($address-widthcanvas) -height $widgetInfo($address-heightcanvas)
    grid $win.t.can -row 1 -column 1 -sticky news
    update
}

proc glance_check {win address} {
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