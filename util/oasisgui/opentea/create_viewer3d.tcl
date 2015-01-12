#  This program is under CECILL_B licence. See footer for details.




# VIEW3D CREATION

proc viewer3d_create { args } {
    set mandatory_arguments { path_father address }
    
    global w3dData w3dTvCompo
    # Initializes the widget
    initWidget  
        
    set w3dData(rendertime) "0.314 s"
    set w3dData(fps) "-"
    set w3dData(cylindrical)  0
    set w3dData(autoDraw)  0
    set w3dData(FocusBox) 0
   
    
    set w3d .view3d
    array set w3dData {}
    set w3dData(rendertime) "-"
    set w3dData(fps) "-"
    set w3dData(cylindrical)  0
    set w3dData(bfcull)  1
    set w3dData(autoDraw)  0
    set w3dData(FocusBox) 0
    set w3dData(light.x) 0.707
    set w3dData(light.y) 0.707
    set w3dData(light.z) 0
    
    
    set w3dData(focus.x) 0
    set w3dData(focus.y) 0
    set w3dData(focus.z) 0
    
    set w3dData(bndsample) 5
    set w3dData(h) 2e-3
   
    set w3dData(view) [list 1 0 0]
    set w3dData(right) [list 0 0 1]
    set w3dData(top) [list 0 1 0]
    
    set w3dData(vecx) [list 1 0 0]
    set w3dData(vecy) [list 0 1 0]
    set w3dData(vecz) [list 0 0 1]
    set w3dData(view) "zy"
    
    set w3dData(quaternion) [axisangle_to_q  [list 1 0 0] 0]
    
    
    
    set w3dData(bg) black
    set w3dData(focuscolor) cyan
    
    set w3dData(listPart) []
    
    
    if {[winfo exists $w3d]} {
        wm deiconify $w3d
        raise $w3d
    } else {
        toplevel $w3d
        
        set unitw [expr { int ([expr { 0.22*$widgetInfo(guiheight)}])}]
        set unith $unitw
        set w3dData(width) $unitw
        
        
        # main structure
        ttk::labelframe $w3d.ct -text "Controls" -width $unitw -height $unith
        
        grid $w3d.ct -column 0 -row 0 -sticky news
        
        ttk::labelframe $w3d.tf -text "Treeview"  -width [expr {3.*$unitw}] -height $unith
        grid $w3d.tf -column 0 -row 1 -sticky news
        
        
        canvas $w3d.gr1 -width [expr {1.4*$unitw}] -height $unith -bg $w3dData(bg)  -highlightbackground [ThemeColor 0.9]
        grid $w3d.gr1 -column 1 -row 0 -sticky news
        
        
        
        canvas $w3d.gr2 -width [expr {1.4*$unitw}] -height $unith -bg $w3dData(bg)   -highlightbackground [ThemeColor 0.9]
        grid $w3d.gr2 -column 2 -row 0 -sticky news
        
        canvas $w3d.gr3 -width [expr {1.4*$unitw}] -height $unith -bg $w3dData(bg)  -highlightbackground [ThemeColor 0.9]
        grid $w3d.gr3 -column 3 -row 0 -sticky news
        
        canvas $w3d.gr0 -width [expr {4.2*$unitw}] -height [expr {3.*$unith}] -bg $w3dData(bg)  -highlightbackground [ThemeColor 0.9]
        grid $w3d.gr0 -column 1 -row 1 -columnspan 3 -sticky news
         
        
        # Controls
        ttk::label $w3d.ct.lbl_rendertime -textvariable w3dData(rendertime) -wraplength $unitw
        grid $w3d.ct.lbl_rendertime -column 0 -row 0
        
        ttk::label $w3d.ct.lbl_fps -textvariable w3dData(fps) -wraplength $unitw
        grid $w3d.ct.lbl_fps -column 1 -row 0
        
        ttk::checkbutton $w3d.ct.cb_cylindrical -text "x-Cylindrical" -variable w3dData(cylindrical) -command "viewer3d_tryToRedraw forced"
        grid $w3d.ct.cb_cylindrical -column 0 -row 1 -sticky w
        
        ttk::checkbutton $w3d.ct.cb_bfcull -text "Backface cull." -variable w3dData(bfcull) -command "viewer3d_tryToRedraw forced"
        grid $w3d.ct.cb_bfcull -column 1 -row 1 -sticky w
        
        
        ttk::checkbutton $w3d.ct.cb_autoDraw -text "auto Draw" -variable w3dData(autoDraw) -command  viewer3d_update_reDrawButton 
        grid $w3d.ct.cb_autoDraw -column 0 -row 2 -sticky w 
        
        ttk::button $w3d.ct.bt_reDraw -text "reDraw" -command  "viewer3d_tryToRedraw forced"
        grid $w3d.ct.bt_reDraw -column 1 -row 2
        
        ttk::checkbutton $w3d.ct.cb_FocusBox -text "Focus Box" -variable w3dData(FocusBox) 
        grid $w3d.ct.cb_FocusBox -column 0 -row 3 -sticky w
        
        
        # Treeview
        ttk::treeview $w3d.tf.tv -height 30 -yscrollcommand [list $w3d.tf.sb  set]
        set tv3d $w3d.tf.tv
        grid $tv3d  -sticky news -column 0 -row 0
        
        ttk::scrollbar $w3d.tf.sb -orient vertical -command [list $tv3d yview]
        grid $w3d.tf.sb  -sticky news -column 1 -row 0
        
        
        array set w3dTvCompo {}
        
        set w3dTvCompo(columns) []
        
        # render
        
        
        set item  "aspect"
        lappend w3dTvCompo(columns) $item
        set w3dTvCompo($item.options) []
        
            set opt "facets/edges"
            lappend w3dTvCompo($item.options) $opt
           
            set opt "facets"
            lappend w3dTvCompo($item.options) $opt
           
             set opt "normals/edges"
            lappend w3dTvCompo($item.options) $opt
           
            set opt "normals"
            lappend w3dTvCompo($item.options) $opt
           
            set opt "edges"
            lappend w3dTvCompo($item.options) $opt
            
            set opt "bounds"
            lappend w3dTvCompo($item.options) $opt
            
            set opt "hidden"
            lappend w3dTvCompo($item.options) $opt
            
        
        set item  "motion"
        lappend w3dTvCompo(columns) $item
        set w3dTvCompo($item.options) []
            set opt "edges"
            lappend w3dTvCompo($item.options) $opt
            
            set opt "facet/edges"
            lappend w3dTvCompo($item.options) $opt
           
            
            
            set opt "bounds"
            lappend w3dTvCompo($item.options) $opt
            
            set opt "hidden"
            lappend w3dTvCompo($item.options) $opt
            
        set item  "color"
        lappend w3dTvCompo(columns) $item
        set w3dTvCompo($item.options) []
            foreach opt [list "grey50" "blue" "red" "green" "purple4" "orange2" "turquoise2"] {
                lappend w3dTvCompo($item.options) $opt
            }
            
        set item  "selection"
        lappend w3dTvCompo(columns) $item
        set w3dTvCompo($item.options) []
        
            set opt "yes"
            lappend w3dTvCompo($item.options) $opt
            
            set opt "no"
            lappend w3dTvCompo($item.options) $opt
            
            set opt "excluded"
            lappend w3dTvCompo($item.options) $opt
            
        
        
        
        
        
        $tv3d configure -columns $w3dTvCompo(columns) 
        
        set id 0
        $tv3d column #0 -width [expr {int(0.7*$unith)}]
        foreach col $w3dTvCompo(columns) {
            incr id
            $tv3d heading $col -text $col 
            $tv3d column #$id -width [expr {int(0.3*$unith)}]
        
        }
        
        bind $tv3d <ButtonPress-2> [subst { viewer3d_tv_context_menu %x %y %X %Y }]
        
    }
    
    
    
    
   
    finishWidget
    
    
    bind $w3d <<sourceRefresh>> [subst {viewer3d_update_source $win $address \%d} ]
    
    bind . <<ThemeUpdate>> +[subst {
        
        .view3d.gr0 configure -bg \$w3dData(bg)   -highlightbackground \[ThemeColor 0.9\]
        .view3d.gr1 configure -bg \$w3dData(bg)   -highlightbackground \[ThemeColor 0.9\]
        .view3d.gr2 configure -bg \$w3dData(bg)  -highlightbackground \[ThemeColor 0.9\]
        .view3d.gr3 configure -bg \$w3dData(bg)   -highlightbackground \[ThemeColor 0.9\]
        
        
    } ]
    
    bind .view3d.gr0 <ButtonPress-1> [subst {viewer3d_main_canvas_but1 %x %y %X %Y}]
    bind .view3d.gr0 <B1-Motion> [subst {viewer3d_main_canvas_mot1 %x %y %X %Y}]
    bind .view3d.gr0 <ButtonRelease-1> [subst {viewer3d_main_canvas_rel1 %x %y %X %Y}]
    
    
    bind .view3d.gr1 <Double-1> {
        set w3dData(view) [list 0 -1 0]
        set w3dData(right) [list 1 0 0]
        set w3dData(top) [list 0 0 -1]
    
        set w3dData(vecx) [list 1 0 0]
        set w3dData(vecy) [list 0 1 0]
        set w3dData(vecz) [list 0 0 1]
        set w3dData(view) "xz"
        viewer3d_rotate_view_coords aspect
        viewer3d_redraw_main aspect
    }
    
    
    bind .view3d.gr2 <Double-1> {
        set w3dData(view) [list 0 0 -1]
        set w3dData(right) [list 1 0 0]
        set w3dData(top) [list 0 1 0]
    
        set w3dData(vecx) [list 1 0 0]
        set w3dData(vecy) [list 0 1 0]
        set w3dData(vecz) [list 0 0 1]
        set w3dData(view) "xy"
        viewer3d_rotate_view_coords aspect
        viewer3d_redraw_main aspect
    }
    
    
     bind .view3d.gr3 <Double-1> {
        set w3dData(view) [list 1 0 0]
        set w3dData(right) [list 0 0 1]
        set w3dData(top) [list 0 1 0]
    
    
        set w3dData(vecx) [list 1 0 0]
        set w3dData(vecy) [list 0 1 0]
        set w3dData(vecz) [list 0 0 1]
        set w3dData(view) "zy"
        viewer3d_rotate_view_coords aspect
        viewer3d_redraw_main aspect
    }
    
    # clean the widget callBack on dstruction
    #bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
}




# to add a source on the viewer 
proc viewer3d_update_reDrawButton {} {
    global w3dTvCompo w3dData
    .view3d.ct.bt_reDraw configure -state disabled    
}



proc viewer3d_tryToRedraw { {mode "normal"} } {
    global w3dTvCompo w3dData
    if {$mode == "forced" || $w3dData(autoDraw) == 1} {
        viewer3d_tv_ReadContent
        
        viewer3d_sort_objects
        viewer3d_redraw_side aspect
        
        
        viewer3d_rotate_view_coords aspect
        viewer3d_redraw_main aspect
        .view3d.ct.bt_reDraw configure -state disabled    
    } else {
        viewer3d_tv_ReadContent
        viewer3d_sort_objects
        .view3d.ct.bt_reDraw configure -state normal   
    }
}


proc viewer3d_tv_setvalue { item col value} {
    set tv3d .view3d.tf.tv
    
    set initValue [$tv3d item $item -values]
    debug "here $initValue"
    set finalValue [lreplace $initValue  $col $col $value]
    $tv3d item $item -values $finalValue

}






proc viewer3d_tv_ReadContent {} {
    global w3dTvCompo w3dData
    set tv3d .view3d.tf.tv
    
    # rebuild list parts (debug)
    #set listItem []
    #foreach item [$tv3d children {} ] {
    #    if {[$tv3d children $item ] == [] } {
    #        lappend listItem $item
    #    } else {
    #        foreach subitem [$tv3d children $item ] {
    #            lappend listItem $subitem            
    #        }
    #    }
    #}
    #
    #set listItem [lsort $listItem]
    #debug "FromTV  $listItem"
    
    
    foreach item $w3dData(listPart) {
        
        set index -1
        foreach value [$tv3d item $item -value] {
            incr index
            set column [lindex $w3dTvCompo(columns) $index]
            set w3dData($item.$column) $value
            debug "$item.$column $value"
        }
        
    }
    
    return
}



proc viewer3d_main_canvas_but1 {x y globalX globalY} {
    global w3dData
    set w3dData(init.view) $w3dData(view)
    set w3dData(init.right) $w3dData(right)
    set w3dData(init.top) $w3dData(top)
    set w3dData(bind.motion.x) $x
    set w3dData(bind.motion.y) $y
    
}


proc viewer3d_main_canvas_mot1 {x y globalX globalY} {
    global w3dData
    
    set start_time [clock milliseconds]
    set dx [expr {$x-$w3dData(bind.motion.x)}]
    set dy [expr {$y-$w3dData(bind.motion.y)}]
    
    set angle [expr { min(1.,sqrt($dx*$dx+$dy*$dy)/300) }]
    
    
    set vec_x  [expr {$dx*[lindex $w3dData(top) 0]+$dy*[lindex $w3dData(right) 0] }   ]
    set vec_y  [expr {$dx*[lindex $w3dData(top) 1]+$dy*[lindex $w3dData(right) 1] }   ]
    set vec_z  [expr {$dx*[lindex $w3dData(top) 2]+$dy*[lindex $w3dData(right) 2] }   ]
    
    
    
   
    
    
    set quaternion [axisangle_to_q [list $vec_x $vec_y $vec_z] $angle  ]
                    
    viewer3d_rotate_view_base $quaternion
    viewer3d_rotate_view_coords motion
    viewer3d_redraw_main motion
    
    set stop_time [clock milliseconds]
    set w3dData(fps)   "[expr {int(1./($stop_time - $start_time)/0.001) }]FPS"
    
    #puts "Quater $quaternion "
    #puts "Right $w3dData(right)"
    #puts "Top $w3dData(top)"
    #puts "angle $angle"
    #$w3dData(quaternion)"
    set w3dData(bind.motion.x) $x
    set w3dData(bind.motion.y) $y
    
    
}

proc viewer3d_main_canvas_rel1 {x y globalX globalY} {
    global w3dData
    set start_time [clock milliseconds]
    viewer3d_rotate_view_coords aspect
    viewer3d_redraw_main aspect
    set stop_time [clock milliseconds]
    set w3dData(rendertime)  "[format "%0.3f" [expr {($stop_time - $start_time)*0.001 }]]s"
}

proc viewer3d_tv_context_menu {x y globalX globalY} {
    global w3dTvCompo
    set tv3d .view3d.tf.tv
    set row [$tv3d identify row $x $y]
    set col [string map {"#" ""} [$tv3d identify column $x $y]]
    incr col -1
    set item [lindex $w3dTvCompo(columns) $col]
    
    if {[$tv3d parent $row ] ==  {} } {
        set anchor 1
    } else {
        set anchor 0
    }
    if {$row== ""} {
        return
    }
    $tv3d selection set $row
    update
    catch {destroy $tv3d.cmenu }
    set popMenu [menu  $tv3d.cmenu ]

    foreach opt $w3dTvCompo($item.options) {    
        $popMenu add command -label $opt -command [ subst {
            viewer3d_tv_setvalue $row $col $opt
            if {$anchor} {
                foreach child \[$tv3d children $row\] {
                    viewer3d_tv_setvalue \$child $col $opt         
                   
                }
            }
            viewer3d_tryToRedraw
           
        }]
    }
    tk_popup $popMenu $globalX $globalY
    
}


# to add a source on the viewer 
proc viewer3d_add_source {args} {
    set mandatory_arguments { path_father address }

    
    
    set w3d .view3d
    # Initializes the widget
    initWidget  
    
    set $widgetInfo($address-variable) "void"
        
    ttk::frame $win
    
    ttk::label $win.value -textvariable $widgetInfo($address-variable)
    
    append widgetInfo($address-refresh) [subst {
        event generate $w3d <<sourceRefresh>> -data $address
    }]
    
    
    finishWidget
    
    
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
     
    
    
}






proc viewer3d_update_source {win address data} {
    global widgetInfo
    global w3dData  w3dTvCompo
    
    set start_time [clock milliseconds]
    
    
    set dataId [string map {"." "_"} $data]
    set widgetInfo($address-dummy) $widgetInfo($data-variable)
    
    
    set fileSource [lindex [split $widgetInfo($data-variable) ";"] 1]
    
    if {$fileSource == ""} {
        return
    }
    
    set anchor [lindex [split  [file tail [file rootname $fileSource]] "_*"] 0]
    set listFiles [glob -nocomplain $fileSource]
    
    
    # UPDATE TREE
    
    set tv3d .view3d.tf.tv
    
    set defaultValues []
    foreach item $w3dTvCompo(columns) {
        lappend defaultValues [lindex $w3dTvCompo($item.options) 0]
    }
    
    if {[$tv3d exists $anchor]} {
        $tv3d delete $anchor
    }
    
    $tv3d insert {} end -id $anchor -text $anchor -values $defaultValues
    
    foreach fi $listFiles {
        set subpart  [file tail [file rootname $fi]] 
        $tv3d insert $anchor end -id $subpart -text $subpart -values $defaultValues
        $tv3d see $subpart
    }
    
    
    
    # UPDATE DATA ARRAY
    
    # cleaning
    array unset w3dData "$anchor*"
    
    foreach subpart $w3dData(listPart) {
        if {[string match "$anchor*" $subpart]} {
            lremove w3dData(listPart) $subpart
        }
    }
    
    #loading
    foreach fi $listFiles {
        set subpart  [file tail [file rootname $fi]] 
        
        set fp [open $fi r]
        set data [read $fp]
        close $fp
        
        lappend w3dData(listPart) $subpart
        set record "none"
        set content ""
        foreach line [split $data "\n"] {
            if {":" in $line} {
                set record [lindex [split $line " :"] 0]
                
                set record "$subpart.$record"
                
                set content  [lindex [split $line " :"] 1]
            } else {
                if {$record != "none"} {
                    if {$line != ""} {
                        set content "$content$line"
                    } else {
                        set w3dData($record) [split $content ","]
                        lremove  w3dData($record) ""
                    }
                }
            }
        }
        
    }
    
    set w3dData(listPart) [lsort $w3dData(listPart)]
    
    set stop_time [clock milliseconds]
    set msgtime "V3D Loading time :  [format "%0.3f" [expr { ($stop_time - $start_time)*0.001 }]] s."
    log $msgtime
    
    viewer3d_tryToRedraw "forced"
}   



proc viewer3d_sort_objects {} {
    global w3dData
    
    
   
    
    
    set w3dData(bbox.min.x) 1.e10
    set w3dData(bbox.min.y) 1.e10
    set w3dData(bbox.min.z) 1.e10
    set w3dData(bbox.rmin) 1.e10
    set w3dData(bbox.tmin) 1.e10
    set w3dData(bbox.max.x) -1.e10
    set w3dData(bbox.max.y) -1.e10
    set w3dData(bbox.max.z) -1.e10
    set w3dData(bbox.rmax) -1.e10
    set w3dData(bbox.tmax) -1.e10
    
    
    array set doneList {}
    array set globalList {}
    foreach cat  [list aspect motion] {
        set doneList($cat) []
        set globalList($cat) []
    }
    
    # create a list accessing to all visible objects with a new ordering 
    foreach part $w3dData(listPart) {
        if { $w3dData($part.aspect) != "hidden" || $w3dData($part.motion) != "hidden" } {
            
            set w3dData(bbox.min.x) [expr {min($w3dData(bbox.min.x),[lindex $w3dData($part.root.bounds.x) 0] )}]
            set w3dData(bbox.min.y) [expr {min($w3dData(bbox.min.y),[lindex $w3dData($part.root.bounds.y) 0] )}]
            set w3dData(bbox.min.z) [expr {min($w3dData(bbox.min.z),[lindex $w3dData($part.root.bounds.z) 0] )}]
            set w3dData(bbox.rmin) [expr {min($w3dData(bbox.rmin),[lindex $w3dData($part.root.bounds.r) 0] )}]
            set w3dData(bbox.tmin) [expr {min($w3dData(bbox.tmin),[lindex $w3dData($part.root.bounds.t) 0] )}]
            set w3dData(bbox.max.x) [expr {max($w3dData(bbox.max.x),[lindex $w3dData($part.root.bounds.x) 1] )}]
            set w3dData(bbox.max.y) [expr {max($w3dData(bbox.max.y),[lindex $w3dData($part.root.bounds.y) 1] )}]
            set w3dData(bbox.max.z) [expr {max($w3dData(bbox.max.z),[lindex $w3dData($part.root.bounds.z) 1] )}]
            set w3dData(bbox.rmax) [expr {max($w3dData(bbox.rmax),[lindex $w3dData($part.root.bounds.r) 1] )}]
            set w3dData(bbox.tmax) [expr {max($w3dData(bbox.tmax),[lindex $w3dData($part.root.bounds.t) 1] )}]
            
        }
        
        # bounds
        set deg2rad [expr {3.141593/180}]
        
        
        foreach cat  [list aspect motion] {
            if { $w3dData($part.$cat) in [list "bounds"] } {
                
                set xmin [lindex $w3dData($part.root.bounds.x) 0]
                set xmax [lindex $w3dData($part.root.bounds.x) 1]
                set ymin [lindex $w3dData($part.root.bounds.y) 0]
                set ymax [lindex $w3dData($part.root.bounds.y) 1]
                set zmin [lindex $w3dData($part.root.bounds.z) 0]
                set zmax [lindex $w3dData($part.root.bounds.z) 1]
                set rmin [lindex $w3dData($part.root.bounds.r) 0]
                set rmax [lindex $w3dData($part.root.bounds.r) 1]
                set tmin [lindex $w3dData($part.root.bounds.t) 0]
                set tmax [lindex $w3dData($part.root.bounds.t) 1]
                
                
                switch $w3dData(cylindrical) {
                    "0" {
                        for {set i 0} {$i < $w3dData(bndsample)} {incr i } {
                            set j [expr {$i+1}]
                            
                            #set color [shadeColor $w3dData($part.color) [expr {  1.0*$i/$w3dData(bndsample)}]]
                            set color $w3dData($part.color) 
                            # x direction
                            set x1 [expr { $xmin + $i*($xmax-$xmin)/$w3dData(bndsample) }]
                            set x2 [expr { $xmin + $j*($xmax-$xmin)/$w3dData(bndsample) }]
                            
                            set y $ymin
                            set z $zmin
                            lappend globalList($cat) [list "b" $part $i $x1 $y $z $color 1 0 0 $x2 $y $z]
                            
                            set y $ymin
                            set z $zmax
                            lappend globalList($cat) [list "b" $part $i $x1 $y $z $color 1 0 0 $x2 $y $z]
                            
                            set y $ymax
                            set z $zmin
                            lappend globalList($cat) [list "b" $part $i $x1 $y $z $color 1 0 0 $x2 $y $z]
                            
                            set y $ymax
                            set z $zmax
                            lappend globalList($cat) [list "b" $part $i $x1 $y $z $color 1 0 0 $x2 $y $z]
                            
                            # y direction
                            
                            set y1 [expr { $ymin + $i*($ymax-$ymin)/$w3dData(bndsample) }]
                            set y2 [expr { $ymin + $j*($ymax-$ymin)/$w3dData(bndsample) }]
                            
                            set x $xmin
                            set z $zmin
                            lappend globalList($cat) [list "b" $part $i $x $y1 $z $color 1 0 0 $x $y2 $z]
                            
                            set x $xmin
                            set z $zmax
                            lappend globalList($cat) [list "b" $part $i $x $y1 $z $color 1 0 0 $x $y2 $z]
                            
                            set x $xmax
                            set z $zmin
                            lappend globalList($cat) [list "b" $part $i $x $y1 $z $color 1 0 0 $x $y2 $z]
                            
                            set x $xmax
                            set z $zmax
                            lappend globalList($cat) [list "b" $part $i $x $y1 $z $color 1 0 0 $x $y2 $z]
                            
                            # r direction
                            
                            set z1 [expr { $zmin + $i*($zmax-$zmin)/$w3dData(bndsample) }]
                            set z2 [expr { $zmin + $j*($zmax-$zmin)/$w3dData(bndsample) }]
                            
                            set x $xmin
                            set y $ymin
                            lappend globalList($cat) [list "b" $part $i $x $y $z1 $color 1 0 0 $x $y $z2]
                            
                            set x $xmax
                            set y $ymin
                            lappend globalList($cat) [list "b" $part $i $x $y $z1 $color 1 0 0 $x $y $z2]
                            
                            set x $xmin
                            set y $ymax
                            lappend globalList($cat) [list "b" $part $i $x $y $z1 $color 1 0 0 $x $y $z2]
                            
                            set x $xmax
                            set y $ymax
                            lappend globalList($cat) [list "b" $part $i $x $y $z1 $color 1 0 0 $x $y $z2]
                            
                        }
                            
                    }
                    "1" {
                        set ysw [expr { $rmin * sin($tmin*$deg2rad)  }]
                        set zsw [expr { $rmin * cos($tmin*$deg2rad)  }]
                        
                        set yse [expr { $rmin * sin($tmax*$deg2rad)  }]
                        set zse [expr { $rmin * cos($tmax*$deg2rad)  }]
                        
                        set yne [expr { $rmax * sin($tmax*$deg2rad)  }]
                        set zne [expr { $rmax * cos($tmax*$deg2rad)  }]
                       
                        set ynw [expr { $rmax * sin($tmin*$deg2rad)  }]
                        set znw [expr { $rmax * cos($tmin*$deg2rad)  }]
                        
                        for {set i 0} {$i < $w3dData(bndsample)} {incr i } {
                            set j [expr {$i+1}]
                            
                            # x direction
                            set x1 [expr { $xmin + $i*($xmax-$xmin)/$w3dData(bndsample) }]
                            set x2 [expr { $xmin + $j*($xmax-$xmin)/$w3dData(bndsample) }]
                            
                            set y $yse
                            set z $zse
                            lappend globalList($cat) [list "b" $part $i $x1 $y $z $w3dData($part.color) 1 0 0 $x2 $y $z]
                            
                            set y $yne
                            set z $zne
                            lappend globalList($cat) [list "b" $part $i $x1 $y $z $w3dData($part.color) 1 0 0 $x2 $y $z]
                            
                            set y $ysw
                            set z $zsw
                            lappend globalList($cat) [list "b" $part $i $x1 $y $z $w3dData($part.color) 1 0 0 $x2 $y $z]
                            
                            set y $ynw
                            set z $znw
                            lappend globalList($cat) [list "b" $part $i $x1 $y $z $w3dData($part.color) 1 0 0 $x2 $y $z]
                            
                            # r direction
                            set y1 [expr { $yse + $i*($yne-$yse)/$w3dData(bndsample) }]
                            set z1 [expr { $zse + $i*($zne-$zse)/$w3dData(bndsample) }]
                            set y2 [expr { $yse + $j*($yne-$yse)/$w3dData(bndsample) }]
                            set z2 [expr { $zse + $j*($zne-$zse)/$w3dData(bndsample) }]
                            
                            lappend globalList($cat) [list "b" $part $i $xmin $y1 $z1 $w3dData($part.color) 1 0 0 $xmin $y2 $z2]
                            lappend globalList($cat) [list "b" $part $i $xmax $y1 $z1 $w3dData($part.color) 1 0 0 $xmax $y2 $z2]
                            
                            
                            set y1 [expr { $ysw + $i*($ynw-$ysw)/$w3dData(bndsample) }]
                            set z1 [expr { $zsw + $i*($znw-$zsw)/$w3dData(bndsample) }]
                            set y2 [expr { $ysw + $j*($ynw-$ysw)/$w3dData(bndsample) }]
                            set z2 [expr { $zsw + $j*($znw-$zsw)/$w3dData(bndsample) }]
                            
                            lappend globalList($cat) [list "b" $part $i $xmin $y1 $z1 $w3dData($part.color) 1 0 0 $xmin $y2 $z2]
                            lappend globalList($cat) [list "b" $part $i $xmax $y1 $z1 $w3dData($part.color) 1 0 0 $xmax $y2 $z2]
                            
                            # theta direction
                            
                            set t1 [expr { ($tmin + $i*($tmax-$tmin)/$w3dData(bndsample))*$deg2rad}]
                            set t2 [expr { ($tmin + $j*($tmax-$tmin)/$w3dData(bndsample))*$deg2rad}]
                            
                            set y1 [expr { $rmin * sin($t1) }]
                            set z1 [expr { $rmin * cos($t1) }]
                            set y2 [expr { $rmin * sin($t2) }]
                            set z2 [expr { $rmin * cos($t2) }]
                            lappend globalList($cat) [list "b" $part $i $xmin $y1 $z1 $w3dData($part.color) 1 0 0 $xmin $y2 $z2]
                            lappend globalList($cat) [list "b" $part $i $xmax $y1 $z1 $w3dData($part.color) 1 0 0 $xmax $y2 $z2]
                            
                            set y1 [expr { $rmax * sin($t1) }]
                            set z1 [expr { $rmax * cos($t1) }]
                            set y2 [expr { $rmax * sin($t2) }]
                            set z2 [expr { $rmax * cos($t2) }]
                            lappend globalList($cat) [list "b" $part $i $xmin $y1 $z1 $w3dData($part.color) 1 0 0 $xmin $y2 $z2]
                            lappend globalList($cat) [list "b" $part $i $xmax $y1 $z1 $w3dData($part.color) 1 0 0 $xmax $y2 $z2]
                        }    
                    }   
                }
            }
        }
        
        # edges
        foreach cat  [list aspect motion] {
            if { $w3dData($part.$cat) in [list "normals/edges" "facets/edges" "edges"] } {
                for {set ii 0} { $ii < [expr {int(0.5*[llength $w3dData($part.root.edges.conn)])}] } {incr ii} {
                    set i [lindex $w3dData($part.root.edges.conn) [expr {2*$ii}]] 
                    set i2 [lindex $w3dData($part.root.edges.conn) [expr {2*$ii+1}]] 
                    set x [lindex $w3dData($part.root.coords.x) $i]
                    set y [lindex $w3dData($part.root.coords.y) $i]
                    set z [lindex $w3dData($part.root.coords.z) $i]
                    set nx [lindex $w3dData($part.root.coords.nx) $i]
                    set ny [lindex $w3dData($part.root.coords.ny) $i]
                    set nz [lindex $w3dData($part.root.coords.nz) $i]
                   
                    
                     if {$w3dData(cylindrical)} {
                            
                            set r [lindex $w3dData($part.root.coords.r) $i]   
                            set t [lindex $w3dData($part.root.coords.t) $i]
                            set nr [expr {$ny*$y/$r+$nz*$z/$r}]
                            set nt [expr {-$nz*$y/$r+$ny*$z/$r}]
                            
                            set y $r
                            set z [expr {$t*$deg2rad*$r}]
                            set ny $nr
                            set nz $nt
                            
                        }  
                    set x2 [lindex $w3dData($part.root.coords.x) $i2]
                    set y2 [lindex $w3dData($part.root.coords.y) $i2]
                    set z2 [lindex $w3dData($part.root.coords.z) $i2]
                    
                    
                    set pscal [expr {  ($nx*$w3dData(light.x) + $ny*$w3dData(light.y) + $nz*$w3dData(light.z) )}]
                    set color [shadeColor $w3dData($part.color) $pscal]
                    lappend globalList($cat) [list "e" $part $i $x $y $z $color $nx $ny $nz $x2 $y2 $z2]
                    lappend doneList($cat)  "$part.$i"
                }
            }
        }
        
        
        
        #facets
        foreach cat  [list aspect motion] {
            if {$w3dData($part.$cat) in  [list "facets/edges" "facets" ] } {
                for {set i 0} { $i < [llength $w3dData($part.root.coords.x)]} {incr i} {
                    # seulement si ces points n'ont pas deja ete affichés par des edges
                    if { "$part.$i"  ni $doneList($cat) } {
                        set x [lindex $w3dData($part.root.coords.x) $i]
                        set y [lindex $w3dData($part.root.coords.y) $i]
                        set z [lindex $w3dData($part.root.coords.z) $i]
                        set nx [lindex $w3dData($part.root.coords.nx) $i]
                        set ny [lindex $w3dData($part.root.coords.ny) $i]
                        set nz [lindex $w3dData($part.root.coords.nz) $i]
                        set n2x [lindex $w3dData($part.root.coords.n2x) $i]
                        set n2y [lindex $w3dData($part.root.coords.n2y) $i]
                        set n2z [lindex $w3dData($part.root.coords.n2z) $i]
                        set n3x [lindex $w3dData($part.root.coords.n3x) $i]
                        set n3y [lindex $w3dData($part.root.coords.n3y) $i]
                        set n3z [lindex $w3dData($part.root.coords.n3z) $i] 
                        if {$w3dData(cylindrical)} {
                            
                            set r [lindex $w3dData($part.root.coords.r) $i]   
                            set t [lindex $w3dData($part.root.coords.t) $i]
                            set nr [expr {$ny*$y/$r+$nz*$z/$r}]
                            set nt [expr {-$nz*$y/$r+$ny*$z/$r}]
                            
                            set y $r
                            set z [expr {$t*$deg2rad*$r}]
                            set ny $nr
                            set nz $nt
                            
                        }  
                  
                        
                        set h $w3dData(h)
                        set x2 [expr {$x+$n2x*$h}]
                        set y2 [expr {$y+$n2y*$h}]
                        set z2 [expr {$z+$n2z*$h}]
                        set x3 [expr {$x2+$n3x*$h}]
                        set y3 [expr {$y2+$n3y*$h}]
                        set z3 [expr {$z2+$n3z*$h}]
                        set x4 [expr {$x3-$n2x*$h}]
                        set y4 [expr {$y3-$n2y*$h}]
                        set z4 [expr {$z3-$n2z*$h}]
                        
                        set pscal [expr { ($nx*$w3dData(light.x) + $ny*$w3dData(light.y) + $nz*$w3dData(light.z) )}]
                        set color [shadeColor $w3dData($part.color) $pscal]
                    
                   
                        lappend globalList($cat) [list "f" $part $i $x $y $z $color $nx $ny $nz $x2 $y2 $z2 $x3 $y3 $z3 $x4 $y4 $z4 ]
                        lappend doneList($cat)  "$part.$i"
                    }
                }
            }
        }
         
        
        #normals
        foreach cat  [list aspect motion] {
            if {$w3dData($part.$cat) in  [list "normals/edges" "normals" ] } {
                for {set i 0} { $i < [llength $w3dData($part.root.coords.x)]} {incr i} {
                    # seulement si ces points n'ont pas deja ete affichés par des edges
                    if { "$part.$i"  ni $doneList($cat) } {
                        set x [lindex $w3dData($part.root.coords.x) $i]
                        set y [lindex $w3dData($part.root.coords.y) $i]
                        set z [lindex $w3dData($part.root.coords.z) $i]
                        set nx [lindex $w3dData($part.root.coords.nx) $i]
                        set ny [lindex $w3dData($part.root.coords.ny) $i]
                        set nz [lindex $w3dData($part.root.coords.nz) $i]
                        set h $w3dData(h)
                       
                        
                         if {$w3dData(cylindrical)} {
                            
                            set r [lindex $w3dData($part.root.coords.r) $i]   
                            set t [lindex $w3dData($part.root.coords.t) $i]
                            set nr [expr {$ny*$y/$r+$nz*$z/$r}]
                            set nt [expr {-$nz*$y/$r+$ny*$z/$r}]
                            
                            set y $r
                            set z [expr {$t*$deg2rad*$r}]
                            set ny $nr
                            set nz $nt
                            
                        }
                        
                        set x2 [expr {$x+$nx*$h}]
                        set y2 [expr {$y+$ny*$h}]
                        set z2 [expr {$z+$nz*$h}]
                        
                        set pscal [expr {  ($nx*$w3dData(light.x) + $ny*$w3dData(light.y) + $nz*$w3dData(light.z) )}]
                        set color [shadeColor $w3dData($part.color) $pscal]
                    
                    
                        lappend globalList($cat) [list "n" $part $i $x $y $z $color $nx $ny $nz $x2 $y2 $z2 ]
                        lappend doneList($cat)  "$part.$i"
                    }
                }
            }
        }    
    }
    
    
    
    
           
    
    set w3dData(bbox.spanx) [expr {$w3dData(bbox.max.x)-$w3dData(bbox.min.x)}]
    set w3dData(bbox.maxspan)  $w3dData(bbox.spanx)
    set w3dData(bbox.spany) [expr {$w3dData(bbox.max.y)-$w3dData(bbox.min.y)}]
    set w3dData(bbox.maxspan)  [expr {max($w3dData(bbox.maxspan), $w3dData(bbox.spany))}]
    set w3dData(bbox.spanz) [expr {$w3dData(bbox.max.z)-$w3dData(bbox.min.z)}]
    if {$w3dData(cylindrical) == 0} {
        set w3dData(bbox.maxspan)  [expr {max($w3dData(bbox.maxspan), $w3dData(bbox.spanz))}]
    }
    set w3dData(focus.point.x) [expr {0.5*($w3dData(bbox.max.x)+$w3dData(bbox.min.x))}]
    set w3dData(focus.point.y) [expr {0.5*($w3dData(bbox.max.y)+$w3dData(bbox.min.y))}]
    set w3dData(focus.point.z) [expr {0.5*($w3dData(bbox.max.z)+$w3dData(bbox.min.z))}]
  
    
    set w3dData(pix_per_m)  [expr { 1.0*$w3dData(width) / $w3dData(bbox.maxspan)  } ]    
    
    if {$w3dData(cylindrical) == 1} {
        set w3dData(pix_per_deg)  [expr { 1.0*$w3dData(width) / $w3dData(bbox.spanz)  } ]  
    }
    
    
    
    # axis
    set h [expr {0.5*$w3dData(bbox.maxspan)}]
    foreach cat  [list aspect motion] {
        for {set i 0} {$i < $w3dData(bndsample)} {incr i } {
            set j [expr {$i+1}]
            # x direction
            set part "axis.x"
            set x1 [expr { $w3dData(focus.point.x)+$i*$h/$w3dData(bndsample) }]
            set x2 [expr { $w3dData(focus.point.x)+$j*$h/$w3dData(bndsample) }]
            set y $w3dData(focus.point.y)
            set z $w3dData(focus.point.z)
            lappend globalList($cat) [list "a" $part $i $x1 $y $z red 1 0 0 $x2 $y $z]
             # y direction
            set part "axis.y"
            if {$w3dData(cylindrical) == 1} {
                set part "axis.r"
            }
            set y1 [expr { $w3dData(focus.point.y)+$i*$h/$w3dData(bndsample) }]
            set y2 [expr { $w3dData(focus.point.y)+$j*$h/$w3dData(bndsample) }]
            set x $w3dData(focus.point.x)
            set z $w3dData(focus.point.z)
            lappend globalList($cat) [list "a" $part $i $x $y1 $z green 1 0 0 $x $y2 $z]
             # z direction
            set part "axis.z"
            if {$w3dData(cylindrical) == 1} {
                set part "axis.r*theta"
            }
            set z1 [expr { $w3dData(focus.point.z)+$i*$h/$w3dData(bndsample) }]
            set z2 [expr { $w3dData(focus.point.z)+$j*$h/$w3dData(bndsample) }]
            set y $w3dData(focus.point.y)
            set x $w3dData(focus.point.x)
            lappend globalList($cat) [list "a" $part $i $x $y $z1 blue 1 0 0 $x $y $z2]
           
           
        }   
    
    }    
    foreach mode  [list aspect motion] {
        set w3dData($mode.order.x) [lsort -real -index 3 $globalList($mode) ]
        set w3dData($mode.order.y) [lsort -real -index 4 $globalList($mode) ]
        set w3dData($mode.order.z) [lsort -real -index 5 $globalList($mode) ]
        set w3dData($mode.order.3d)  $w3dData($mode.order.x)
    }
    
   
    return

}


proc viewer3d_rotate_view_base { quaternion } {
    global w3dData
    # update quaternion
   set w3dData(quaternion) [vect_normalize  $quaternion]
   
    # update ref vectors
    foreach vect [list vecx vecy vecz] {        
        set w3dData($vect) [ vect_normalize [qv_mult  $w3dData(quaternion) [lindex $w3dData($vect) 0] [lindex $w3dData($vect) 1] [lindex $w3dData($vect) 2]] ]
    }
   
     
}


proc viewer3d_rotate_view_coords { mode } {
    global w3dData

   
    set w3dData($mode.order.3d) []
    foreach elt $w3dData($mode.order.x) {
        
        set type [lindex $elt 0]
        set part [lindex $elt 1]
        set id [lindex $elt 2]
        
        
        set x [lindex $elt 3] 
        set y [lindex $elt 4]
        set z [lindex $elt 5]
        set nv [point_rotate  $x $y $z]
        set x [lindex $nv 0]
        set y [lindex $nv 1]
        set z [lindex $nv 2]
        
        
        set color [lindex $elt 6]
        
        set nx [lindex $elt 7]
        set ny [lindex $elt 8]
        set nz [lindex $elt 9]
        set nv [vect_rotate  $nx $ny $nz]
        set nx [lindex $nv 0]
        set ny [lindex $nv 1]
        set nz [lindex $nv 2]
        
       
        set x2 [lindex $elt 10]
        set y2 [lindex $elt 11]
        set z2 [lindex $elt 12]
        set nv [point_rotate  $x2 $y2 $z2]
        set x2 [lindex $nv 0]
        set y2 [lindex $nv 1]
        set z2 [lindex $nv 2]
        switch $type {
            "b" -
            "n" -
            "e" -
            "a" {
                lappend w3dData($mode.order.3d) [list "b" $part $id $x $y $z $color $nx $ny $nz $x2 $y2 $z2 0 0 0 0 0 0]
            }
            "f" {
                
                
                set x3 [lindex $elt 13]
                set y3 [lindex $elt 14]
                set z3 [lindex $elt 15]
                set nv [point_rotate  $x3 $y3 $z3]
                set x3 [lindex $nv 0]
                set y3 [lindex $nv 1]
                set z3 [lindex $nv 2]

                set x4 [lindex $elt 16]
                set y4 [lindex $elt 17]
                set z4 [lindex $elt 18]
                set nv [point_rotate  $x4 $y4 $z4]
                set x4 [lindex $nv 0]
                set y4 [lindex $nv 1]
                set z4 [lindex $nv 2]
                
                lappend w3dData($mode.order.3d) [list "f" $part $id $x $y $z $color $nx $ny $nz $x2 $y2 $z2 $x3 $y3 $z3 $x4 $y4 $z4 ]
            }
        }
    }
    switch $w3dData(view) {
        "xz" {    
            set w3dData($mode.order.3d) [lsort  -real -index 4 $w3dData($mode.order.3d) ]
        }
        "xy" {    
            set w3dData($mode.order.3d) [lsort  -real -index 5 $w3dData($mode.order.3d) ]
        }
        "zy" {    
            set w3dData($mode.order.3d) [lsort -decreasing -real -index 3 $w3dData($mode.order.3d) ]
        }
    }
}





proc viewer3d_real_to_pix {dir1 dir2 coords } {
    global w3dData
    set result []
    
    set map1 $w3dData(pix_per_m)
    set map2 $w3dData(pix_per_m)
    
    
    
    if {$w3dData(cylindrical) == 1 } {
        if {$dir1 =="z"} {
            set map1 $w3dData(pix_per_deg)
        }
        if {$dir2 =="z"} {
            set map2 $w3dData(pix_per_deg)
        }
    }
    
    
    foreach cp $coords {
            lappend result [expr {([lindex $cp 0]-$w3dData(focus.point.$dir1))*$map1}]
            lappend result [expr {([lindex $cp 1]-$w3dData(focus.point.$dir2))*$map2}]
    }
    
    
    return $result
}



proc viewer3d_redraw_side {mode} {
    global w3dData
    
    #set start_time [clock milliseconds]

    .view3d.gr1  delete all
    .view3d.gr2  delete all
    .view3d.gr3  delete all
    
    foreach elt $w3dData($mode.order.x) {
        
        set type [lindex $elt 0]
        set part [lindex $elt 1]
        set id [lindex $elt 2]
        
        set x [lindex $elt 3]
        set y [lindex $elt 4]
        set z [lindex $elt 5]
        
        set color [lindex $elt 6]
        
     
            
        set nx [lindex $elt 7]
        set ny [lindex $elt 8]
        set nz [lindex $elt 9]
        set x2 [lindex $elt 10]
        set y2 [lindex $elt 11]
        set z2 [lindex $elt 12]

        
        switch $type {
            "b" -
            "n" -
            "e" -
            "a" {
                .view3d.gr1 create line [viewer3d_real_to_pix x z [list [list $x $z] [list $x2 $z2] ]] -fill $w3dData(focuscolor) -tags "$part $part.line  $part.$id" -width 2 -state hidden
                .view3d.gr2 create line [viewer3d_real_to_pix x y [list [list $x $y] [list $x2 $y2] ]] -fill $w3dData(focuscolor) -tags "$part $part.line  $part.$id" -width 2 -state hidden
                .view3d.gr3 create line [viewer3d_real_to_pix z y [list [list $z $y] [list $z2 $y2] ]] -fill $w3dData(focuscolor) -tags "$part $part.line $part.$id" -width 2 -state hidden
                .view3d.gr1 create line [viewer3d_real_to_pix x z [list [list $x $z] [list $x2 $z2] ]] -fill $color -tags "$part $part.$id"
                .view3d.gr2 create line [viewer3d_real_to_pix x y [list [list $x $y] [list $x2 $y2] ]] -fill $color -tags "$part $part.$id"
                .view3d.gr3 create line [viewer3d_real_to_pix z y [list [list $z $y] [list $z2 $y2] ]] -fill $color -tags "$part $part.$id"
            }
            
            "f" {
                set x3 [lindex $elt 13]
                set y3 [lindex $elt 14]
                set z3 [lindex $elt 15]
                set x4 [lindex $elt 16]
                set y4 [lindex $elt 17]
                set z4 [lindex $elt 18]
                
                
                if {$ny < 0 && $w3dData(bfcull)==1 } {
                    #skip
                } else {
                    .view3d.gr1 create polygon [viewer3d_real_to_pix x z [list [list $x $z] [list $x2 $z2] [list $x3 $z3] [list $x4 $z4]]] -fill $color -tags "$part $part.poly $part.$id" -outline  ""
                }
                
                
                if {$nz < 0 && $w3dData(bfcull)==1 } {
                    #skip
                } else {
                    .view3d.gr2 create polygon [viewer3d_real_to_pix x y [list [list $x $y] [list $x2 $y2] [list $x3 $y3] [list $x4 $y4]]] -fill $color -tags "$part $part.poly $part.$id" -outline  ""
                }
                
                if {$nx > 0 && $w3dData(bfcull)==1 } {
                    #skip
                } else {
                    .view3d.gr3 create polygon [viewer3d_real_to_pix z y [list [list $z $y] [list $z2 $y2] [list $z3 $y3] [list $z4 $y4]]] -fill $color -tags "$part $part.poly  $part.$id" -outline  ""
                }
                
                

            }
            
        }
        
        
    }
    
    
    
    foreach elt  $w3dData(aspect.order.y) {    
        set part [lindex $elt 1]
        set id [lindex $elt 2]
        .view3d.gr1 raise "$part.$id"
    }
    #.view3d.gr1 scale all 0 0 1 -1
    .view3d.gr1 move all [expr { 0.7*$w3dData(width)}] [expr { 0.5*$w3dData(width)}]
    
    
    foreach elt  $w3dData(aspect.order.z) {    
        set part [lindex $elt 1]
        set id [lindex $elt 2]
        .view3d.gr2 raise "$part.$id"
    }
    .view3d.gr2 scale all 0 0 1 -1
    .view3d.gr2 move all [expr { 0.7*$w3dData(width)}] [expr { 0.5*$w3dData(width)}]
    
    
    foreach elt [lreverse $w3dData(aspect.order.x)] {    
        set part [lindex $elt 1]
        set id [lindex $elt 2]
        .view3d.gr3 raise "$part.$id"
    }
    
    
    .view3d.gr3 scale all 0 0 1 -1
    .view3d.gr3 move all [expr { 0.7*$w3dData(width)}] [expr { 0.5*$w3dData(width)}]
    
 
    
    #update 
    #set stop_time [clock milliseconds]
    #set w3dData(rendertime) "RenderSide: [format "%0.3f" [expr { ($stop_time - $start_time)*0.001 }]] s."
    
}



proc vect_rotate { x y z} {
    global w3dData
    
    set xx [expr { [lindex $w3dData(vecx) 0]*$x + [lindex $w3dData(vecy) 0]*$y+  [lindex $w3dData(vecz) 0]*$z}]
    set yy [expr { [lindex $w3dData(vecx) 1]*$x + [lindex $w3dData(vecy) 1]*$y+  [lindex $w3dData(vecz) 1]*$z}]
    set zz [expr { [lindex $w3dData(vecx) 2]*$x + [lindex $w3dData(vecy) 2]*$y+  [lindex $w3dData(vecz) 2]*$z}]
    
    
    
    return [list $xx $yy $zz]
    
}


proc point_rotate { x y z} {
    global w3dData
    set x [expr { $x-$w3dData(focus.point.x)}]
    set y [expr { $y-$w3dData(focus.point.y)}]
    set z [expr { $z-$w3dData(focus.point.z)}]
    
    set xx [expr { [lindex $w3dData(vecx) 0]*$x + [lindex $w3dData(vecy) 0]*$y+  [lindex $w3dData(vecz) 0]*$z}]
    set yy [expr { [lindex $w3dData(vecx) 1]*$x + [lindex $w3dData(vecy) 1]*$y+  [lindex $w3dData(vecz) 1]*$z}]
    set zz [expr { [lindex $w3dData(vecx) 2]*$x + [lindex $w3dData(vecy) 2]*$y+  [lindex $w3dData(vecz) 2]*$z}]
    
    #
    set xx [expr { $xx +$w3dData(focus.point.x)}]
    set yy [expr { $yy +$w3dData(focus.point.y)}]
    set zz [expr { $zz +$w3dData(focus.point.z)}]
    
    return [list $xx $yy $zz]
    
}

proc viewer3d_redraw_main { mode  } {
    global w3dData
    
    
    #set start_time [clock milliseconds]

    .view3d.gr0  delete all
    
  
    set count 0
    set items [llength $w3dData($mode.order.3d)]
    set sgap 0
    set gap [expr {[llength $w3dData($mode.order.3d)]/5}]
    set autow 0.5
    set autow2 1.5
    
    foreach elt $w3dData($mode.order.3d) {
        incr count
        incr sgap
        if {$sgap> $gap} {
            set sgap 0
            set autow [expr {$autow+0.5}]
            set autow2 [expr {$autow+1}]
        }
        set completion [expr {sqrt(1.*$count/$items) }]
        set type [lindex $elt 0]
        set part [lindex $elt 1]
        set id [lindex $elt 2]
        
        set x [lindex $elt 3]
        set y [lindex $elt 4]
        set z [lindex $elt 5]
        
        set color [lindex $elt 6]
        set color [ mix_color2 $w3dData(bg) $color $completion]
        
        
            
             
        set nx [lindex $elt 7]
        set ny [lindex $elt 8]
        set nz [lindex $elt 9]
        set x2 [lindex $elt 10]
        set y2 [lindex $elt 11]
        set z2 [lindex $elt 12]

        
        
        switch $type {
            "b" -
            "n" -
            "e" -
            "a" {     
                switch $w3dData(view) {
                    "xz" {    
                    .view3d.gr0 create line  [viewer3d_real_to_pix x z [list [list $x $z] [list $x2 $z2]]] -fill $w3dData(focuscolor) -tags "$part $part.line $part.$id" -width $autow2 -state hidden   
                    .view3d.gr0 create line  [viewer3d_real_to_pix x z [list [list $x $z] [list $x2 $z2]]] -fill $color -tags "$part  $part.$id" -width $autow
                    }
                    "xy" {    
                    .view3d.gr0 create line  [viewer3d_real_to_pix x y [list [list $x $y] [list $x2 $y2]]] -fill $w3dData(focuscolor) -tags "$part $part.line $part.$id" -width $autow2 -state hidden   
                    .view3d.gr0 create line  [viewer3d_real_to_pix x y [list [list $x $y] [list $x2 $y2]]] -fill $color -tags "$part  $part.$id" -width $autow 
                    }
                    "zy" {    
                    .view3d.gr0 create line  [viewer3d_real_to_pix z y [list [list $z $y] [list $z2 $y2]]] -fill $w3dData(focuscolor) -tags "$part $part.line $part.$id" -width $autow2 -state hidden   
                    .view3d.gr0 create line  [viewer3d_real_to_pix z y [list [list $z $y] [list $z2 $y2]]] -fill $color -tags "$part  $part.$id" -width $autow 
                    }
                }
            }
            
            "f" {
                set x3 [lindex $elt 13]
                set y3 [lindex $elt 14]
                set z3 [lindex $elt 15]
                set x4 [lindex $elt 16]
                set y4 [lindex $elt 17]
                set z4 [lindex $elt 18]
               
                switch $w3dData(view) {
                    "xz" {
                        if {$ny < 0 && $w3dData(bfcull)==1 } {
                            #skip
                        } else {
                            .view3d.gr0 create polygon [viewer3d_real_to_pix x z [list [list $x $z] [list $x2 $z2] [list $x3 $z3] [list $x4 $z4]]] -fill $color -tags "$part $part.poly $part.$id"  -outline ""
                        }
                    }
                    "xy" {
                        if {$nz < 0 && $w3dData(bfcull)==1 } {
                            #skip
                        } else {
                            .view3d.gr0 create polygon [viewer3d_real_to_pix x y [list [list $x $y] [list $x2 $y2] [list $x3 $y3] [list $x4 $y4]]] -fill $color -tags "$part $part.poly $part.$id"  -outline  ""
                        }
                    }
                    "zy" {
                        if {$nx > 0 && $w3dData(bfcull)==1 } {
                            #skip
                        } else {
                            .view3d.gr0 create polygon [viewer3d_real_to_pix z y [list [list $z $y] [list $z2 $y2] [list $z3 $y3] [list $z4 $y4]]] -fill $color -tags "$part $part.poly $part.$id"  -outline "" 
                        }
                    }
                }
                
                

            }
            
        }
        
        
        
        

        if {$mode != "motion"} {
            .view3d.gr0 bind $part <Enter>  [subst {
                .view3d.gr0 itemconfigure $part.poly  -outline $w3dData(focuscolor)
                .view3d.gr1 itemconfigure $part.poly  -outline $w3dData(focuscolor)
                .view3d.gr2 itemconfigure $part.poly  -outline $w3dData(focuscolor)
                .view3d.gr3 itemconfigure $part.poly  -outline $w3dData(focuscolor)
                .view3d.gr0 itemconfigure $part.line  -state normal 
                .view3d.gr1 itemconfigure $part.line  -state normal 
                .view3d.gr2 itemconfigure $part.line  -state normal 
                .view3d.gr3 itemconfigure $part.line  -state normal 
              
              
              
              
                canvas_text_highlighted .view3d.gr0 %x %y  "$part" "information"    
            }]
            .view3d.gr0 bind $part <Leave>  [subst {
                .view3d.gr0 delete "information"
                .view3d.gr0 itemconfigure $part.poly -outline ""
                .view3d.gr1 itemconfigure $part.poly -outline ""
                .view3d.gr2 itemconfigure $part.poly -outline ""
                .view3d.gr3 itemconfigure $part.poly -outline ""
               
                .view3d.gr0 itemconfigure $part.line -state hidden
                .view3d.gr1 itemconfigure $part.line  -state hidden 
                .view3d.gr2 itemconfigure $part.line  -state hidden 
                .view3d.gr3 itemconfigure $part.line  -state hidden 
                
            }]
        }
        
        
    }
    #
    #set vx [lindex $w3dData(view) 0]
    #set vy [lindex $w3dData(view) 1]
    #set vz [lindex $w3dData(view) 2]
    #
    #set maxv $vx
    #set orient x
    #
    #if {abs($vy) > abs($maxv)} {
    #    set maxv $vy
    #    set orient y
    #}
    #if {abs($vz) > abs($maxv)} {
    #    set maxv $vz
    #    set orient z
    #}
    #
    #set order3d $w3dData(aspect.order.$orient)
    #if {$maxv > 0} {
    #    set order3d [lreverse $order3d]
    #}
    #
    #foreach elt $order3d {    
    #    set part [lindex $elt 1]
    #    set id [lindex $elt 2]
    #    .view3d.gr0 raise "$part.$id"
    #}
    #
    #puts "order $orient $maxv"
    #
    
    switch $w3dData(view) {
        "xz" {
            .view3d.gr0 scale all 0 0 3 3
            .view3d.gr0 move all [expr { 2*$w3dData(width)}] [expr { 1.5*$w3dData(width)}]
        }    
        "xy" {
            .view3d.gr0 scale all 0 0 3 -3
            .view3d.gr0 move all [expr { 2*$w3dData(width)}] [expr { 1.5*$w3dData(width)}]
        }    
        "zy" {
            .view3d.gr0 scale all 0 0 3 -3
            .view3d.gr0 move all [expr { 2*$w3dData(width)}] [expr { 1.5*$w3dData(width)}]
        }    
    }
    
}


#########################
# quaternions utilities #
#########################



proc vect_normalize { v {tolerance 0.00001}} {
    set mag2  0
    foreach c $v {
        set mag2 [expr {$mag2 + $c*$c}]
    }
    if { [expr {abs($mag2 - 1.0)}] > $tolerance } {
        set mag [expr {sqrt($mag2)}]
        set vnew []
        foreach c $v {
            lappend vnew [expr {$c/$mag}]
        }
       set v $vnew
    }
    return $v
}
    
 
proc q_mult {q1 q2} {
    set w1 [lindex $q1 0]
    set x1 [lindex $q1 1]
    set y1 [lindex $q1 2]
    set z1 [lindex $q1 3]
    set w2 [lindex $q2 0]
    set x2 [lindex $q2 1]
    set y2 [lindex $q2 2]
    set z2 [lindex $q2 3]
    set w [expr { $w1 * $w2 - $x1 * $x2 - $y1 * $y2 - $z1 * $z2 }]
    set x [expr { $w1 * $x2 + $x1 * $w2 + $y1 * $z2 - $z1 * $y2 }]
    set y [expr { $w1 * $y2 + $y1 * $w2 + $z1 * $x2 - $x1 * $z2 }]
    set z [expr { $w1 * $z2 + $z1 * $w2 + $x1 * $y2 - $y1 * $x2 }]
    set q [list $w $x $y $z]
    
    
    return $q
}

proc q_conjugate {q} {
    set q [vect_normalize $q]
    set w [lindex $q 0]
    set x [expr { -1.0*[lindex $q 1]}]
    set y [expr { -1.0*[lindex $q 2]}]
    set z [expr { -1.0*[lindex $q 3]}]
    return [list $w $x $y $z]
}



proc qv_mult {q u v w} {
    #set q [vect_normalize $q]
    
    set q2 [list 0 $u $v $w]
    
    set r [q_mult [q_mult $q $q2] [q_conjugate $q]]
    
    set x [lindex $r 1]
    set y [lindex $r 2]
    set z [lindex $r 3]
    
    return [list $x $y $z]
}


proc axisangle_to_q { v  theta} {
    
    set v [vect_normalize $v]
    set x [lindex $v 0]
    set y [lindex $v 1]
    set z [lindex $v 2]
    set theta [expr {$theta / 2}]
    set w [expr { cos($theta) }]
    set x [expr { $x * sin($theta)}]
    set y [expr { $y * sin($theta)}]
    set z [expr { $z * sin($theta)}]
    return [list $w $x $y $z]
}

proc q_to_axisangle { q } {
    set w [lindex $q 0]
    set x [lindex $q 1]
    set y [lindex $q 2]
    set z [lindex $q 3]
    set [expr { 2.*acos($w)}]
    return [list [list $x $y $z] $theta]
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