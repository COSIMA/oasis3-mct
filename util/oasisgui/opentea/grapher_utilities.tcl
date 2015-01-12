#  This program is under CECILL_B licence. See footer for details.


proc grapher_highlight_node {wincan anchor match side index} {
    set a_x [lindex $anchor 0]
    set a_y [lindex $anchor 1]
    
    switch $match {
        "value" {
            set color red
        }
        "children" {
            set color green4
        }
        
    }
    set h 5
    $wincan create oval [expr {$a_x-$h}] [expr {$a_y-$h}] [expr {$a_x+$h}] [expr {$a_y+$h}] -outline $color -width 2 -tags "graph $side"
    set h 10
    canvas_text_vector $wincan [expr {$a_x+$h}] [expr {$a_y+$h}] $index nw 10 0 $color "graph $side"
}


proc grapher_drawnode {wincan anchor  maxdepth childnb kinnb dsname dsaddress side value shade {title "notitle"}  } {
    
    set color "yellow"
    if {[string is double $value]} {set color "orange"}
    if {[string is integer $value]} {set color "red"}
    if {[llength $value] >1} {set color "blue"}
    if {$value==""} {set color "white"}
 
    set color [shadeColor $color $shade]
    set shadeblack [shadeColor "black" $shade]
    
    set dist [expr {10+pow($maxdepth,1.8)* 2+pow($childnb,0.7)* 2+pow($kinnb,2)* 0.1}]
    set a_x [lindex $anchor 0]
    set a_y [lindex $anchor 1]
    set a_angle [lindex $anchor 2]
    #puts "$a_x+$dist*cos($a_angle*3.1416/180.0)"
    set b_x [expr {$a_x+$dist*cos(($a_angle-90)*3.1416/180.0)}]
    set b_y [expr {$a_y+$dist*sin(($a_angle-90)*3.1416/180.0)}]
    
    set tag "$dsname#$dsaddress"
    $wincan create line $a_x $a_y $b_x $b_y -fill $shadeblack -tags "graph $side" 
    
    set h 4
    #$wincan create oval [expr {$b_x-$h}] [expr {$b_y-$h}] [expr {$b_x+$h}] [expr {$b_y+$h}] -fill $shadeblack  -width 0 -tag  "$tag handle graph $side"
    set h 3
    $wincan create oval [expr {$b_x-$h}] [expr {$b_y-$h}] [expr {$b_x+$h}] [expr {$b_y+$h}] -fill $color -width 0 -tag  "$tag handle graph $side"
    
    set lastnode [lindex [split $dsaddress "."] end]
    
    set msg [string map { "[" "(" "]" ")" } "$title\n $value" ] 
    $wincan bind $tag <Enter> [subst {grapher_showlocation $wincan  "$msg" }]
    $wincan bind $tag <Leave> [subst {$wincan delete "pointer"}]
    set new_anchor "$b_x $b_y $a_angle"
    return $new_anchor
}

proc grapher_children_directions {dsname a_angle dsaddress} {
    upvar $dsname dataset
    set children $dataset($dsaddress-children)
    set list_angle_child ""
    
    if {$a_angle > 0 && $a_angle < 180} {
        set shift 5
    } elseif {$a_angle < 0} {
        set shift -5
    } else {
        set shift 0
    }
    
    if {$children != ""} {
        set nchild [llength $children]
        
        switch $nchild {
            "1" {
                set list_angle_child 0 
            }
            "2" {
                set list_angle_child {-30 +30}    
            }
            "3" {
                set list_angle_child {-45 0 45}
            }
            "4" {
                set list_angle_child {-60 -20 20 60}
            }
            "5" {
                set list_angle_child {-90 -45 0 45 90}
            }
            "6" {
                set list_angle_child {-125 -75 -25 25 75 125}
            }
            default {
                set list_angle_child ""
                for {set i 0} {$i < $nchild} {incr i} {
                     set angle [expr { -125 + (($i)*1.0/($nchild-1))*2.0*125    }]
                    lappend list_angle_child $angle
                }
            }
        }
        set final_list ""
            for {set i 0} {$i < $nchild} {incr i} {
            lappend final_list [expr {$shift + [lindex $list_angle_child $i]}]
        }
        set list_angle_child $final_list
    }
    
    
    return $list_angle_child
}

proc grapher_trace { address dsname  anchor dsaddress side} {
    
    upvar $dsname dataset
    
    global widgetInfo
    
    set wincan $widgetInfo($address-wincan) 
    
    set title ""
    if {$dsaddress != "dataset"} {
        set title  [grapher_gettitle_of_child dataset "$dsaddress" ] 
    }
    
    
    # draw node
    set new_anchor [grapher_drawnode $wincan $anchor $dataset($dsaddress-maxdepth) $dataset($dsaddress-childnb) $dataset($dsaddress-kinnb) $dsname $dsaddress $side $dataset($dsaddress-value) 0.0 $title]
    
    # call iteration for children children
    set children $dataset($dsaddress-children)
    
    set b_x [lindex $new_anchor 0]
    set b_y [lindex $new_anchor 1]
    set a_angle [lindex $new_anchor 2]
    set list_angle_child [grapher_children_directions dataset $a_angle $dsaddress]
    set child_id 0
    
    
   
    #foreach child  $children 
    foreach child [grapher_ordered_children dataset $dsaddress all] {
        set anglechild [expr {$a_angle+[lindex $list_angle_child $child_id]}]
        grapher_trace $address dataset "$b_x $b_y $anglechild" "$dsaddress.$child" $side
        incr child_id
    }
    
}

proc grapher_ordered_children {dsname dsaddress subset} {
    upvar $dsname dataset
    set children $dataset($dsaddress-children)
    if {$subset == "all"} {
        set subset $children
    }
    
    set list_item_childnb ""
    foreach child  $dataset($dsaddress-children) {
        if {[lsearch $subset $child ]!= -1} {
            lappend list_item_childnb [list $child $dataset($dsaddress.$child-childnb)]
        }
    }
    set ordered_list_item_childnb [lsort -integer -decreasing -index 1 $list_item_childnb]
    
    set list_pyramid ""
    set side "left"
    foreach pair $ordered_list_item_childnb {
        set child [lindex $pair 0]
        switch $side {
            "left" {
                set list_pyramid [ concat  $child $list_pyramid ]
                set side "right"
            }
            "right" {
                set list_pyramid [ concat   $list_pyramid $child]
                set side "left"
            }
        }
    }
    return  $list_pyramid
}
proc grapher_showlocation { wincan msg } {
    
    $wincan delete "pointer"
    set x [$wincan canvasx [expr {[winfo pointerx $wincan ] - [winfo rootx $wincan ]}]]
    set y [$wincan canvasy [expr {[winfo pointery $wincan ] - [winfo rooty $wincan ]}]]
    canvas_text_highlighted $wincan $x $y  $msg "pointer" 
}

proc grapher_fill {address dsname dsaddress indent dump} {
    global widgetInfo
    upvar $dsname dataset
    incr indent 
    set node [list "$indent" [lindex [split $dsaddress "."] end] "$dataset($dsaddress-value)"]
    set children $dataset($dsaddress-children)
    
    # write array representation
    ####################################
    # for graph issues
    # add the max depth attribute
    # add the number of children
    set dataset($dsaddress-maxdepth) 0
    set dataset($dsaddress-childnb) 0
    set fatheraddress [join [lrange [split $dsaddress "."] 0 end-1] "."]
    if {$fatheraddress == ""} {
        set kin 0
    } else {
        set kin $dataset($fatheraddress-childnb)
    }
    
    set dataset($dsaddress-kinnb) $kin
    
    if {$children != ""} {
        set dataset($dsaddress-childnb) [llength $children]
        set dataset($dsaddress-maxdepth) 1
        set tmpadr [join [lrange [split $dsaddress "."] 0 end-1] "."]
        set depth 1
        while {$tmpadr != ""} {
            incr depth
            if {$depth > $dataset($tmpadr-maxdepth)} {
                set dataset($tmpadr-maxdepth) $depth
            }
            set tmpadr [join [lrange [split $tmpadr "."] 0 end-1] "."]
        }
    }
    
    
    ####################################
    
    # probably shoud add number of children here!
    
       
    foreach child $children {     
        grapher_fill $address "dataset" "$dsaddress.$child" $indent $dump
    }
     
    return 
}





proc grapher_html_write_table { address msg mode {lvl 100} } {
    global widgetInfo    
    set folder $widgetInfo($address-folder) 
    set filerootname $widgetInfo($address-filerootname) 
    
    set promote_begin ""
    set promote_end ""
    
    if {$lvl == 1} {
        set promote_begin "<h2>"
        set promote_end "<\h2>"
    }
    
    if {$lvl == 2} {
        set promote_begin "<h3>"
        set promote_end "<\h3>"
    }
    
    if {$lvl == 3} {
        set promote_begin "<h4>"
        set promote_end "<\h4>"
    }
    if {$lvl == 4} {
        set promote_begin "<h5>"
        set promote_end "<\h5>"
    }
    
    
    
    #add text in html report
    if {$widgetInfo($address-dump)} {
        set htmlfile [open "[file join $folder $filerootname].html" a+]
        switch $mode {
            "start" {
                puts $htmlfile "<table border='0'> <caption> $msg </caption>"
            }
            "end" {
                puts $htmlfile "</table>"
            }
             "headers" {
                 puts $htmlfile "    <tr>"
                 foreach item $msg {
                    puts $htmlfile "      <th>"
                    puts $htmlfile "      $item"
                    puts $htmlfile "      </th>"
                 }
                 puts $htmlfile "    </tr>"
            }
            
            "value" {
                 puts $htmlfile "    <tr>"
                 foreach item $msg {
                    puts $htmlfile "      <td>"
                    
                    puts $htmlfile "  <span style=\"color:red\">    $promote_begin [limit_string $item 50] $promote_end </span>"
                    puts $htmlfile "      </td>"
                 }
                 puts $htmlfile "    </tr>"
            }
            "children" {
                 puts $htmlfile "    <tr>"
                 foreach item $msg {
                    puts $htmlfile "      <td>"
                    puts $htmlfile "   <span style=\"color:green\">   $promote_begin  [limit_string $item 50] $promote_end </span>"
                    puts $htmlfile "      </td>"
                 }
                 puts $htmlfile "    </tr>"
            }
            default {
                 puts $htmlfile "    <tr>"
                 foreach item $msg {
                    puts $htmlfile "      <td>"
                    puts $htmlfile "    $promote_begin [limit_string $item 50] $promote_end"
                    puts $htmlfile "      </td>"
                 }
                 puts $htmlfile "    </tr>"
            }
        }
        close $htmlfile
    }
}



proc grapher_compare {address dsname1 dsname2 dsaddress anchor1 anchor2 } {
    upvar $dsname1 dataset1
    upvar $dsname2 dataset2
    global widgetInfo
    
    grapher_setup_XML dataset1 $dsaddress
    
    set wincan $widgetInfo($address-wincan)
    set wintv   $widgetInfo($address-wintv)
    set folder $widgetInfo($address-folder) 
    
    
    
    set value1 $dataset1($dsaddress-value)
    set value2 $dataset2($dsaddress-value)
    set children1 $dataset1($dsaddress-children)
    set children2 $dataset2($dsaddress-children)
    
    
        
    set shade 0.7
    set match "match"
    set v1 $value1
    set v2 ""
    
    if {$value1 != $value2} {
        set match "value"
        incr widgetInfo($address-mismatch)
        set shade 0.0
        set v1  "($widgetInfo($address-mismatch)) $value1"
        set v2  "($widgetInfo($address-mismatch)) $value2"
    }
    if {$children1 != $children2} {
        set only1 ""
        set only2 ""
        set both ""
        
        if {$match != "value"} {
         incr widgetInfo($address-mismatch)
        }
        
        foreach child $children1 {
            if {[lsearch  $children2 $child ] ==-1 } {
                lappend only1 $child
            } else {
                lappend both $child
            }
        }
        
        foreach child $children2 {
            if {[lsearch  $children1 $child ] ==-1 } {
                lappend only2 $child
            } 
        }
        
        if {$only1 != ""} {
            set v1 "($widgetInfo($address-mismatch)) : [join $only1 ";"]"
        } else {
            set v1 ""
        }
        if {$only2 != ""} {
            set v2 "($widgetInfo($address-mismatch)) : [join $only2 ";"]"
        } else {
            set v2 ""
        }
        set match "children"
        set shade 0.0
    }
    
    
        
    set title ""
    if {$dsaddress != "dataset"} {
        set title  [grapher_gettitle_of_child dataset1 "$dsaddress" ] 
        set lvl [llength [split "$dsaddress" "."] ]
        grapher_html_write_table $address [list $title $v1 $v2] $match $lvl
        $wintv insert [crop_address $dsaddress 1] end -id $dsaddress -text "$title" -values [list $v1 $v2] -open $widgetInfo($address-tvopen) -tag $match
        if {$match != "match"} {
            $wintv see $dsaddress
        }
    }
    
    
    # draw node
    set new_anchor1 [grapher_drawnode $wincan $anchor1 $dataset1($dsaddress-maxdepth) $dataset1($dsaddress-childnb) $dataset1($dsaddress-kinnb) $dsname1 $dsaddress left $dataset1($dsaddress-value) $shade $title]
    set new_anchor2 [grapher_drawnode $wincan $anchor2 $dataset2($dsaddress-maxdepth) $dataset2($dsaddress-childnb) $dataset2($dsaddress-kinnb) $dsname2 $dsaddress right $dataset2($dsaddress-value) $shade $title]
    
    

    set b1_x [lindex $new_anchor1 0]
    set b1_y [lindex $new_anchor1 1]
    set a1_angle [lindex $new_anchor1 2]
    set list_angle_child1 [grapher_children_directions dataset1 $a1_angle $dsaddress]
    
    set b2_x [lindex $new_anchor2 0]
    set b2_y [lindex $new_anchor2 1]
    set a2_angle [lindex $new_anchor2 2]
    set list_angle_child2 [grapher_children_directions dataset2 $a2_angle $dsaddress]
  
    switch $match {
        "match" {
            set child_id 0
            foreach child [grapher_ordered_children dataset1 $dsaddress all] {
                set anglechild [expr {$a1_angle+[lindex $list_angle_child1 $child_id]}]
                grapher_compare $address dataset1 dataset2 "$dsaddress.$child" "$b1_x $b1_y $anglechild" "$b2_x $b2_y $anglechild"
                     
                incr child_id
            }
        }
        "value" {
            grapher_highlight_node $wincan "$b1_x $b1_y 0" $match left $widgetInfo($address-mismatch)
            grapher_highlight_node $wincan "$b2_x $b2_y 0" $match right $widgetInfo($address-mismatch)
            set child_id 0
            foreach child [grapher_ordered_children dataset1 $dsaddress all] {
                set anglechild [expr {$a1_angle+[lindex $list_angle_child1 $child_id]}]
                grapher_trace $address dataset1  "$b1_x $b1_y $anglechild" "$dsaddress.$child" left
                incr child_id
            }
            set child_id 0
            foreach child [grapher_ordered_children dataset2 $dsaddress all] {
                set anglechild [expr {$a2_angle+[lindex $list_angle_child2 $child_id]}]
                grapher_trace $address dataset2  "$b2_x $b2_y $anglechild" "$dsaddress.$child" right
                incr child_id
            }
        }
        "children" {

            grapher_highlight_node $wincan "$b1_x $b1_y 0" $match left $widgetInfo($address-mismatch)
            grapher_highlight_node $wincan "$b2_x $b2_y 0" $match right $widgetInfo($address-mismatch)
            set child_id 0
            foreach child [grapher_ordered_children dataset1 $dsaddress $both]   {
                set anglechild [expr {$a1_angle+[lindex $list_angle_child1 $child_id]}]
                grapher_compare $address dataset1 dataset2 "$dsaddress.$child" "$b1_x $b1_y $anglechild" "$b2_x $b2_y $anglechild" 
                incr child_id
            }
            foreach child [grapher_ordered_children dataset1 $dsaddress $only1]   {
                set anglechild [expr {$a1_angle+[lindex $list_angle_child1 $child_id]}]
                grapher_trace $address dataset1  "$b1_x $b1_y $anglechild" "$dsaddress.$child" left
                incr child_id
            }
            
            set child_id 0
            foreach child $both {
                incr child_id
            }
            foreach child [grapher_ordered_children dataset2 $dsaddress $only2]  {
                set anglechild [expr {$a2_angle+[lindex $list_angle_child2 $child_id]}]
                grapher_trace $address dataset2  "$b2_x $b2_y $anglechild" "$dsaddress.$child" right
                incr child_id
            }
            
        }
    }
    return 
}



proc grapher_loadfile {dsname filename} {
    upvar $dsname dataset
    
    set chout [ open $filename r ]
    set out [ split [ read $chout] "\n" ]
    close $chout

    foreach line $out {
        set linesplit [split $line "="]
        set key [lindex $linesplit 0 ]
        set value [string trim [join [lrange $linesplit 1 end ] "="]  " " ]
        set dataset($key) $value   
    }
    return 
}


proc grapher_zoom {wincan factor} {
     set y0 0
     set x0 0
     $wincan scale all $x0 $y0 $factor $factor
     $wincan configure -scrollregion [ $wincan bbox all]
}


    
proc grapher_bboxgraphs {address } {
    global widgetInfo
    
    set wincan $widgetInfo($address-wincan)
  
    set bboxright  [$wincan bbox right]
    set bboxleft [$wincan bbox left]
    
    set xml [lindex $bboxleft 0]
    set yml [lindex $bboxleft 1]
    set xpl [lindex $bboxleft 2]
    set ypl [lindex $bboxleft 3]
    
    set dxl [expr {$xpl-$xml}]
    set dyl [expr {$ypl-$yml}]
    set mid_xl [expr {0.5*($xpl+$xml)}]
    set mid_yl [expr {0.5*($ypl+$yml)}]
    
    set hoval 15
    if {$bboxright != ""} {
        set xmr [lindex $bboxright 0]
        set ymr [lindex $bboxright 1]
        set xpr [lindex $bboxright 2]
        set ypr [lindex $bboxright 3]
        
        
        set dxr [expr {$xpr-$xmr}]
        set dyr [expr {$ypr-$ymr}]
        set mid_xr [expr {0.5*($xpr+$xmr)}]
        set mid_yr [expr {0.5*($ypr+$ymr)}]
        #$wincan create oval [expr {$mid_xr-0.3*$dxr}] [expr {$mid_yr-0.3*$dxr}] [expr {$mid_xr+0.3*$dxr}] [expr {$mid_yr+0.3*$dxr}] -fill [shadeColor [grapher_color right] 0.8] -outline [shadeColor [grapher_color right] 0.5] -tags "right graph circles"
        #$wincan create oval [expr {$mid_xl-0.3*$dxl}] [expr {$mid_yl-0.3*$dxl}] [expr {$mid_xl+0.3*$dxl}] [expr {$mid_yl+0.3*$dxl}] -fill [shadeColor [grapher_color left] 0.8] -outline [shadeColor [grapher_color left] 0.5] -tags "left graph circles"
        $wincan create oval -$hoval -$hoval $hoval $hoval -fill [shadeColor [grapher_color right] 0.8] -outline [shadeColor [grapher_color right] 0.5] -tags "right graph circles"
        $wincan create oval -$hoval -$hoval $hoval $hoval -fill [shadeColor [grapher_color left] 0.8] -outline [shadeColor [grapher_color left] 0.5] -tags "left graph circles"
        
        
        canvas_text_vector $wincan [expr {$mid_xr-0.5*$dxr}] [expr {$mid_yr+0.5*$dyr+ 10}] $widgetInfo($address-filerootname2)  ne 10 0 [grapher_color right] "graph right"
        canvas_text_vector $wincan [expr {$mid_xl+0.5*$dxl}] [expr {$mid_yl+0.5*$dyl+ 10}] $widgetInfo($address-filerootname1)  nw 10 0 [grapher_color left] "graph left"
        
        # centering graphs
        $wincan move right [expr {-$mid_xr+0.5*$dxr+10}] [expr {-$mid_yr}]
    } else {
        #$wincan create oval [expr {$mid_xl-0.3*$dxl}] [expr {$mid_yl-0.3*$dxl}] [expr {$mid_xl+0.3*$dxl}] [expr {$mid_yl+0.3*$dxl}] -fill [shadeColor black 0.8] -outline [shadeColor black 0.5] -tags "left graph circles"
        $wincan create oval -$hoval -$hoval $hoval $hoval -fill [shadeColor black 0.8] -outline [shadeColor black 0.5] -tags "left graph circles"
        canvas_text_vector $wincan [expr {$mid_xl}] [expr {$mid_yl+0.5*$dyl+ 10}] $widgetInfo($address-filerootname)  center 10 0 black "graph left"
    }
    
    # add oval behind
    $wincan lower "circles"
    
    # centering graphs
    $wincan move left  [expr {-$mid_xl-0.5*$dxl-10}] [expr {-$mid_yl}]
    
     
}

proc grapher_color {tag} {
    set color black

    switch $tag {
        "left" {
            set color chocolate
        }
        "right" {
            set color purple
        }
        "both" {
            set color grey20
        }
        "treeval" {
            set color DarkBlue
        }
        
        "default" {
            set color black
        }
    }
}


proc grapher_init_treeview {win address tv type} {
    global widgetInfo
    
    $tv tag configure "value" -background red -foreground white
    $tv tag configure "children" -background green -foreground white
    
    
    # clean treeview
    if {[$tv exists "dataset"]} {
        foreach child [$tv children "dataset"] {
             $tv delete $child
        }
        
    } else {
        $tv insert {} end -id "dataset" -text "dataset" -open true
    }
    
    switch $type {
        "single" {
            set widgetInfo($address-tvopen) "true"
           
            $tv heading 0 -text "Value"
            $tv heading 1 -text ""
            $tv column 0 -anchor center
            $tv column 1 -anchor center
            
            
           }
        "double" {
            set widgetInfo($address-tvopen) "false"
            
            $tv heading 0 -text "Left Value"
            $tv heading 1 -text "Right Value"
            $tv column 0 -anchor center
            $tv column 1 -anchor center
            
        }
    }
    
}

proc grapher_array_to_treeview {win address tv arrayname dsaddress} {
    global widgetInfo 
    upvar $arrayname dataset
    
    grapher_setup_XML dataset $dsaddress
    
    foreach child $dataset($dsaddress-children) {
        
        set title  [grapher_gettitle_of_child dataset "$dsaddress.$child" ] 
        
        set lvl [llength [split "$dsaddress.$child" "."] ]
        grapher_html_write_table  $address [list $title $dataset($dsaddress.$child-value)] default $lvl
        
        $tv insert $dsaddress end -id $dsaddress.$child -text $title -values "{$dataset($dsaddress.$child-value)} {}" -open $widgetInfo($address-tvopen)
        grapher_array_to_treeview $win $address $tv dataset $dsaddress.$child
    }
    
    
}

proc grapher_gettitle_of_child { arrayname dsaddress } {
    global tmpXMLtree
    upvar $arrayname dataset
    
    # get last item
    set last [lindex [split $dsaddress "."] end]
    
    
    # build XMLaddress
    set XMLaddress  "root [dTree_cleanKey [lrange [ split "$dsaddress" "." ] 1 end ]] "
            
    # remove item, a lavel which does not exists in XML tree
    if {[lsearch $XMLaddress "item"]} {
        lremove XMLaddress "item"
    }
    
    # If title exist , set it
    if {[dTree_attrExists $tmpXMLtree $XMLaddress "title"]} {
        set title [dTree_getAttribute $tmpXMLtree $XMLaddress "title"]
    } else {
        #puts "$dsaddress without title"
        set title [string totitle $last]
    }
    
    # case of named nultiple
    if {[lindex [split $last "_"] 0] == "item"} {
        set title "$dataset($dsaddress-value)"
    }
    
    
    return $title
}

proc grapher_setup_XML {arrayname dsaddress} {
    global tmpXMLtree widgetInfo
    upvar $arrayname dataset
    
    if { $dsaddress == "dataset"} {
        set appli $dataset($dsaddress-children)
        if {$appli in $widgetInfo(applicationList)} {
            log "Loading XML of $appli"
            set modelPath [file join  $widgetInfo(libraryPath) $appli XML]
            set dataPath [file join $widgetInfo(libraryPath)]
            set tmpXMLtree ""
            OpenTeaXML2tree tmpXMLtree $modelPath $dataPath
        }
    }

}

proc grapher_create { win address wincan wintv folder file1 file2 dump} {
    global widgetInfo
    
    set widgetInfo($address-wincan) $wincan
    set widgetInfo($address-wintv) $wintv
    set widgetInfo($address-folder) $folder
    set widgetInfo($address-dump) $dump
    
    # cleaning
    $wincan delete all
   
    #  scroll
    bind $wincan   <ButtonPress> [subst {$wincan scan mark %x %y}]
    bind $wincan   <B1-Motion> [subst {$wincan scan dragto %x %y 1}]
    
    if {$file1 == $file2} {
        grapher_init_treeview $win $address $wintv "single"
        set widgetInfo($address-filerootname) $file1
        grapher_html_write_table  $address "Exploring simulation" start
        grapher_html_write_table  $address "Element $file1 " headers
        
        grapher_loadfile "ds1" [file join $folder "$file1.dat"]
        grapher_array_to_treeview $win $address $wintv "ds1" "dataset"
        grapher_fill $address  "ds1" "dataset" 0 1
        grapher_trace $address  "ds1"  {0 0 10.0} "dataset" left
        grapher_bboxgraphs $address
    } else {
        grapher_init_treeview $win $address $wintv "double"
        set widgetInfo($address-filerootname) "$file1\_VS_$file2"
        set widgetInfo($address-filerootname1) "$file1"
        set widgetInfo($address-filerootname2) "$file2"
        set widgetInfo($address-mismatch) 0
        grapher_html_write_table  $address "Comparing simulations " start
        grapher_html_write_table  $address "Element $file1 $file2" headers
        
        grapher_loadfile "ds1" [file join $folder "$file1.dat"]
        grapher_loadfile "ds2" [file join $folder "$file2.dat"]
        grapher_fill $address "ds1" "dataset" 0 0 
        grapher_fill $address "ds2" "dataset" 0 0
        grapher_compare $address "ds1" "ds2" "dataset" {0 0 10.0} {0 0 10.0}
        grapher_bboxgraphs $address
    }
    
    grapher_html_write_table  $address "" end
        
    
   $wincan raise "handle"
   $wincan configure -scrollregion [ $wincan bbox all]
   

   
   
   if {$dump} {
     set jobduration [time {
            set result [canvas_makegif $wincan [file join $folder "$widgetInfo($address-filerootname).gif"]]
        }]
        set jobtime [expr { 1.*[lindex $jobduration 0]/ 1000000}]
    debug "Gif created in $jobtime s"
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