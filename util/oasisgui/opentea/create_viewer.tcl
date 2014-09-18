#  This program is under CECILL_B licence. See footer for details.




# ENTRY CREATION

proc viewer_create { args } {
    set mandatory_arguments { path_father address }
    
    # Initializes the widget
    initWidget  
    
    # case when the folders are known beforehand
    set folders ""
    if  {[dTree_attrExists $XMLtree  $full_address_XML_clean "folders"]==1} {
        set list_folder_names [split [dTree_getAttribute $XMLtree $full_address_XML_clean "folders"] "#"]
        foreach group $list_folder_names {
            lappend folders [file join {*}[split $group ";"] ]
        }
    }
    
    #inits
    set widgetInfo($address-folders) $folders
    set widgetInfo($address-zoom) "50%"
    set widgetInfo($address-selimg) ""
    set widgetInfo($address-status) 1
    
    
    
    set lightboxcolorbg [ThemeColor 1.0]
    set lightboxcolorfg [ThemeColor 0.3]
    
    set hsize [getcharsize [expr 0.6*$widgetInfo(guiBigWidgetWidth)]]
    
    frame $win -bg white
    eval $widgetInfo(packme-$win)
    
    ###############
    # Canvas part
    ###############
    
    frame $win.img -bg $lightboxcolorbg
    canvas $win.img.c -width [expr 0.7*$widgetInfo(guiBigWidgetWidth)] -height [expr 0.4*$widgetInfo(guiBigWidgetWidth)] -background $lightboxcolorbg  -highlightthickness 1 -highlightbackground $lightboxcolorbg
    pack $win.img.c 
    bind $win.img.c <ButtonPress> [subst {
        $win.img.c  scan mark %x %y
    }]
    bind $win.img.c <B1-Motion> [subst {
        $win.img.c  scan dragto %x %y 1
    }]
    image create photo $address-img
    $address-img blank
    $win.img.c create image [expr 0.35*$widgetInfo(guiBigWidgetWidth)] [expr 0.2*$widgetInfo(guiBigWidgetWidth)] -image $address-img 
    
    ###############
    #list of images part
    ###############
    
    frame $win.l -bg $lightboxcolorbg
    listbox $win.l.lb -listvariable widgetInfo($address-listimg) -yscrollcommand [list $win.l.sb set] -bd 0 -height 6 -width $hsize -selectmode browse -foreground $lightboxcolorfg -background $lightboxcolorbg -selectbackground  $lightboxcolorfg -selectforeground  $lightboxcolorbg

    bind $win.l.lb <<ListboxSelect>> [subst { viewer_curimage $win $address}]
    
    grid $win.l.lb -column 0 -row 0 -columnspan 2 -sticky news -padx {10 0} -pady {10 0}
    scrollbar $win.l.sb -command [list $win.l.lb yview]
    grid $win.l.sb -column 2 -row 0 -sticky news  -padx {0 10} -pady {10 0}
    
    label $win.l.space -text "Image selected:"  -foreground $lightboxcolorfg -background $lightboxcolorbg
    grid $win.l.space -column 0 -row 1 -sticky news
    label $win.l.sel -textvariable widgetInfo($address-selimg) -wraplength [expr 0.6*$widgetInfo(guiBigWidgetWidth)] -foreground $lightboxcolorfg -background $lightboxcolorbg
    grid $win.l.sel -column 1 -row 1 -sticky news 
    
    
    ###############
    # text part
    ###############
    
    #text $win.txt -width $hsize -height 6 -wrap word -background khaki1 -highlightcolor black -highlightbackground [ThemeColor 0.9] -selectbackground skyblue1
    text $win.txt -width $hsize -height 6 -wrap word -foreground $lightboxcolorfg -background $lightboxcolorbg -highlightcolor white -highlightbackground $lightboxcolorbg -selectbackground skyblue1
    #label $win.txt.corner -image icon_corner -compound left -background khaki1 -borderwidth 0
    #place $win.txt.corner -relx 1.0 -rely 1.0 -anchor se
    
    
    ###############
    # buttons part
    ###############
    
    ttk::frame $win.b 
     
    ttk::button $win.b.del -text "Delete image" -command [subst {viewer_delimage $win $address }]
    #pack $win.b.del -side left -fill x -padx 4
    grid $win.b.del -column 0 -row 0 -sticky news -padx 4 -pady 4
    
    ttk::button $win.b.save -text "Save Comment"  -command [subst {viewer_savecomment $win $address }]
    #pack $win.b.save  -side left -fill x  -padx 4
    grid $win.b.save -column 1 -row 0 -sticky news -padx 4 -pady 4

    ttk::button $win.b.reset -text "Update image list" -command [subst {viewer_update $win $address }]
    #pack $win.b.reset  -side left -fill x  -padx 4
    grid $win.b.reset -column 2 -row 0 -sticky news -padx 4 -pady 4
  
  
    ttk::button $win.b.zoomp -image icon_magnifierplus -command [subst {
        viewer_zoom $win $address "+"
    }]
    ttk::button $win.b.zoomm -image icon_magnifierminus -command [subst {
        viewer_zoom $win $address "-"
    }]
    
    grid $win.b.zoomp -column 3 -row 0 -sticky news -padx 4  -pady 4
    grid $win.b.zoomm -column 4 -row 0 -sticky news -padx 4  -pady 4
    
    ##########################################
    # case where folder is not known beforehand
    if {$widgetInfo($address-folders) ==""} {
        ttk::button $win.b.browsebut -image icon_folder  -compound right -text "Set folder" -command  [subst {
            set widgetInfo($address-folders) \[tk_chooseDirectory -title "Choose gif images folder" \]
            viewer_update $win $address
        }]
        grid $win.b.browsebut -column 5 -row 0 -sticky news -padx 4  -pady 4
    }
    ##########################################
    
    set framewidth 0
    # stacking
    pack $win.b -side top -fill x  -pady 0 -ipadx $framewidth
    pack $win.l -side top -fill x -pady "$framewidth 0" -padx $framewidth 
    pack $win.img -side top -fill x -pady 0 -padx $framewidth
    pack $win.txt -side top -fill x -pady "0 $framewidth" -padx $framewidth
    
    finishWidget
    
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
}


# to update the list of images
proc viewer_update {win address} {
    global widgetInfo

    set  widgetInfo($address-listimg) ""
    foreach folder $widgetInfo($address-folders) {
         set widgetInfo($address-listimg) [concat $widgetInfo($address-listimg) [glob -nocomplain [file join $folder *.gif]]]
    }
    if {$widgetInfo($address-listimg) == ""} {
        log "No images to show..."
        viewer_cleanimg $win $address
        return
    }
    
    foreach imgfile $widgetInfo($address-listimg) {
        set rootname [file rootname $imgfile]
        set name [file tail $rootname]
        set txtfile "$rootname.txt"
        if {![file exists $txtfile]} {
            set fout [open $txtfile w]
            puts $fout "$name \n no specific comment"
            close $fout    
        }
        
    }
    
    
}

# to get the current image
proc viewer_curimage {win address} {
    global widgetInfo
    if {$widgetInfo($address-listimg) == ""} {return}
    set index [$win.l.lb curselection]
    set widgetInfo($address-selimg) [lindex $widgetInfo($address-listimg) $index]
                                     
    viewer_setimage $win $address
    
    return 
}


proc viewer_zoom {win address zoom} {
    global widgetInfo
    
    set list_zoom ""
    lappend list_zoom "25%"
    lappend list_zoom "33%"
    lappend list_zoom "50%"
    lappend list_zoom "100%"
    
    set max [llength $list_zoom]
    set pos [lsearch $list_zoom  $widgetInfo($address-zoom)]
    
    
    switch $zoom {
        "+" {
            incr pos
            if {$pos == $max } {return}
        }
        "-" {
            incr pos -1
            if {$pos == -1 } {return}
        }
    }
    set widgetInfo($address-zoom) [lindex $list_zoom $pos]
    viewer_setimage $win $address
    
}
# to save a comment done on an image
proc viewer_savecomment {win address} {
    global widgetInfo
    if {$widgetInfo($address-selimg) == ""} {
        log "no image selected"
        return
    }
    set rootname [file rootname $widgetInfo($address-selimg)]
    set txtfile "$rootname.txt"
    set fout [open $txtfile w]
    set txt [string trim [$win.txt get 0.0 end]]
    puts $fout $txt
    close $fout
   
}

# to kill an image and its comment
proc viewer_delimage {win address} {
    global widgetInfo
     if {$widgetInfo($address-selimg) == ""} {
        log "No image to delete"
        return
    }
    log "Deleting file $widgetInfo($address-selimg)."
    
    # keep the line of the last selection    
    set index [$win.l.lb curselection]

    #delete files
    set rootname [file rootname $widgetInfo($address-selimg)]
    file delete -force $widgetInfo($address-selimg)
    file delete -force "$rootname.txt"
    
    #update list
    viewer_update $win $address

    # exception when the last element is deleted
    if {$index == [llength $widgetInfo($address-listimg)]} {
        incr index -1
    }
    # switch to next image if any
    $win.l.lb selection clear 0 end
    $win.l.lb selection set $index 
    viewer_curimage $win $address
}

#to clean the lightbox from an image
proc viewer_cleanimg {win address} {
    global widgetInfo
    $address-img blank
    $win.txt delete 0.0 end
    set widgetInfo($address-selimg) ""
}


# to show the curent image
proc viewer_setimage {win address} {
    global widgetInfo
    if {$widgetInfo($address-selimg) == ""} {
        log "No image to show"
        viewer_cleanimg $win $address
        return
    }
    
    # get caption
    set rootname [file rootname $widgetInfo($address-selimg)]
    set txtfile "$rootname.txt"
    set fin [open $txtfile r]
    set caption [read $fin]
    close $fin
    
    # show caption in text
    $win.txt delete 0.0 end
    $win.txt  insert end "$caption" 
    
    # show image
    $address-img blank
    image create photo tmp-img -file $widgetInfo($address-selimg)
    switch $widgetInfo($address-zoom) {
        "25%" {
            $address-img copy tmp-img -subsample 4 4        
        }
        "33%" {
            $address-img copy tmp-img -subsample 3 3        
        }
        "50%" {
            $address-img copy tmp-img -subsample 2 2        
        }
        "100%" {
            $address-img copy tmp-img -subsample 1 1        
        }
    }
    
    # resize canvas after image (not used for the moment)
    #$win.img.c configure -width [image width $address-img] -height [image height $address-img]
    #$win.img.c configure -scrollregion [ $win.img.c bbox all]
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