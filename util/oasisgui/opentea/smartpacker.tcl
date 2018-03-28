#  This program is under CECILL_B licence. See footer for details.


proc smartpacker_getconfig {} {
    global widgetInfo
    set guiWidth [getConfig "config gui appearance width"]
    if {$guiWidth == ""} {
        set guiWidth "700"
    }
    set guiHeight [getConfig "config gui appearance height"]
    if {$guiHeight == ""} {
        set guiHeight "700"
    }
    
    set widgetInfo(guiheight) $guiHeight
    set widgetInfo(guiwidth) $guiWidth
    
    set widgetInfo(guiSmallWidgetWidth)  [expr { int ([expr { 0.37*$widgetInfo(guiwidth)}])}]
    set widgetInfo(guiBigWidgetWidth) [expr { int ([expr { 0.74*$widgetInfo(guiwidth)}])}]
    set widgetInfo(guiEntryRelWidth)  0.5

}


# called at the start of the application to initiate the window

proc smartpacker_initialize_gui { loadedProject } {
    global widgetInfo
    global workingDir
    
    # bind the main window to the resizing action
    
    set widgetInfo(SetViewMode) "large"
    event generate . <<SetView>> 
    
    
    bind . <Configure> {
	set ActualWidth [winfo width .]
	#puts "AW $ActualWidth"
	#puts "VM $widgetInfo(SetViewMode)"
	
	if {$ActualWidth >[expr { (2.2)*$widgetInfo(guiSmallWidgetWidth)}] && $widgetInfo(SetViewMode)=="small"} {
	    set widgetInfo(SetViewMode) "large"
	    event generate . <<SetView>> 
	}
	if {$ActualWidth >[expr { (3.3)*$widgetInfo(guiSmallWidgetWidth)}]  && $widgetInfo(SetViewMode)=="large"} {
	    set widgetInfo(SetViewMode) "huge"
	    event generate . <<SetView>> 
	}
	if {$ActualWidth <[expr { (2.2)*$widgetInfo(guiSmallWidgetWidth)}]  && $widgetInfo(SetViewMode)=="large"} {
	    set widgetInfo(SetViewMode) "small"
	    event generate . <<SetView>> 
	}
	if {$ActualWidth <[expr { (3.3)*$widgetInfo(guiSmallWidgetWidth)}]  && $widgetInfo(SetViewMode)=="huge"} {
	    set widgetInfo(SetViewMode) "large"
	    event generate . <<SetView>> 
	}
	
	    
    }
	
    wm geometry . ${widgetInfo(guiwidth)}x${widgetInfo(guiheight)}
    raise .
    
    if {$loadedProject != "none"} {      
            loadProject $loadedProject
    }
    update    
}

# correction of focus
proc smartpacker_focus_correction {} {
    global focusCorrection
    if {$focusCorrection == 1} {
	bind . <ButtonPress-1> {
	    if {[focus] == ""} {
		focus -force [focus -lastfor .]
	    }
	}
    }
    event generate . <<InitializeGUI>>
}


# smartpacker algorithm
proc smartpacker_regrid {   } {
    global widgetInfo
    #log "regrid"
    set viewmode $widgetInfo(SetViewMode)
    

    if {$widgetInfo(tabfocus) == ""} {
	set $widgetInfo(tabfocus) [lindex $widgetInfo(form_to_update) 0]
    }
    if {$widgetInfo(tabfocus) == ""} {return} 
    set win "$widgetInfo(tabfocus).sf.vport.form"
    
    #incr widgetInfo(nregrid)
    #puts "Regridding... $widgetInfo(nregrid) \n mode $viewmode"
	
    set ColumnSize [expr { 1.1*$widgetInfo(guiSmallWidgetWidth)}]
    set VerticalPad 15
    set HorizontalPad [expr { 0.001*$widgetInfo(guiSmallWidgetWidth)}]
    set VerticalStart 5    
    set HorizontalStart 5

    # get the number of columns
    switch $viewmode {
	"small" {
	    set colmax 1
	}
	"large" {
	    set colmax 2
	}
	"huge" {
	    set colmax 3
	}
	default {
	    set colmax 1
	}
    }
    
    update idletasks
    
    #list of widget to repack
    set widgetlist ""
    foreach wc [winfo children $win] {
	if {[ winfo manager $wc ] != ""} {
	    lappend widgetlist $wc
	}
    }
    
    #  1st loop on widgets to fill info and depack widgets
    set wp(narrow-height) 0
    set wp(narrow-list) ""
    set wp(wide-list) ""
    foreach widget $widgetlist {
	eval $widgetInfo(unpackme-$widget)
	
	pack $widget 
	
	set wp($widget-height) [winfo reqheight $widget]
	if {$wp($widget-height) == 0 }  {set wp($widget-height) [winfo height $widget]}
	set wp($widget-width) [winfo reqwidth $widget]
	if {$wp($widget-width) == 0 }  {set wp($widget-width) [winfo width $widget]}
	
	pack forget $widget
		
		
		
	if {$wp($widget-width)  < $ColumnSize} {
	    incr wp(narrow-height)  $wp($widget-height)
	    lappend wp(narrow-list) $widget
	} else {
	    lappend wp(wide-list) $widget
	}
	
    }
    
	
    set vmax 0
    set hmax 0
    # balance the columns
    # if the middle of the widget is below the maximum average,
    # then the widget must jump one col
    set max [expr {int($wp(narrow-height)/$colmax*1.0)}]
    set position 0
    set mid_position 0
    set jumpwidget 0
    set sparecolumns $colmax 
    
    foreach widget $wp(narrow-list) { 
	set  mid_position [ expr {int($position+ int(0.5*$wp($widget-height)))}]
	if { $mid_position > $max &&  $sparecolumns > 1 } {
	    set position 0
	    set wp($widget-jump) 1
	    incr sparecolumns -1
	} else {
	    set wp($widget-jump) 0
	}
	incr position $wp($widget-height)
    }
  
    
    # loop on narrow widgets
    set wp(x1) $HorizontalStart
    set wp(y1) $VerticalStart
    set wp(narrowtotalheight) 0
    foreach widget $wp(narrow-list) {
	if {$wp($widget-jump)} {
	    set wp(x1) [expr {int($wp(x1) + $HorizontalPad + $ColumnSize)}]
	    set wp(y1) $VerticalStart
	}
	place $widget -x $wp(x1) -y $wp(y1) -width $wp($widget-width) -height $wp($widget-height)		
	set widgetInfo(packme-$widget) [subst {
	    set widgetInfo(fixedview) 1
	    place $widget -x $wp(x1) -y $wp(y1) -width $wp($widget-width) -height $wp($widget-height)		
	    event generate . <<SetView>>
	    set widgetInfo(fixedview) 0
	}]
	set widgetInfo(unpackme-$widget) [subst {
	    place forget $widget
	}]
	
	
	
	incr wp(y1) $wp($widget-height)
	incr wp(y1) $VerticalPad
	if { $wp(narrowtotalheight) < $wp(y1) } { set wp(narrowtotalheight) $wp(y1)}        
    }
    set hmax [expr {int($wp(x1) + $HorizontalPad + $ColumnSize)}]
    
  
    #loop on wide widgets
    set wp(x1) $HorizontalStart
    set wp(y1) $wp(narrowtotalheight)
    foreach widget $wp(wide-list) {
	place $widget -x $wp(x1) -y $wp(y1) -width $wp($widget-width) -height $wp($widget-height)
	set widgetInfo(packme-$widget) [subst {
	    set widgetInfo(fixedview) 1
	    place $widget -x $wp(x1) -y $wp(y1) -width $wp($widget-width) -height $wp($widget-height)
	    event generate . <<SetView>>
	    set widgetInfo(fixedview) 0
	}]
	set widgetInfo(unpackme-$widget) [subst {
	    place forget $widget
	}]
	
	incr wp(y1) $wp($widget-height)
	incr wp(y1) 5
	set widthwide [expr {int($wp($widget-width) +$HorizontalStart+2*$HorizontalPad)}]
	if { $widthwide > $hmax} {set hmax $widthwide}
	
    }
    set vmax $wp(y1)
    
    
    # clean all variables used
    array unset wp
    
    # adjust the form
    $win configure -width $hmax -height $vmax
    set widgetInfo(ReGrid) "no"
    
    scrollform_resize [crop_address $win 2]
    
}


proc smartpacker_update_visibility { win address } {
    global widgetInfo
    if {$widgetInfo($address-existIf)} {
        if {$widgetInfo($address-visible)}  {
	    eval $widgetInfo(packme-$win)
        } else {
	    #debug "unpackme $win"
            eval $widgetInfo(unpackme-$win)
        }
    }
}

# called when a modelframe is initiated, to keep the same layout for each frame
proc smartpacker_setup_modelframe { win address } {
    uplevel 1 {} {
	
	set title [dTree_getAttribute $XMLtree $full_address_XML "title"]
    
	# main labelframe or frame in flat-style
	switch -glob $style {
	    "flat" {
		ttk::frame $win
	    }
	    "group_of_models" {
		ttk::frame $win
		ttk::label $win.title -text $title -style "Model.TLabelframe.Label"
		ttk::separator $win.sep  -orient horizontal
		pack $win.title -side top
		pack $win.sep -side top -fill x
		
	    }
	    
	    "onrequest" {
		set widgetInfo($address-onrequest) 0
		ttk::frame $win
		
		ttk::label $win.title -image icon_plus -compound left -text $title
		
		
		ttk::separator $win.sep  -orient horizontal
		pack $win.title -side top -anchor w
		pack $win.sep -side top -fill x
		ttk::frame $win.snap
		bind $win.title <ButtonPress> [subst {
		    if {\$widgetInfo($address-onrequest) == 0} {
			set widgetInfo($address-onrequest) 1
			pack $win.snap -side top -fill both
			$win.title configure -image icon_minus
			event generate . <<SetView>> 
		    } else {
			set widgetInfo($address-onrequest) 0
			pack forget $win.snap
			$win.title configure -image icon_plus
			event generate . <<SetView>> 
		    }
		}]
	    }
	    "sublevel_*" {
		set level [lindex [split $style "_" ] 1]
		
		if {$level < 1} {set level 1}
		if {$level > 3} {set level 3}
		set pad [expr { $level *20}] 
		ttk::frame $win
		label $win.title -text $title -image "bullet_$level" -compound left -bg [ThemeColor 1.0] -fg [ThemeColor [expr {$level*0.2}]]
		#-style "Model.TLabelframe.Label"
		ttk::separator $win.sep  -orient vertical
		pack $win.sep -side left -fill y -padx "$pad 0" -pady 10
		
		    
		
		pack $win.title -side top -anchor w
		
	    }
	    default {
		ttk::labelframe $win -text $title -style "Model.TLabelframe"
	    }
	}
	
	eval $widgetInfo(packme-$win)
	
	ttk::frame $win.forceps -width $widgetInfo(guiSmallWidgetWidth) -height 0 
	pack $win.forceps -side top 
    }
}

# called when a simple widget is initiated, to keep the same label layout for each simple widget
proc smartpacker_setup_label { win address } {
    uplevel 1 {} {
	set title "[dTree_getAttribute $XMLtree $full_address_XML "title"]:"
	ttk::label $win.lbl -text $title -justify right
    
	$win.lbl configure -wraplength [expr { int(0.5*$widgetInfo(guiSmallWidgetWidth))}]
	
	place $win.lbl -relx 0.5 -rely 0. -anchor ne
    }
}
 
# called when a widget is initiated, to keep the same status layout for each simple widget
proc smartpacker_setup_status { win address } {
    uplevel 1 {} {
	set widgetInfo($address-status) 0
	set widgetInfo($address-status_txt) ""
	ttk::label $win.status -textvariable widgetInfo($address-status_txt) -foreground "red" -justify center -compound left
	$win.status configure -wraplength [expr { 0.6*$widgetInfo(guiSmallWidgetWidth)}]
	place $win.status -relx 0.5 -rely 1 -anchor s
    }
}


proc Start_think { cmd} {
    global abort_cmd
    
    set abort_cmd $cmd
    
    . config -cursor watch
    grab .log
    update
}

proc Stop_think {  mode }  {
    global abort_cmd
    . config -cursor left_ptr
    grab release .log
    raise .
    if {$abort_cmd == ""} {set mode "normal"}
    switch $mode {
	"normal" {
	    
	}
	"abort" {
	    warning "Aborting ..."
	    eval $abort_cmd
	}
	default {
	    error "Wrong use of stop_think"
	}
    }
    update
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