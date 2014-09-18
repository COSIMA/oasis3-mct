#  This program is under CECILL_B licence. See footer for details.



proc browser_create { args } {
    set mandatory_arguments { path_father address }
    initWidget
    global workingDir
    
    ttk::frame $win -height 40p
    eval $widgetInfo(packme-$win)
    
    #set title "[dTree_getAttribute $XMLtree $full_address_XML "title"]"
    set widgetInfo($address-browsertype) "[dTree_getAttribute $XMLtree $full_address_XML "type"]" 
    set widgetInfo($address-filter) [split [dTree_tryGetAttribute $XMLtree $full_address_XML "filter" "none" ] ";"] 
    
    
    
    smartpacker_setup_label $win $address
    smartpacker_setup_status $win $address

    
    ttk::entry $win.entry  -textvariable widgetInfo($address-variable) 
    #pack  $win.entry -side left
    
    bind $win.entry <FocusOut> [subst {browser_entry_cmd $address}]
    bind $win.entry <Return> [subst {browser_entry_cmd $address}]
    
    #bind $win.entry <FocusOut> [subst {$win.entry xview moveto 1.}]
    
    ttk::button $win.butt -image icon_folder  -compound right -command  [subst {
        browser_bsr_cmd $win $address "$title" 
    }]
    
    
    #pack $win.butt -side left
    place $win.entry -relx 0.5 -rely 0. -anchor nw   -relwidth [expr {0.8*$widgetInfo(guiEntryRelWidth)}]
    place $win.butt -relx [expr {0.5 + 0.8*$widgetInfo(guiEntryRelWidth)}] -rely 0. -relwidth [expr {0.2*$widgetInfo(guiEntryRelWidth)}] -anchor nw  
    
    append widgetInfo($address-refreshStatus) [ subst { browser_refreshStatus $win $address}]
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]    
    finishWidget
    
    
    
    
    
    
    # setup browser starts
    set initialdircommand ""
    #set browsemode [getConfig "config gui paths"]
    #switch $browsemode {
    #    "constant" {
    #        set constdir [getConfig "config gui paths constant workingDir"]
    #        set initialdircommand " -initialdir $constdir"
    #    }
    #    "auto" {
    #        set initialdircommand " -initialdir $workingDir"
    #    }
    #    "last" {
    #        set initialdircommand "" 
    #    }
    #    
    #}
    
    # set up filters
    set filtercommand " "
    if {$widgetInfo($address-browsertype) in {"file" "multiple_files"} } {
        if {$widgetInfo($address-filter) !="none"} {
            set filterlist ""
            foreach suffix $widgetInfo($address-filter) {
                lappend filterlist "$suffix .$suffix"
            }
            set filtercommand " -filetypes \{$filterlist\}"
            
        } 
    }
    
    if {$widgetInfo($address-browsertype) == "h5_asciiBound"} {
        set filtercommand " -filetypes \{\{ \"mesh *.mesh.h5 + *.asciiBound\" \"*.h5\"\}\}"
       
    }    
    
    
    # build callback
    switch $widgetInfo($address-browsertype)  {
        "file" {
            set widgetInfo($address-callback) [subst { tk_getOpenFile -title \"$title\" $initialdircommand $filtercommand}]
        }
        "multiple_files" {
            set widgetInfo($address-callback) [subst { tk_getOpenFile -title \"$title\" $initialdircommand $filtercommand -multiple 1}]
        }
        "h5_asciiBound" {
            set widgetInfo($address-callback) [subst {tk_getOpenFile -title \"$title\" $initialdircommand $filtercommand}]
        }
        "folder" {
            set widgetInfo($address-callback) [subst {tk_chooseDirectory -title \"$title\" $initialdircommand}]
        }
    }
    
    
    
    
}


# Command associated to entry
proc browser_entry_cmd {address} {
    global widgetInfo 
    eval $widgetInfo($address-check)
}

# Command associated to browser
proc browser_bsr_cmd { win address title} {
    global widgetInfo  
    
    
    set last_value $widgetInfo($address-variable)
    set widgetInfo($address-variable) [eval $widgetInfo($address-callback)]
    
    if {$widgetInfo($address-variable) == ""} {
        set widgetInfo($address-variable) $last_value
    }
    
    if {[llength $widgetInfo($address-variable) ] > 1} {
        set widgetInfo($address-variable) [string map {" " ";"} $widgetInfo($address-variable)]
    }
    
    
    eval $widgetInfo($address-check) 
    
}




proc browser_refreshStatus {win address} {
    global widgetInfo

    if {$widgetInfo($address-variable)!=""} {
        #set widgetInfo($address-variable) [ pathTo $widgetInfo($address-variable) [pwd]]
    }    
    if {$widgetInfo($address-browsertype) == "multiple_files"} {
        set filename [lindex [split $widgetInfo($address-variable) ";"] 0]      
    } else {
        set filename $widgetInfo($address-variable)
    }
    
    
    
    set widgetInfo($address-status) 1
    set widgetInfo($address-status_txt) ""
    
     
    
    
    if { [file exists $filename] == 0} {
        set widgetInfo($address-status) -1
        set widgetInfo($address-status_txt) "File not found"
    }
    
    if { $widgetInfo($address-browsertype) == "h5_asciiBound" && [file exists $filename]} {
        set asciiboundfile "[string range  $filename 0 end-8].asciiBound"
        if {[file exists $asciiboundfile] == 0} {
            set widgetInfo($address-status) -1
            set widgetInfo($address-status_txt) "AsciiBound not found"
            log "Error while loading mesh file \n $filename.\n Cannot find asciiBound file: \n $asciiboundfile \n"
        }
    }

   
    $win.status configure -image ""
    
    if { $filename == ""} {
        set widgetInfo($address-status) -1
        set widgetInfo($address-status_txt) "(...)"
        $win.status configure -image icon_question
    }
    
    if { $widgetInfo($address-status) == -1 &&  $widgetInfo($address-status_txt) != "(...)" } { 
        $win.status configure -image icon_flag
    } 
    
    smartpacker_update_visibility $win $address
    update idletasks
    $win.entry xview moveto 1.
}



# http://wiki.tcl.tk/15925
# get relative path to target file from current file
 # arguments are file names, not directory names (not checked)
# CODE CHANGED : while {[string equal [lindex $cc 0] [lindex $tt 0]] && [llength $cc] > 1} {
 
 proc pathTo {target current} {
     set cc [file split [file normalize $current]]
     set tt [file split [file normalize $target]]
     if {![string equal [lindex $cc 0] [lindex $tt 0]]} {
         # not on *n*x then
         return -code error "$target not on same volume as $current"
     }
     while {[string equal [lindex $cc 0] [lindex $tt 0]] && [llength $cc] >= 1} {
         # discard matching components from the front (but don't
         # do the last component in case the two files are the same)
         set cc [lreplace $cc 0 0]
         set tt [lreplace $tt 0 0]
     }
     set prefix ""
     if {[llength $cc] == 1} {
         # just the file name, so target is lower down (or in same place)
         set prefix "."
     }
     # step up the tree (start from 1 to avoid counting file itself
     for {set i 1} {$i <= [llength $cc]} {incr i} {
         append prefix " .."
     }
     # stick it all together (the eval is to flatten the target list)
     return [eval file join $prefix $tt]
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