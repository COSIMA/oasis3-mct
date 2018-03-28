#  This program is under CECILL_B licence. See footer for details.

# create a choice dialog

proc choice_create { args } {

    set mandatory_arguments { path_father address }
    initWidget

      
    ttk::frame $win 
    eval $widgetInfo(packme-$win)
    
    smartpacker_setup_label $win $address
    smartpacker_setup_status $win $address        
    set widgetInfo($address-status) 1
   
    set widgetInfo($address-mode) "standard"
    set widgetInfo($address-valuelist) ""     
    set widgetInfo($address-titlelist) ""    
   
   
    
    if {[dTree_attrExists $XMLtree $full_address_XML_clean "source"]} {
        set source "root [dTree_getAttribute $XMLtree $full_address_XML_clean "source"]"
        set widgetInfo($address-mode) "source"
        set children [dTree_getChildren_fast $XMLtree $source]
        if {[llength $children] > 3} {
            set widgetInfo($address-choicetype) "combobox"
        } else {
            set widgetInfo($address-choicetype) "radiobox"
        }
    } elseif { [dTree_attrExists $XMLtree $full_address_XML_clean "require"]} {
        set widgetInfo($address-mode) "require"
        
        set widgetInfo($address-valuelist) $widgetInfo($address-requiredValue)        
        set widgetInfo($address-titlelist) $widgetInfo($address-requiredValue)
        set children $widgetInfo($address-valuelist)

        set widgetInfo($address-choicetype) "combobox"
        set widgetInfo($address-variable) [lindex $widgetInfo($address-valuelist) 0]
        
    } else {
        set children [dTree_getChildren_fast $XMLtree $full_address_XML_clean  ]
        if {[llength $children] > 3} {
            set widgetInfo($address-choicetype) "combobox"
        } else {
            set widgetInfo($address-choicetype) "radiobox"
        }
    }
    
    
    # initialize the widget
    switch $widgetInfo($address-choicetype) {
        "combobox" {
            set widgetInfo($address-height) 40   
            ttk::combobox $win.cbbox -textvariable widgetInfo($address-choice) -width 14 -state readonly
            place $win.cbbox -relx 0.5 -rely 0.0 -anchor nw  -relwidth $widgetInfo(guiEntryRelWidth)
            set widgetInfo($address-status) 1
            append widgetInfo($address-check) [subst {choice_check $win $address}] 
            append widgetInfo($address-refresh) [subst {choice_refresh $win $address}]
            append widgetInfo($address-refreshStatus) [ subst { choice_refreshStatus $win $address}]
            bind  $win.cbbox <<ComboboxSelected>> [subst {eval \$widgetInfo($address-check)}]
            $win configure -height $widgetInfo($address-height)
            
        }
        "radiobox" {
            ttk::frame $win.butts        
            set widgetInfo($address-height) 20   
            set widgetInfo($address-choiceshift) 0 
            place $win.butts -relx 0.5 -rely 0.0 -anchor nw  -relheight 1 -relwidth 0.5
            set widgetInfo($address-status) 1
            append widgetInfo($address-refreshStatus) [ subst { choice_refreshStatus $win $address}]    
        } 
    }
   
   
    # in the case of a source, the create_gui will not loop acroos the children by itself
    # this loop s then called inside the creation of the widget
    if {$widgetInfo($address-mode) != "require"} {     
        foreach child $children {
            choice_add -path_father $win -address "$address.$child"
        }
        
    }
    
    finishWidget

    choice_refresh $win $address
    
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    return $win
}

# to add a new radiobutton to radiobox
proc choice_add { args } {
    global widgetInfo XMLtree
    set mandatory_arguments {path_father address}
    
    # Get the widget informations
    getinfoFatherWidget 
    
    incr widgetInfo($address_father-nboptions) 1
    
    
    if {$widgetInfo($address_father-mode) != "standard"} {
        set value [lindex $full_address_XML end]
        set full_address_XML [lrange $full_address_XML 0 end-1]
        set full_address_XML_clean [lrange $full_address_XML_clean 0 end-1]
        
        set title $value
        set name "child_$name"
    } else {
        set value [dTree_getAttribute_fast $XMLtree $full_address_XML_clean "value"]
        set title [string totitle [ join [ split $value "_" ] " " ] ]
        if {[dTree_attrExists $XMLtree $full_address_XML_clean  "title"]} {
            set title [dTree_getAttribute_fast $XMLtree $full_address_XML_clean  "title"]
        }        
    }    
    
    lappend widgetInfo($address_father-valuelist) $value
    lappend widgetInfo($address_father-titlelist) $title  
    switch $widgetInfo($address_father-choicetype) {
        "radiobox" {
            ttk::radiobutton $win_father.butts.$name -text $title -variable widgetInfo($address_father-variable)  -value $value -command [subst {eval \$widgetInfo($address_father-check)}]
            place $win_father.butts.$name -x 0 -y $widgetInfo($address_father-choiceshift)  -relwidth 1 -anchor nw            
            incr widgetInfo($address_father-choiceshift) 20
            incr widgetInfo($address_father-height) 20
            $path_father configure -height $widgetInfo($address_father-height)
        }
        "combobox" {
            $win_father.cbbox configure -values $widgetInfo($address_father-titlelist) 
        }        
    }
    
}

proc choice_check {win address} {
    global widgetInfo
    
    # if combobox used , sync the choice of the combobox and the variable    
    if { $widgetInfo($address-choicetype) == "combobox"} {
        set widgetInfo($address-variable) [lindex $widgetInfo($address-valuelist) [$win.cbbox current]]
        $win.cbbox selection clear
    }
    
}

proc choice_refresh {win address} {
    global widgetInfo
    if { $widgetInfo($address-mode)  == "require" } {   
        set widgetInfo($address-valuelist) $widgetInfo($address-requiredValue)        
        set widgetInfo($address-titlelist) $widgetInfo($address-requiredValue)
        
        
        if {$widgetInfo($address-requiredValue) == ""} {
            set widgetInfo($address-status_txt) "No choices available now"
            set widgetInfo($address-status) -1
        } else {
            $win.cbbox configure -values $widgetInfo($address-titlelist) 
            set widgetInfo($address-status_txt) ""
            set widgetInfo($address-status) 1
            if {[lsearch $widgetInfo($address-titlelist) $widgetInfo($address-variable)] == -1 } {
                #warning "$widgetInfo($address-variable) is not in the list ( $widgetInfo($address-titlelist) )\n automatic switch to [lindex $widgetInfo($address-valuelist) 0] "
                set widgetInfo($address-variable) [lindex $widgetInfo($address-valuelist) 0]
            }
        }
    }
    
    if { $widgetInfo($address-choicetype) == "combobox"} {
        $win.cbbox configure -values $widgetInfo($address-titlelist)
        set widgetInfo($address-choice) [lindex $widgetInfo($address-titlelist) [lsearch -exact $widgetInfo($address-valuelist) $widgetInfo($address-variable)]]
    }
    
}


proc choice_refreshStatus {win address} {
    global widgetInfo
        
    if { $widgetInfo($address-status) == -1 } { 
        $win.status configure -image icon_flag
    } else {
        $win.status configure -image ""
    }    
    
    smartpacker_update_visibility $win $address
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