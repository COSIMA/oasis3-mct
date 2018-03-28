#  This program is under CECILL_B licence. See footer for details.


# create the frame in wich an exclusive selection of model wil be done
# create the menubutton used for selection
proc modelxor_create { args } {

    set mandatory_arguments {path_father address}

    # Initializes the widget
    initWidget
    
    smartpacker_setup_modelframe $win $address
    
    set widgetInfo($address-to_reset) 0
    
    set widgetInfo($address-status) 0
    set widgetInfo($address-dependencies_status) 1
    
	set widgetInfo($address-status_txt) ""
	ttk::label $win.status -textvariable widgetInfo($address-status_txt) -foreground "red" -justify center -compound left
	# the  menubutton seclector
    ttk::menubutton $win.mb -menu $win.mb.xor -textvariable widgetInfo($address-variable_title) 
    menu $win.mb.xor -tearoff 0 -postcommand [subst { modelxor_mb_refresh $win $address}]
    pack $win.mb
    pack $win.status

    # this frame is where all xor choices will be used
    #switchform_create $win.xor
    ttk::frame $win.xor
    pack $win.xor -side top -expand 1 -fill both
    
    
    
    if  {[dTree_attrExists $XMLtree $full_address_XML "groups"]==1} {
            set widgetInfo($address-groups) 1            
    } else {
            set widgetInfo($address-groups) 0
    }    
    
    
    set widgetInfo($address-full_address_XML_clean) $full_address_XML_clean
    set widgetInfo($address-children) [dTree_getChildren_fast $XMLtree $full_address_XML_clean ]
    
    
    set main_menu_reduced ""
    # find the redondant childs in the menu and gather , if groups are enabled
    if {$widgetInfo($address-groups)} {
        set main_menu ""
        foreach child $widgetInfo($address-children) {
            lappend main_menu [lindex [split $child "_"] 0]
        }
        foreach category  $main_menu {
            set items [llength [lsearch -all $main_menu $category] ]
            if {$items > 1} {
                lappend main_menu_reduced $category
            }
        }
        set main_menu_reduced [lsort -unique $main_menu_reduced]
    }    
    
    set widgetInfo($address-categories) $main_menu_reduced
    
    
    
    # add the groups (if any)
    set widgetInfo($address-catindex-none) 0
    foreach category  $main_menu_reduced {
        menu $win.mb.xor.$category -tearoff 0 
        $win.mb.xor add cascade -menu $win.mb.xor.$category  -label "[string totitle $category]"
        set widgetInfo($address-catindex-$category) 0
        set widgetInfo($address-catchildren-$category) ""
    }
    
    
    set widgetInfo($address-child-conditional) ""
    
    foreach child $widgetInfo($address-children) {
        
        set title [dTree_getAttribute_fast $XMLtree "$full_address_XML_clean $child" "title"]
        set child_nodeType [dTree_getAttribute_fast $XMLtree "$full_address_XML_clean $child" "nodeType"]
        if {[dTree_attrExists $XMLtree "$full_address_XML_clean $child" "existif"] ==1 } {
            lappend widgetInfo($address-child-conditional) $child
            bind . <<visiblityChanged-$address.$child>> +[subst { eval modelxor_updatechoice_from_existifs $win $address}]
        }
        
        
        
        # add the command either at the root or in the groups
        set category [lindex [split $child "_"] 0]
        if {[lsearch $widgetInfo($address-categories) $category] ==-1 } {
            $win.mb.xor add command -label "$title" -command [subst {modelxor_mb_action $win $address $child }] 
            set widgetInfo($address-childindex-$child) $widgetInfo($address-catindex-none)
            set widgetInfo($address-childmenu-$child) $win.mb.xor
            set widgetInfo($address-childcategory-$child) "none"
            lappend widgetInfo($address-catchildren-none) $child
            incr widgetInfo($address-catindex-none) 1
        } else {
            $win.mb.xor.$category add command -label "$title" -command [subst {modelxor_mb_action $win $address $child }]
            set widgetInfo($address-childindex-$child) $widgetInfo($address-catindex-$category)
            set widgetInfo($address-childmenu-$child) $win.mb.xor.$category
            set widgetInfo($address-childcategory-$child) "$category"
            lappend widgetInfo($address-catchildren-$category) $child
            incr widgetInfo($address-catindex-$category) 1
        }
    }
    
    # Finishes the widget
    # append widgetInfo($address-check) [ subst {modelxor_check $win $address}]
    append widgetInfo($address-refreshStatus) [ subst {modelxor_refreshStatus $win $address}]
    
    finishWidget
    
    help_add_desc_docu_to_widget
    
    # initialize the layout by the default 
    modelxor_mb_action $win $address $widgetInfo($address-variable)
    
    
   # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]    
    
    return $win 
}

proc modelxor_mb_refresh {win address} {
    global widgetInfo XMLtree tmpTree
    
    set full_address_XML_clean $widgetInfo($address-full_address_XML_clean)
    # Updates of existifs in the menu
    foreach child $widgetInfo($address-child-conditional) {
        set existifArg [build_existIf_argument "$full_address_XML_clean $child"]
        set existIfArgument [join [lindex $existifArg 0] " "]
        if {[expr $existIfArgument]} {
            $widgetInfo($address-childmenu-$child) entryconfigure $widgetInfo($address-childindex-$child) -state normal
            lappend child_authorized $child
        } else {
            $widgetInfo($address-childmenu-$child) entryconfigure $widgetInfo($address-childindex-$child) -state disabled    
        }        
    }
   
}



proc modelxor_mb_action { win address setchild } {
    global widgetInfo XMLtree tmpTree
    
    
    # update the value
    if { $widgetInfo($address-variable) != $setchild} {        
        set widgetInfo($address-variable) $setchild
        eval $widgetInfo($address-check)
    }
    
}


proc modelxor_updatechoice_from_existifs {win address} {
    global widgetInfo
    
    set initStatus $widgetInfo($address-status)
    set widgetInfo($address-dependencies_status) "1"
    
    
    
          
    set child $widgetInfo($address-variable)
    if { $child in $widgetInfo($address-child-conditional) } {    
        if {$widgetInfo($address.$child-visible) == 0 } {
            warning "XOR $address cannot keep $widgetInfo($address-variable)"
            
            set widgetInfo($address-dependencies_status)  "-1"
            $win.status configure -image icon_flag
            set widgetInfo($address-status_txt) "$widgetInfo($address-variable) is not a possible choice"
        } 
    }
    
    set widgetInfo($address-status) [expr min($widgetInfo($address-status),$widgetInfo($address-dependencies_status)) ]
    
    # commented for the moment. Might be a need.
    #if {$initStatus != $widgetInfo($address-status)} {
        #UpdateValidationStatus $address
    #}
}


proc modelxor_refreshStatus {win address} {
    global widgetInfo XMLtree tmpTree
    
    set full_address_XML_clean $widgetInfo($address-full_address_XML_clean)
    
    set widgetInfo($address-status_txt) ""
    $win.status configure -image ""
    
    
    #  l'enfant actuellement affiche
    set currentchild [lindex [split [winfo children $win.xor] "."] end ]
    
    # l'enfant qu'il faudrait afficher
    set child $widgetInfo($address-variable)
    
    
    
    set refreshxor 0
    # si l'enfant actuellemnt affiche n'est pas le bon
    if {$currentchild != $child } {
        modelxor_killchild $win $address $currentchild
        set refreshxor 1
    }
    # si il faut reinitialiser l'onglet            
    if {$widgetInfo($address-to_reset) == 1} {
        #debug ">>>> reinit $address.$child "
        destroy $win.xor.$child
        set refreshxor 1
        set widgetInfo($address-to_reset) 0
    }
    
    
    # create new memory and gui
    if { $refreshxor == 1} {
        #debug ">>>> creating $address.$child "
        set winchild $win.xor.$child
        ttk::frame $winchild
        pack $winchild -side top -expand 1 -fill both
        set child_nodeType [dTree_tryGetAttribute_fast $XMLtree "$full_address_XML_clean $child" "nodeType" "exception_XOR"]
        
        if {$child_nodeType ==  "exception_XOR"} {
            popup_error "[subst {"The option $child is no longer available in the XOR $full_address_XML_clean. Please remove this item from the XML"}]" strict 
        }
        set newpart [gui_addpart -address $address.$child  -path_father $winchild -class $child_nodeType -style "flat"]    
        CheckFamily  $address.$child
    }
    
    
    set widgetInfo($address-variable_title) [dTree_getAttribute_fast $XMLtree "$widgetInfo($address-full_address_XML_clean) $widgetInfo($address-variable)" "title"]
        
    smartpacker_update_visibility $win $address
    modelxor_updatechoice_from_existifs $win $address
    
   

}

proc modelxor_killchild {win address child} {
    global widgetInfo
    global tmpTree
    if {$child  ==""} {return}
    
    #debug ">>>>  killing $address.$child"
    destroy $win.xor.$child
    # clean widget info
    array unset widgetInfo $address.$child\*
    # destroy tmptree information
    set full_address_XML [split $address.$child "."]
    if {[dTree_nodeExists $tmpTree $full_address_XML]} {
        dTree_rmBranch tmpTree $full_address_XML
    }
    return
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