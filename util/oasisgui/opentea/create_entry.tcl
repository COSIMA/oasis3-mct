#  This program is under CECILL_B licence. See footer for details.




# ENTRY CREATION

proc entry_create { args } {
    set mandatory_arguments { path_father address }
    
    # Initializes the widget
    initWidget  
    
    ttk::frame $win -height 40p
    
    eval $widgetInfo(packme-$win)

    smartpacker_setup_label $win $address
    smartpacker_setup_status $win $address
    
    ttk::entry $win.entry -textvariable  widgetInfo($address-variable) 
    bind $win.entry <KeyRelease>  [subst {eval \$widgetInfo($address-check)}]
    
    place $win.entry -relx 0.5 -rely 0. -relwidth $widgetInfo(guiEntryRelWidth)  -anchor nw
    
    
    #add the check/refresh procedure to the bindings of the variable
   
    append widgetInfo($address-refreshStatus) [ subst { entry_refreshStatus $win $address}]
    
    finishWidget
    
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
}


proc entry_refreshStatus {win address} {
    global widgetInfo XMLtree
    
    smartpacker_update_visibility $win $address

    
    set full_address_XML [split $address "."]
    set type [dTree_getAttribute $XMLtree $full_address_XML "type"]
    
    
    switch -glob $type {
        "*double*" {
            set widgetInfo($address-variable) [string map {"," "."} $widgetInfo($address-variable)]
        }
        "*ascii*" -
        "*string*" {
            set widgetInfo($address-variable) [stringclean $widgetInfo($address-variable)]
        }
    }
    
    
    set test_entry [test_vartype $widgetInfo($address-variable) $type ]
    if { $test_entry == 1 } {
        set widgetInfo($address-status_txt) ""
        set widgetInfo($address-status) 1
    } else {
        set widgetInfo($address-status_txt) $test_entry
        set widgetInfo($address-status) -1
    }
    
    
    $win.status configure -image ""
    
    if { $widgetInfo($address-variable) == ""} {
        set widgetInfo($address-status) -1
        set widgetInfo($address-status_txt) "(...)"
        $win.status configure -image icon_question
    }
     
    if { $widgetInfo($address-status) == -1 && $widgetInfo($address-status_txt) != "(...)" } { 
       $win.status configure -image icon_flag
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