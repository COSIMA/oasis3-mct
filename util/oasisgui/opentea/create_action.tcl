#  This program is under CECILL_B licence. See footer for details.

proc action_create { args } {
    global codesPath defaultSolver solverPath
    set mandatory_arguments { path_father address }
    initWidget
    set widgetInfo($address-title) [dTree_getAttribute $XMLtree $full_address_XML "title"]
    set widgetInfo($address-args) [split [dTree_tryGetAttribute $XMLtree $full_address_XML "script_args" ""] ";"]
    set widgetInfo($address-blocking) [split [dTree_tryGetAttribute $XMLtree $full_address_XML "status_blocking" "yes"] ";"]
    
    set widgetInfo($address-starttime) "0"
    set widgetInfo($address-endtime) "0"
    
    
    #smartpacker_setup_modelframe $win $address
    ttk::frame $win
    eval $widgetInfo(packme-$win)
    
    ttk::frame $win.action
    ttk::frame $win.infos    
    
    
   
    set script [dTree_getAttribute $XMLtree $full_address_XML "script"]
    set widgetInfo($address-scriptAddress) [file join $solverPath scripts $script]
    if {![file exists $widgetInfo($address-scriptAddress)]} {
        error "File not found : \n$widgetInfo($address-scriptAddress)"
    }
    
    
    
    #ttk::label $win.title  -width -10 -text $title 
    ttk::label $win.action.status -width -20 -textvariable widgetInfo($address-status_txt) -justify left -compound left
    ttk::progressbar $win.action.progress  -mode indeterminate 
    $win.action.progress stop
    ttk::button $win.action.launch -text $widgetInfo($address-title) -command [subst {
        log "\n@@@@@@@@ Process Action '$widgetInfo($address-title)' @@@@@@@"
        openPipe $win $address
    }]
    #
    set widgetInfo($address-progressValue) 0    
    
    
    ############################
    # Adding script node 
    emptyNode_create -path_father $win -address $address.scriptNode
    append widgetInfo($address.scriptNode-refresh) [subst { set widgetInfo($address.scriptNode-status) \[ expr (\$widgetInfo($address.scriptNode-variable)+1)/2]}]

    #pack $win -pady 5
    
    #grid $win.action -sticky news -column 0 -row 0
    pack $win.action -side top -fill x -expand 1  
    pack $win.action.launch -side top  -expand 1  
    pack $win.action.progress -side top  -expand 1  
    pack $win.action.status -side top  -expand 1  
    
    pack $win.infos -side top -fill x -expand 1  
    #grid $win.infos -sticky news -column 0 -row 1
    
    #grid columnconfigure $win $win.infos -weight 1
        
    append widgetInfo($address-refresh) [subst { action_refresh $win $address }]
    append widgetInfo($address-refreshStatus) [subst { action_refreshStatus $win $address }]
    
    set widgetInfo($address-onScriptStarts) [subst {
        $win.action.status configure -image icon_void -foreground black
        set widgetInfo($address-cancellation) 0
        if {\$widgetInfo($address-status) == -1} {
            set widgetInfo($address-cancellation) 0
            set widgetInfo($address-status_txt) "Warning, status was initially wrong.."
        } else {
            $win.action.progress start
            set widgetInfo(action-callingAddress) $address
            $win.action.launch configure -text "Escape to Abort"
            #$win.action.launch configure -command "stopScript $win $address"
        }
    }]
    
    set widgetInfo($address-onScriptStops) [subst {
        $win.action.progress configure -mode indeterminate         
        $win.action.progress stop
        $win.action.launch configure -text "$widgetInfo($address-title)"
        $win.action.launch configure -command "openPipe $win $address"
        set widgetInfo($address.scriptNode-status) -1
        set widgetInfo($address.scriptNode-variable) 0
        eval \$widgetInfo($address.scriptNode-check)
        action_refreshStatus $win $address
        Stop_think normal
    }]
    
    set widgetInfo($address-updateProgress) [subst {
         $win.action.progress stop            
         $win.action.progress configure -mode determinate -value \$widgetInfo($address-progressValue)
         
     }]    
    
    set widgetInfo($address-onScriptEnds) [subst {
        $win.action.progress configure -mode indeterminate          
        $win.action.progress stop
        $win.action.launch configure -text "$widgetInfo($address-title)"
        $win.action.launch configure -command "openPipe $win $address"
        set widgetInfo($address.scriptNode-status) 1
        set widgetInfo($address.scriptNode-variable) 1
        eval \$widgetInfo($address.scriptNode-check)
        action_refreshStatus $win $address
        Stop_think normal
        }]
    
    set widgetInfo($address-status) "-2"
    if {[dTree_attrExists $XMLtree $full_address_XML "initstatus"]  == 1 } {
        set widgetInfo($address-status) "1"
    }
    
    action_refreshStatus $win $address
    finishWidget
    return $win.infos
}

proc action_refreshStatus {win address} {
    global widgetInfo
    
    set duration  [printtime $widgetInfo($address-starttime) $widgetInfo($address-endtime) ]                    

  
    switch $widgetInfo($address.scriptNode-status) {
        "-1" {$win.action.status configure -image icon_error -foreground red
            set widgetInfo($address.scriptNode-status) -1
            set widgetInfo($address-progress) 0
            set widgetInfo($address-status_txt) "Execution error : check logs. Took $duration s."
            set widgetInfo($address-variable) 0
        }
        "1" {$win.action.status configure -image icon_ok -foreground green4
            set widgetInfo($address.scriptNode-status) 1            
            set widgetInfo($address-progress) 100
            set widgetInfo($address-status_txt) "Done in $duration s."
            set widgetInfo($address-variable) 1
        }
        "-2" {$win.action.status configure -image icon_void -foreground black
            set widgetInfo($address-progress) 100
            set widgetInfo($address-status_txt) "Waiting for launch"
            set widgetInfo($address-variable) 1
            set widgetInfo($address.scriptNode-status) -2            
        }
        default {$win.action.status configure -image icon_void -foreground black
            set widgetInfo($address-progress) 0
            set widgetInfo($address-status_txt) ""
            set widgetInfo($address-variable) 0
            set widgetInfo($address.scriptNode-status) 0            
        }
    }
    
    smartpacker_update_visibility $win $address
    
    if {$widgetInfo($address-blocking) =="no"} {
        set widgetInfo($address-status) 1
        set widgetInfo($address.scriptNode-status) 1
    }
    
}

proc action_refresh {win address} {
    # take the variable value from DStree to see if the run has already be done
    
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