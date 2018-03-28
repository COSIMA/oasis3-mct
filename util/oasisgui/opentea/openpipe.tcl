#  This program is under CECILL_B licence. See footer for details.

proc openPipe {win address} {
    global widgetInfo codesPath tmpTree metaTree loadedProject workingDir configTree configPath
    
    Start_think "stopScript $win $address"
    
    #$win.action.progress start
    
    # Check that project has been been saved at least once, otherwise, quit
    if {$loadedProject == "none"} {
        log "The project needs to be saved first"
        #tk_messageBox -message "The project first needs to be saved " -type ok -icon warning
        Save_XML_File "new"
    }
    
    if {$loadedProject == "none" || $loadedProject == ""} {
        warning "The project is not savec. Aborting..."
        Stop_think normal
        return
    }    
    
    saveDataset "dataset.xml" $address
    
    
    # Detects type of script
    set script $widgetInfo($address-scriptAddress)
    set ext [file extension $script]
    set pythonInterpreter "python"
    if {[getConfig "config gui python interpreter"] == "user_specific"} {
        set pythonInterpreter [getConfig "config gui python user_specific interpreter"]
        log "python interpreter is now : $pythonInterpreter"
    }
    switch $ext {
        ".py" {
            execScript $win $address $pythonInterpreter
        }
        ".tcl" {
           # set execCommand "wish $script"
            execScript $win $address "tclsh" 
        }
        
        default {
           # set execCommand $script
            debug "not implemented yet"
        }
    }
    
    
#    if { [catch {tk_exec $execCommand} error_id] } {
#        debug $error_id
#        #debug $returnScript
#        set widgetInfo($address-status) 0
#        $win.action.progress stop
#        return
#    } else {
#        #debug $returnScript
#        
#    }
    
    #set widgetInfo($address-status) 1
    
    #$win.action.progress stop
    
    #action_check $win $address
    
    
}

proc execScript {win address interpreter} {
    global widgetInfo codesPath initializedSolver
    # If the stop file already exists delete it !
    # Apparently deprecadted
    #if {[file exists [file join $initializedSolver stop_[file tail [file rootname $widgetInfo($address-scriptAddress)]]]]} {
    #    file delete [file join $initializedSolver stop_[file tail [file rootname $widgetInfo($address-scriptAddress)]]]
    #}
    eval $widgetInfo($address-onScriptStarts)
    
    
    if {$widgetInfo($address-cancellation) } {
        log "cancellation of $address"
        return
    }
    set os $::tcl_platform(os)
    if {$os == "Windows NT"} {
        #set execCommand "cmd \"$interpreter $widgetInfo($address-scriptAddress)\" "
        set execCommand "$interpreter $widgetInfo($address-scriptAddress) $widgetInfo($address-args)"      
    } else {
        set execCommand "$interpreter $widgetInfo($address-scriptAddress) $widgetInfo($address-args)"
    }    
    
    #log "\"$execCommand\" is now running"
    set widgetInfo($address-starttime) [clock milliseconds]
    set widgetInfo($address-actionChan) [open "| $execCommand" "r+"]
    fileevent $widgetInfo($address-actionChan) readable "readPipe $win $address" 
}

proc readPipe {win address} {
    global widgetInfo DStree metaTree
    if {[gets $widgetInfo($address-actionChan) line] >= 0} {
        if {[regexp {^%\d{1,3}%.*$} $line]} {
            set line [split $line "%"]
            set percent [lindex $line 1]
            set message [join [lrange $line 2 end] "%"]
            set widgetInfo($address-progressValue) $percent
            set widgetInfo($address-status_txt) $message
            eval $widgetInfo($address-updateProgress)
        } else {
            set tagid "none"
            if {[string match "Ok*" $line]} { set tagid "ok"}
            if {[string match "Error*" $line]} { set tagid "warning"}
            if {[string match "Warning*" $line]} { set tagid "warning"}
            if {[string match "Note*" $line]} { set tagid "debug"}
            if {[string match "XDRExecute*" $line]} {
                set tagid "xdrexecute"
                set line [string map "XDRExecute >" $line]
            }
            log $line $tagid
        }
        
    } else {
        # Normal end
        success "Script $widgetInfo($address-scriptAddress) is completed."
        if {[catch {close $widgetInfo($address-actionChan) } err] } {
            warning "Error in script $widgetInfo($address-scriptAddress) :\n$err"
            set widgetInfo($address-status) 0   
            set scriptStatus 0
        } elseif {![file exists "out_dataset.xml"]} {
            warning "No information found about this action. \n  Possible Causes \n - finish() statement not reached \n - permission denied on filesystem \n - disk quota exceeded \n\n Graphical application might behave erratically from now on..."
            
            set widgetInfo($address-status) 0   
            set scriptStatus 0            
        } else {
            set widgetInfo($address-status) 1
            loadDataset "out_dataset.xml"
            file delete "out_dataset.xml"    
            set scriptStatus [dTree_getAttribute $metaTree "root meta scriptSuccess" "value"]                  
        }
          
        
        #set dummyTree ""
        #dTree_init dummyTree
        #parseFile "out_dataset.xml" dummyTree "" "DStree"
        
        
        #dTree_copyBranch dummyTree [gui2tree $address] DStree [lrange [gui2tree $address] 0 end-1]
        # We need to reconstruct interface (multiple which depends on an unknown list have to be initialized)
        #initializeNode tree "root"
        #RefreshFamily $address
        #RefreshFamily "root"
        #CheckValidationStatus $address
        #action_check $win $address
        set widgetInfo($address-endtime) [clock milliseconds]
        if {$scriptStatus == "0"} {
            eval $widgetInfo($address-onScriptStops)            
        } else {
            eval $widgetInfo($address-onScriptEnds)
        }
        
        Stop_think normal
        
    }
}

proc stopScript {win address} {
    global codesPath widgetInfo debug
    
    warning "Trying to stop script. Zombies process can still be running. Beware !!!"
#    debug "writing [file join $codesPath stop_[file tail [file rootname $widgetInfo($address-scriptAddress)]]]"
#    set stopFile [open [file join $codesPath "stop_[file tail [file rootname $widgetInfo($address-scriptAddress)]]"] "w"]
#    close $stopFile
    set widgetInfo($address-endtime) [clock milliseconds]
    
    catch {close $widgetInfo($address-actionChan)} err
    
    eval $widgetInfo($address-onScriptStops)
    
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