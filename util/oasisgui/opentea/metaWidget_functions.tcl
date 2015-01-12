#  This program is under CECILL_B licence. See footer for details.


        
################
proc read_arguments { } {
    uplevel 1 {} {
        global XMLtree
        global DStree
       
        # loop on keywords provided
        foreach { keyword argument } $args {
            # test keywords
            if {![string match -* $keyword ]} {
                tk_messageBox -icon error -message "Error : keyword must be like -*"
                return
            }
    
            set varname [ string trim $keyword "-"]
            set $varname $argument
        }
        
        # test if mandatory arguments are set
        foreach argument $mandatory_arguments  {
            if {![info exists $argument]} {
                tk_messageBox -icon error -message  "Cannot create widget : $argument is missing"
                return
            }
        }
        
        # Process address from arguments to get nodes and attribute
        set full_address_XML [split $address "."]
	set full_address_XML_clean [ dTree_cleanKey $full_address_XML ]
	
        set name [lindex $full_address_XML end ]
        set nodes [lrange $full_address_XML 0 end-1 ]
        
        set nodeType ""
        catch {set nodeType [dTree_getAttribute_fast $XMLtree "$full_address_XML" "nodeType"]}
        
    }
}



################
# This procedure must be called by all metaWidgets to get the correct behavior in the IHM
#   before anything else
#                                       no exceptions!
################
proc initWidget { } {
    
    uplevel 1 { } {
        # Get access to the central data tree
        global GuiInfo
        global widgetInfo loading
        global DStree tmpTree
        global initializedSolver
        read_arguments
        
        ################
        # widgetInfo Initializations
        ################
        
        # set the status on OK by default
        set widgetInfo($address-status) 0
       
	
        # initialize the check/refrech/refreshStatus methods
        set widgetInfo($address-check)   ""
        set widgetInfo($address-refresh) ""
        set widgetInfo($address-require) ""
        set widgetInfo($address-refreshStatus) ""
        set widgetInfo($address-variable) ""
	set widgetInfo($address-visible) 1
        set widgetInfo($address-editable) 1
        
        set widgetInfo($address-visibility) [dTree_tryGetAttribute $XMLtree "$full_address_XML" "visibility"  "shown"]
                
        # keep track of the GUI root of the widget
        set widgetInfo($address-win) $path_father.$name
        set win $widgetInfo($address-win)
     
     
	# initialize the documentation preferences
	set docu_prefered_side "bottom"
	set docu_prefered_place $win
     
        set widgetInfo($address-name) $name
        
	set widgetInfo(packme-$win) [subst {
	    set widgetInfo(fixedview) 1
	    pack $win  -expand 1 -fill x
	    event generate . <<SetView>>
	    set widgetInfo(fixedview) 0
	}]

        # unpackme method
        set widgetInfo(unpackme-$win) [subst {
            pack forget $win
        }]
        
        
        
        
        
        
        # inherit attributes from parents
        foreach parent [lrange [addrGetAncestors $address] 0 end-1] {
            if {$widgetInfo($parent-editable) == 0} {
                set widgetInfo($address-editable) 0
                #log "> Herits : $address can't be edited"
            }
        } 
        
        ## basic/expert
        #if {[dTree_attrExists $XMLtree "$full_address_XML" "level"] == 1} {
        #    if {[getConfig "config id user expert"] != "1" && [dTree_getAttribute $XMLtree "$full_address_XML" "level"]=="expert"} {
        #        set widgetInfo($address-editable) 0
        #        #log "> $address can't be edited"
        #    }
        #}
        
        # dependsOn
        if  {[dTree_attrExists $XMLtree $full_address_XML_clean "dependson"]==1} {
            set rawDependsOn [dTree_getAttribute_fast $XMLtree $full_address_XML_clean "dependson"]
            set rawDependsOn [split $rawDependsOn ";"]
            set listDependsOn ""            
            foreach node $rawDependsOn {
                set possibleAddresses [dTree_searchNode $XMLtree $node]
                if {[llength $possibleAddresses] != 1} {
                    set newNode "$initializedSolver $node"
                    set possibleAddresses [dTree_searchNode $XMLtree $newNode]
                    if {[llength $possibleAddresses] != 1} {
                        error "Keywords $newNode have not given relevant results\n Results :\n $possibleAddresses"
                    }
                }
                                    
                set addressDependsOn [lindex $possibleAddresses 0]
                
                bind . <<statusChanged-[tree2gui $addressDependsOn]>> +[subst {
                    set widgetInfo($address-status) 0
                    eval \$widgetInfo($address-refreshStatus)
                    catch { 
                        # In case of an action or tab widget
                         set widgetInfo($address.scriptNode-status) 0
                         eval \$widgetInfo($address.scriptNode-refreshStatus)                        
                    }
                    updateParent [join $address "." ]                     
                }]                
            }
            
        }        
        
        
        # Require
        set widgetInfo($address-requiredValue) ""
        
        if  {[dTree_attrExists $XMLtree  $full_address_XML_clean "require"]==1} {
            set require [dTree_getAttribute_fast $XMLtree $full_address_XML_clean "require"]  
            set possibleAddresses [dTree_searchNode $XMLtree $require]
            if {[llength $possibleAddresses] != 1} {
                set newNode "$initializedSolver $require"
                set possibleAddresses [dTree_searchNode $XMLtree $newNode]
                if {[llength $possibleAddresses] != 1} {
                    error "Keywords $newNode have not given relevant results\n Results :\n $possibleAddresses"
                }
            }         
            set widgetInfo($address-require)  [lindex $possibleAddresses 0]
             
	    switch [dTree_getAttribute_fast $XMLtree $widgetInfo($address-require) "nodeType"] {
                 multiple {
		    bind . <<treeChanged-[tree2gui $widgetInfo($address-require)]>> +[subst {
			widget_require_multiple $win $address
	    	    }]
		 }
		 default {
		    bind . <<treeChanged-[tree2gui $widgetInfo($address-require)]>> +[subst {
			widget_require_default $win $address 
		    }]
		 }
            }
	    # for initialisation of the require
            event generate . <<treeChanged-[tree2gui $widgetInfo($address-require)]>>
        }
        
	
	
	
        # ExistIf
        set widgetInfo($address-existIf) 0
        if {[dTree_attrExists $XMLtree $full_address_XML_clean "existif"] ==1 } {
            set widgetInfo($address-existIf) 1
	    set existifArg [build_existIf_argument $full_address_XML_clean]
            set widgetInfo($address-existIfArgument) [join [lindex $existifArg 0] " "]
            set  widgetInfo($address-valuesToListen) [lindex $existifArg 1] 
            
            # Bind on all the values :
            foreach addressToListen $widgetInfo($address-valuesToListen) {
                set addr [tree2gui $addressToListen]
		    bind . <<tmpTreeChanged-$addr>> +[subst {
			widget_InquireVisibility $win $address
                }]
		# for initialization of the existif
		event generate . <<tmpTreeChanged-$addr>>
            }
	}

        
        
        
        ################
        # Creation of the nodes in the different trees
        ################
        
        # Actual creation of the nodes
	
	if {[dTree_nodeExists $tmpTree "$nodes $name"]} {
	    set default_value  [dTree_getAttribute $tmpTree "$nodes $name" "value" ]
	    log "MWF : $name already exists ($default_value)"
	    
        } {
	    dTree_addNode tmpTree $nodes $name    
	    # Filling with default value or blank if no default specified
	    set default_value [dTree_tryGetAttribute $XMLtree $full_address_XML_clean "default" ""]
        }
        
	dTree_setAttribute tmpTree "$full_address_XML" "value" $default_value
        set widgetInfo($address-variable) $default_value
        set widgetInfo($address-default) $default_value
        
        # set validation behaviour to "onProcess" by default
        # can be set to "automatic" for refresh-only widgets
        switch $nodeType {
            "info" -
            "action" -
            "multiple" {
                set  widgetInfo($address-validateMode) "automatic"
            }
            default {
                set  widgetInfo($address-validateMode) "onProcess"
            }
        }
    }
}

proc finishWidget {} {
    uplevel 1 { } {
        # keep basic variables at hand
        #set win $widgetInfo($address-win) 
        #puts "finish : $win"
        if {[winfo exists $win] && $widgetInfo($address-editable)==0} {
            $win state disabled
            foreach w [winfo children $win] {
                $w state disabled
            }
        }
        #####################
        # Widget to tree
        #####################
        # Each time a widget changes (when user change a value in an entry for instance),
        # The method widgetInfo($address-check) is called. It is componed by two parts :
        #   The first part is the actual check by the widget itself, an entry is checking the type of the value for instance
        #   The second part is not widget-specific. It adds the "widget2tree" behaviour and perform a refreshStatus, this is the code below
        
	
	set widgetInfo($address-check) [subst {
	    $widgetInfo($address-check)
	    tail_Check $win $address
	}]
        
        
        #####################
        # tree to widget
        #####################
        # Sometimes, a widget needs to update its value from the tree,
        # This is done by the method widgetInfo($address-refresh) which code is below
        
	
	# CRITICAL AD Dec 2014
	# CE EXIST IF etait present avant le refactoring 
	# JE ME DEMANDE PKOI IL EST NECESSAIRE
	# pas de raison a priori
	# COMMENTE POUR L'INSTANT
	
	# DE MEME LE EVAL DU CHECK EST MAINTENANT STATIQUE
	
	set widgetInfo($address-refresh) [subst {
	    head_Refresh $win $address
	    $widgetInfo($address-refresh)
	    if {$widgetInfo($address-existIf) == 1} {
		widget_InquireVisibility $win $address
	    }
	    $widgetInfo($address-check)
	}]
	
	
	
	
	
        
        
        #####################
        # refresh status
        #####################
        # Normally, each widget should have a method called widgetInfo($address-refreshStatus) which displays the status
        # of the node to the user.
        # As this method is widget-dependant, there is no code below
        # This method is called by manageStatus
        set adding [subst {
            event generate . <<statusChanged-$address>>
        }]
        set widgetInfo($address-refreshStatus) "$widgetInfo($address-refreshStatus) $adding"
        
	
	
	
	# necessary to rescale the multiple packing
        set widgetInfo(fixedview) 1
	event generate . <<SetView>>
        set widgetInfo(fixedview) 0
	
	# check visibility at creation
	#eval $widgetInfo($address-refresh)
	
	
    }
    
    
}

################
# updateParent
################
# This recursive procedure is called to update the status of each node, starting from a node
#
# An "end-widget" can have three status, valid (1), not valid (-1) or empty/unknown (0) before any action of the user
# A container can have three status as well : false (-1) if one of its child is false, not validated (0) if all the children
# are valid but the DStree is not up to date and valid (1) if the DStree is
# General behaviour :
# - If a widget's status changes and the parent was valid, parent has to turn its status into "not validated" 
#
# The procedure is called to apply a changing status

proc updateParent {node} {
    global widgetInfo
    if {$node == ""} {return}
    set status $widgetInfo($node-status)
    if {$status != 0 } {
        event generate . <<statusModified-$node>>    
    }
        set widgetInfo($node-status) 0
        eval $widgetInfo($node-refreshStatus)
    updateParent [join [lrange [split $node .] 0 end-1] . ]

}

################
# CheckValidationStatus
################
# This recursive procedure is called to get the status of a node considering all the branch starting from the node
# By calling this procedure, a node cannot be "not validated" anymore, its status is set to false if only one element from its branch is false
# Otherwise, it is set to valid

proc CheckValidationStatus {node  {rank 0}} {
       
    global widgetInfo tmpTree DStree
    set childrenStatus ""
    set children [dTree_getUsedChildren $tmpTree [gui2tree $node]]
    
    
    
    
    # case of no children
    if {[llength $children] == 0} {
        set status $widgetInfo($node-status)
    # case of children
    } else {
	foreach child $children {
	    lappend childrenStatus [CheckValidationStatus "$node.$child" [expr $rank+1]]
	}
	set status [expr min([join $childrenStatus ,])]
	
	# When loading, some nodes, which weren't processed have a status equal to 1 at this point,
	#so if they don't exist in the DStree their status is turned to 0. However, items in the DStree
	# and in the tmpTree haven't the same name, so we check that there is no item_* in the address
	if {$status==1 && ![dTree_attrExists $DStree [gui2tree $node] value] && [lsearch [gui2tree $node] "item_*"] == -1 } {
	    set status 0
	}

	set widgetInfo($node-status) $status
	eval $widgetInfo($node-refreshStatus)
	set status $widgetInfo($node-status)

    }
    
    
    
    # in all cases , to update potential requires and existifs...
    if {$status == 1} {
	event generate . <<treeChanged-$node>>
    }
    return $status

}

################
# UpdateValidationStatus
################
# This procedure is called to set the status of the ancestors after a validation
# It acts like the CheckValidationStatus procedure but upward, for one leved (not recursive by itself)
proc UpdateValidationStatus {node} {
    global widgetInfo tmpTree
    foreach address [addrGetAncestors $node] {
        set childrenStatus ""
        set children [dTree_getChildren_fast $tmpTree [gui2tree $address]]
        #if {[llength $children] == 0} {
        #    #Here, it is a security but it should never happen as this procedure is called by one of the children
        #    #tk_messageBox -title "Spacio-temporal problem" -message "It seems that there is a problem that should have never happened!\nCauses could be :\n- There is a giant black hole about to destroy earth, then you should run for your life instead of using this software\n- There was a change in the internal structure of this soft and you should contact the developpers about it."
        #    error "Bug : UpdateValidationStatus has been called by $node whose father is $address\n But problem : $node is not considered as a child of $address"
        #    continue
        #}
        
        foreach child $children {
            lappend childrenStatus $widgetInfo($address.$child-status)
        }
        
        set status [expr min([join $childrenStatus ,])]
	#debug "UpdateValidationStatus : $status ($address  / $node)"
        set widgetInfo($address-status) $status
        eval $widgetInfo($address-refreshStatus)
    }
}

################
# Validation
################
# Using the procedures above, here is a validation procedure. It needs an address to start from.

proc Validate {win address} {
    global widgetInfo tmpTree DStree
    
    #debug "Validate  $address $widgetInfo($address-status)"
    
    set treeAddress [gui2tree $address]
    # Create the node corresponding to address if it does not exist
    if {![dTree_nodeExists $DStree $treeAddress]} {
        dTree_addNode DStree [lrange $treeAddress 0 end-1] [lindex $treeAddress end]
    }
    
    # remove corresponding branch from the DStree
    dTree_rmBranch DStree $treeAddress
    
    # Copy branch from the tmpTree to the DStree
    dTree_copyRelevantBranch tmpTree $treeAddress DStree [gui2tree [addrGetFather $address]]
    
    # If exists, run the script
    if {[info exists widgetInfo($address-scriptAddress)] && $widgetInfo($address-status) == 1} {}
    if {[info exists widgetInfo($address-scriptAddress)] } {    
        openPipe $win $address
    } else { 
        # Umbrella backward  then upward
        CheckValidationStatus $address
        UpdateValidationStatus $address
    }
    
  
}


################
# RefreshFamily 
################
# This procedure is used to refresh all the widgets with a common ancestor (node) to be refreshed with tmpTree values
# !! Correction : in the refresh callback , there is
#   head_refresh : DStree >> Variable
#   tail_check : Variable >> tmpTree
proc RefreshFamily {node} {
    global tmpTree widgetInfo DStree
 
    #puts "RefreshFamily $node : $widgetInfo($node-refresh)"
    eval $widgetInfo($node-refresh)
    foreach child [dTree_getChildren_fast $tmpTree [gui2tree $node]] {
        RefreshFamily "$node.$child"
    }

}


###############
# CheckFamily 
################
# This procedure is used to check all the widgets with a common ancestor (node) to be refreshed with tmpTree values
# !! Correction : in the check callback , there is ONLY 
#   tail_check : Variable >> tmpTree


proc CheckFamily {node} {
    global tmpTree widgetInfo
    #puts "CheckFamily $node : $widgetInfo($node-refresh)"
    
    eval $widgetInfo($node-check)
    foreach child [dTree_getChildren_fast $tmpTree [gui2tree $node]] {
        CheckFamily "$node.$child"
    }
}

################
# this procedure is used in the case of additive widgets : tabs, raddiobutton, modelxor, etc...
# instead of InitWidget. See the radiobox widget for a clean example
################
proc getinfoFatherWidget {} {
    uplevel 1 {
    # Get access to the central data tree
        global DStree
        global XMLtree
        global GuiInfo
        global widgetInfo
        
        read_arguments
        
        set address_father [join $nodes "."]
        set win_father $widgetInfo($address_father-win)
    }
}

########################
# utilitie to clean the memory on widget destruction
#######################
proc cleanWidgetMemory { address} {
    global widgetInfo DStree

    #remove all GUI memory
    foreach item [array names widgetInfo "$address-*"] {
        unset widgetInfo($item)
    }
    # remove all XML memory
    dTree_rmBranch tmpTree [split $address .]

}


###########
# Require procs
###########

proc widget_require_multiple {win address}  {
    global widgetInfo DStree
    
    # skip if element is not existing
    if {[array names widgetInfo $address-require] == ""} {return}
	
    set multipleChildren [dTree_getChildren_fast $DStree $widgetInfo($address-require) ]
    set requiredValue ""
    foreach child $multipleChildren {
	lappend requiredValue [dTree_getAttribute_fast $DStree [concat $widgetInfo($address-require) $child] "value"]
    }
    if {$requiredValue != $widgetInfo($address-requiredValue)} {
	set widgetInfo($address-requiredValue) $requiredValue
	eval $widgetInfo($address-refresh)
    }
}


proc widget_require_default {win address}  {
    global widgetInfo DStree
    
    # skip if element is not existing
    if {[array names widgetInfo $address-require] == ""} {return}
    
    set requiredValue [split [dTree_tryGetAttribute_fast $DStree $widgetInfo($address-require) "value" ""] ";"]		 
    if {$requiredValue != $widgetInfo($address-requiredValue)} {
	set widgetInfo($address-requiredValue) $requiredValue
	eval $widgetInfo($address-refresh)
    }
}

#############
# EXistif procs
#############

proc widget_InquireVisibility {win address} {
    global widgetInfo tmpTree
	# skip if not existing
	if { [catch {set dummy  $widgetInfo($address-visible)}]} {return}
	set oldVisibility $widgetInfo($address-visible)
    if {[expr $widgetInfo($address-existIfArgument)]} {
		if {$oldVisibility == 0} {
			set widgetInfo($address-visible) 1
			eval $widgetInfo($address-check)
			event generate . <<visiblityChanged-$address>>
		}
	} else {
		if {$oldVisibility == 1} {
			set widgetInfo($address-visible) 0
			eval $widgetInfo($address-check)
			event generate . <<visiblityChanged-$address>>
		}
		
    }

}

proc build_existIf_argument {full_address_XML_clean} {
    global XMLtree initializedSolver
    set rawExistif [dTree_getAttribute_fast $XMLtree $full_address_XML_clean "existif"]
    set rawExistif [split $rawExistif "#"]
    
    set listExistif ""
    set valuesToListen ""
    
    if {[expr [llength $rawExistif] % 2] != 1 } {error "[join $rawExistif {"#"}] is not well-formed"}
    for {set i 0} {$i < [llength $rawExistif]} {incr i} {
	if {$i % 2 == 1} {
	    # Assuming this member of the list is an adress to parse
	    # foreach address to parse, replacing keywords by actual address
	    set AddressToParse [lindex $rawExistif $i]
	    set possibleAddresses [dTree_searchNode $XMLtree $AddressToParse]
	    
	    # FISHY , Guillaume c'est quoi ca????
	    if {[llength $possibleAddresses] != 1} {
		set newNode "$initializedSolver $AddressToParse"
		set possibleAddresses [dTree_searchNode $XMLtree $newNode]
		if {[llength $possibleAddresses] != 1} {
		
		    error [subst {"In node : $full_address_XML_clean,  Keywords $newNode have not given relevant results\n Results :\n $possibleAddresses"}]
		}
	    }
	    lappend listExistif [subst {\[dTree_tryGetAttribute_fast \$tmpTree "[lindex $possibleAddresses 0]" "value" "existIf element not found"]}]
	    lappend valuesTolisten [lindex $possibleAddresses 0]
	    
	} else {
	    lappend listExistif [lindex $rawExistif $i]
	}
    }
    return [list $listExistif $valuesTolisten]
    
}

#############
# Decorateurs Refresh et Check
#############



# Le check compare la variable a ce qu'on trouve dans le tmpTree
# Si pas pareil, le tmpTree recoit la variable
# + l'evenement tmptree changed est emis

proc  tail_Check {win address} {
    global tmpTree widgetInfo
    set full_address_XML [split $address "."]
    set nodes [lrange $full_address_XML 0 end-1 ]
    set tree_value [dTree_getAttribute_fast $tmpTree $full_address_XML "value"]
    if { $tree_value != $widgetInfo($address-variable) || $widgetInfo($address-variable) == ""} {
	dTree_setAttribute tmpTree $full_address_XML "value" $widgetInfo($address-variable)
	event generate . <<tmpTreeChanged-$address>>
	# necessaire pour dire aux parents que le statu a change
	updateParent [join $nodes .]    
    }
    eval $widgetInfo($address-refreshStatus)
}



# Le refresh cherche une valeur dans DSTREE
# si elle y est, il l'envoie dans la variable GUI

proc head_Refresh {win address} {
    global widgetInfo DStree

    set full_address_XML [split $address "."]
	
    if {[dTree_attrExists_fast $DStree $full_address_XML "value"]} {
	set widgetInfo($address-variable) [dTree_getAttribute_fast $DStree $full_address_XML "value"]
    }
#    if {$widgetInfo($address-require) != ""} {
#	set reqAddress [tree2gui $widgetInfo($address-require)]
#    }   
}    
    
 

########################
# utilitie to clean the check refresh and refreshStatus of destroyed widget in the requires
#######################

proc widget_destroy {win address} {
    global widgetInfo
    set widgetInfo($address-refresh) ""
    set widgetInfo($address-refreshStatus) ""
    set widgetInfo($address-check) ""
	bind . <<tmpTreeChanged-$address>> ""
	bind . <<TreeChanged-$address>> ""
	bind . <<statusModified-$address>> ""
	bind . <<visiblityChanged-$address>> ""
}




# Note

# Refresh callback
#   1 head_refresh  : DStree >> widgetInfo
#   SPECIFIC
#   2 visibility issues
#   3 tail_check : vairable >> tmpTree

# Check callback
#   SPECIFIC
#   1 tail_check : variable >> tmpTree

# refreshStatus callback
#  SPECIFIC
#	set widgetInfo(fixedview) 1
#        event generate . <<SetView>>
#        set widgetInfo(fixedview) 0



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