#  This program is under CECILL_B licence. See footer for details.


set load_start_time [clock milliseconds]


if {[info exists banner] == 0} {
    set banner {.____   ____   ___   _  _   _____  ___     _   
/ __ \ )  _)\ ) __( ) \/ ( )__ __() __(   )_\  
))__(( | '__/ | _)  |  \ |   | |  | _)   /( )\ 
\____/ )_(    )___( )_()_(   )_(  )___( )_/ \_(}
					       
}

##############################################################
# Handle  errors in thecode with a message                   #
##############################################################
proc popup_error {msg {type exitcontinue} } {
    raise .
    .startup.l configure -text  "Problem at startup\nClick here for more informations"
    # Display a pop up for the user
    set win ".popup"
    set ierror 0
    puts "################"
    if {[winfo exists $win]} {
        destroy $win
    } 

    switch $type {
        "exitcontinue" {
            puts "POPUP WARNING..."
            puts $msg
            set reply [tk_dialog $win "Error" $msg ""  0 exit continue]

	    if {$reply == 0} {
                exit
            }
        }
        "strict" {
            puts "POPUP ERROR..."
            puts $msg
            
            tk_messageBox -icon error -message "$msg"
            exit
        }
    }
    
}





####################################
# Read the arguments

ttk::frame .startup
ttk::label .startup.l -text "Starting up GUI..."
pack .startup -fill both 
pack .startup.l



if {[expr [llength $argv] % 2] != 0} {
    tk_messageBox -icon error -message "Error : arguments ($argv) are invalid."
    exit
}

foreach { keyword argument } $argv {
    # test keywords
    if {![string match -* $keyword ]} {
        tk_messageBox -icon error -message "Error : keyword must be like -*"
        exit
    }

    set varname [ string trim $keyword "-"]
    set listkeyword [list "code" "file" "config" "library" "plugins"]
    if {$varname in $listkeyword} {
        set key_$varname $argument
    } else {
	set listkey [join $listkeyword "\n-"]
	#foreach $item $listkeyword {set listkey "$listkey \n $item"}
        popup_error "Error : keyword -$varname is not recognized. \n Use one from \n \n-$listkey" strict
        exit
    }
}



####################################
#Initialize variables



global debug loading FunctionUsed solverList loadApplication configTree widgetInfo configPath pluginsPath starpack relaunchCmd
global logHistory loadedProject metaTree requiredValue additionalWidgets
global workingDir focusCorrection theme banner


set workingDir [pwd]
set configTree ""
set loadApplication "none"
set loadedProject "none"
set abort_cmd ""
set widgetInfo(SetViewMode)  "none"
set widgetInfo(ReGrid)  "no"
set widgetInfo(nregrid) 0
set widgetInfo(form_to_update) ""
set widgetInfo(tabfocus) ""
set widgetInfo(fixedview) "1"
set solverOrder(none) "0"
set log_channel stdout



set debug 1
set wrapper 0
set TraceProfile 1
set tabscroll 1

set ndim 3
set requiredValue ""
set widgetInfo(classApp) ""


####################################
# Initialize the pathes

set pathEngine [file normalize [file dirname [info script]]]

if {"vfs" ni [package names]} {
    #The project has NOT been starpacked / classical behavior
    set starpack 0
    set topPath [file dirname $pathEngine ]
    set relaunchCmd "[file normalize [info nameofexecutable]] [file normalize $argv0] "
} else {
    # The project has been starpacked
    set starpack 1
    set topPath [file dirname $starkit::topdir]
    set relaunchCmd [file normalize [info nameofexecutable]]
}

# Gittag
set gittag_file [file join $topPath "gittag"]
if {[file exists $gittag_file ]} {
    set engine_version [read [ open $gittag_file  ]]
} else {
    set engine_version "- No gittag -"
}

# Config
#set configPath [file normalize [file join $topPath "myconfig.xml"]]
if {[info exists key_config]} {
    set configPathtmp [file normalize [file join [pwd] $key_config]]
    if {[file exists $configPathtmp]} {
	set configPath $configPathtmp
    } else {
	file copy -force $configPath $::env(HOME)
	popup_error "The config file provided $configPathtmp does not exists. A template has been copied on your \$HOME." strict
    }
} else {
    file copy -force $configPath $::env(HOME)
    popup_error "You must specify a config file with -config option. A template has been copied on your \$HOME." strict
}
if {![file exists $configPath]} {
    popup_error "Config file $configPath doesn't exist." strict
}


set relaunchCmd "$relaunchCmd -config $configPath"

# library
set libraryPath [file normalize [file join $topPath "library"]]
if {[info exists key_library]} {
    set libraryPath  [file normalize $key_library]
    set widgetInfo(libraryPath)   $libraryPath

}
if {![file exists $libraryPath]} {
    popup_error "Library folder $libraryPath does not exist." strict
}

set relaunchCmd "$relaunchCmd -config $configPath -library $libraryPath"

# plugin
set pluginsPath [file normalize [file join $topPath "library" "DATA" "pluginscripts"]]
if {[info exists key_plugins]} {
    set pluginsPath  [file normalize $key_plugins]
    set widgetInfo(pluginsPath)  $pluginsPath

}
if {![file exists $pluginsPath]} {
    popup_error "Plugins folder $pluginsPath does not exist." strict
}

set relaunchCmd "$relaunchCmd -config $configPath -plugins $pluginsPath"


# XDR
set XDRpath [file normalize [file join $topPath "XDRpy"]]
if {![file exists $XDRpath]} {
    popup_error "XDR folder $XDRpath does not exist." strict
}


####################################
# Check if XDRpy folder is available in the the PYTHONPATH
if {"PYTHONPATH" in [array names ::env]} {
    puts "PYTHONPATH content at startup : $::env(PYTHONPATH)"
    puts "XDRpy is expected at  : $XDRpath"
    
} else {
    puts "Warning ,  PYTHONPATH variable not set, no link possible with XDRpy"
    puts "The XDRpy library is expected to be located at \n: $XDRpath " 
    puts "exiting..."
    exit
}
    
if { ![string match *XDRpy* $::env(PYTHONPATH)] } {
    puts "Warning ,  no pattern XDRpy found in PYTHONPATH"
    puts "The XDRpy library is expected to be located at : $XDRpath"
    puts "exiting..."
    exit
}








####################################
# Load the procedures


if {$wrapper==1} {
    source [file join $pathEngine "wrapper.tcl"]
} else {
    source [file join $pathEngine "unwrapped.tcl"]
}   

source [file join $pathEngine "functions.tcl"]
source [file join $pathEngine "functions_theme.tcl"]
source [file join $pathEngine "functions_help.tcl"]
source [file join $pathEngine "dTree.tcl"]
source [file join $pathEngine "test_vartypes.tcl"]
source [file join $pathEngine "create_gui.tcl"]
source [file join $pathEngine "declare_icons.tcl"]
source [file join $pathEngine "declare_faq.tcl"]
source [file join $pathEngine "XMLparser.tcl"]
source [file join $pathEngine "dTreeBrowser.tcl"]
source [file join $pathEngine "treeAnalyser.tcl"]
source [file join $pathEngine "create_entry.tcl"]
source [file join $pathEngine "create_cluster.tcl"]
source [file join $pathEngine "create_comment.tcl"]
source [file join $pathEngine "create_comparator.tcl"]
source [file join $pathEngine "create_action.tcl"]
source [file join $pathEngine "create_choice.tcl"]
source [file join $pathEngine "create_dynlist.tcl"]
source [file join $pathEngine "create_graph.tcl"]
source [file join $pathEngine "create_glance.tcl"]
source [file join $pathEngine "create_info.tcl"]
source [file join $pathEngine "create_emptyNode.tcl"]
source [file join $pathEngine "create_multiple.tcl"]
source [file join $pathEngine "create_metaTree.tcl"]
source [file join $pathEngine "create_switch.tcl"]
source [file join $pathEngine "create_browser.tcl"]
source [file join $pathEngine "create_selection.tcl"]
source [file join $pathEngine "create_tabs.tcl"]
source [file join $pathEngine "create_main.tcl"]
source [file join $pathEngine "create_modelframe.tcl"]
source [file join $pathEngine "create_modelxor_light.tcl"]
source [file join $pathEngine "create_solverframe.tcl"]
source [file join $pathEngine "create_scrollform.tcl"]
source [file join $pathEngine "create_switchform.tcl"]
source [file join $pathEngine "create_status.tcl"]
source [file join $pathEngine "create_timeline.tcl"]
source [file join $pathEngine "create_window.tcl"]
source [file join $pathEngine "create_viewer.tcl"]
source [file join $pathEngine "create_viewer3d.tcl"]
source [file join $pathEngine "color_utilities.tcl"]
source [file join $pathEngine "metaWidget_functions.tcl"]
source [file join $pathEngine "config.tcl"]
source [file join $pathEngine "openpipe.tcl"]
source [file join $pathEngine "recentProjects.tcl"]
source [file join $pathEngine "genPyFromXMLtree.tcl"]
source [file join $pathEngine "grapher_utilities.tcl"]
source [file join $pathEngine "smartpacker.tcl"]
source [file join $pathEngine "load_save.tcl"]


if {[info exists additionalWidgets] == 0} {
    set additionalWidgets ""
}

foreach widget $additionalWidgets  {
    source [file join $pathEngine "SOURCES_PRIV" create_$widget.tcl]    
}





.startup.l configure -image  icon_gui_small -compound top


#Build application list, then check
set applicationList [glob -tails -type d -directory $libraryPath *]
lremove applicationList DATA
set widgetInfo(applicationList) $applicationList
if {[info exists key_code]} {
    if {[lsearch $applicationList $key_code] < 0} {
	set listappl [join $applicationList "\n"]
	popup_error "Application $key_code was not found in the library.\n\n Applications available are:\n$listappl." strict
    }
    set loadApplication $key_code
}



if {[info exists key_file]} {
    set fileerror 0
    if {[file extension $key_file] != ".xml"} {
	set fileerror "File $key_file is not an XML file."
    }
    if {![file exists $key_file]} {
	set fileerror "File $key_file was not found."
    }
    
    if {$fileerror != 0 } {
	set dir [file dirname $key_file]
	
	set listfile [join [glob -directory $dir "*.xml"] "\n"]
	
	set fileerror "$fileerror \n\n Other xml files in this directory are \n$listfile."
	
	popup_error $fileerror strict
	exit
    }
    set loadedProject [file normalize $key_file]
}


# declare fonts (see function.tcl)
font_create
set theme [ getConfig "config gui appearance theme"]
ThemeUpdate 



############################
# Creating the Application #
############################


if {$loadApplication == "none"} {
    main_create
    
} else {
    # make sure default solver is in solverList
    if {[lsearch $applicationList $loadApplication] < 0} {
	    tk_messageBox -icon error -message "Application $loadApplication was not found in the library.\nApplications available : [split $applicationList {, }]"
	    exit
    } 
    
    set modelPath [file normalize [file join $libraryPath $loadApplication XML]]
    set codesPath [file join $libraryPath $loadApplication scripts]
    set dataPath [file join $libraryPath]
    
    set startNode ""
    set XMLtree ""
    set tmpTree ""
    set DStree ""
    set metaTree ""
    
     
    global DStree
    global XMLtree
    global tmpTree
    global metaTree
    
   
    
    dTree_init DStree
    dTree_init tmpTree
    
    fill_metaTree

    
    set browsemode [getConfig "config gui paths"]
    if { $browsemode == "constant"} {
	set workingDir [getConfig "config gui paths constant workingDir"]
	if {![file exists $workingDir]} {
	    popup_error "Constant working dir $workingDir doest not exists\nTo fix the pb, edit your file  $configPath \n with a valid path" strict
	}
    } else {
        set workingDir [pwd]
    }
    
    cd $workingDir
    smartpacker_getconfig 
    
    
    
    OpenTeaXML2tree XMLtree $modelPath $dataPath
   
   
    set nodes_to_skip ""
    # Get the list of nodes to skip
    set filename [file join $modelPath "nodes_to_skip" ]
    if [file exists $filename ] {
	set chout [ open $filename r ]
	set content [ split [ read $chout] "\n" ]
	close $chout
	foreach line $content {
	    if {$line != "" && [string index $line 0] != "#"} {
		lappend nodes_to_skip $line
	    }
	}
    }
    
    # Get the list of solvers to add
    set solvers_to_add ""
    set filename [file join $modelPath "solvers_to_add" ]
    if [file exists $filename ] {
	set chout [ open $filename r ]
	set content [ split [ read $chout] "\n" ]
	close $chout
	foreach line $content {
	    if {$line != "" && [string index $line 0] != "#"} {
		lappend solvers_to_add $line
	    }
	}
    }
    
    analyseModelTree XMLtree $nodes_to_skip $solvers_to_add
    
    if {[dTree_attrExists $XMLtree "root $loadApplication" "class"]} {
         set widgetInfo(classApp) [dTree_getAttribute $XMLtree "root $loadApplication" "class"]        
    }
    log_create
    Start_think ""
    createWin .
        
    ttk::frame .main
    pack .main -side left -fill both -expand true
    set gui_win [gui_create -path ".main" -solver $loadApplication]
    wm title . " [string toupper $loadApplication]"
     
    updateTitle    

    
    RefreshFamily "root"
    update
    
    smartpacker_initialize_gui $loadedProject 
    
    
    bind . <Escape> {+Stop_think abort}
    bind . <Control-s> {Save_XML_File  "save"}
    bind . <Control-h> {Window_help_generator}
    bind . <Control-q> {quit_gui}
    
    set focusCorrection [getConfig "config gui appearance focusCorrection"]
    smartpacker_focus_correction
    
    
    set load_stop_time [clock milliseconds]
    set msgloadtime "Startup time :  [format "%0.3f" [expr { ($load_stop_time - $load_start_time)*0.001 }]] s."
    log $msgloadtime

    Stop_think normal
}


destroy .startup





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