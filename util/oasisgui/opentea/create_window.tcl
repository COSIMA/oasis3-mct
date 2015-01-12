#  This program is under CECILL_B licence. See footer for details.

###########################
# this procedure handles the
# creation of the main window
# with the menu at the top
###########################

proc createWin {win} {
    global DStree XMLtree debug widgetInfo FunctionUsed FunctionTime loadApplication applicationList topPath relaunchCmd configPath  faqdata loadedProject wrapper 
    wm title . "OpenTEA"
    wm focusmodel . active    
    . configure -takefocus 1
    
    raise .
    
    if {$widgetInfo(classApp) != "nanoapp"} {

        wm protocol . WM_DELETE_WINDOW {quit_gui}    
        menu .menu
        menu .menu.file
    
        .menu add cascade -menu .menu.file -label "File"
        .menu.file add command -label "Load as new" -command { loadProject_as_new }
        .menu.file add command -label "Load as part " -command {Load_XML_File}
        
        .menu.file add command -label "Save" -command {Save_XML_File  "save"} -accelerator "ctrl-s"
        .menu.file add command -label "Save as" -command {Save_XML_File  "save_as"}    
        .menu.file add separator
        #.menu.file add command -label "Import CSV" -command {Load_CSV_File}
        #.menu.file add command -label "Export CSV" -command {Save_CSV_File}
        .menu.file add separator
        .menu.file add command -label "Generate help" -command {Window_help_generator}  -accelerator "ctrl-h"
        .menu.file add command -label "Preferences" -command {Window_preferences}
        .menu.file add command -label "About  engine" -command {Window_about}
        .menu.file add separator
        .menu.file add command -label "Quit" -command {quit_gui} -accelerator "ctrl-q"
        
        menu .menu.apps
        .menu add cascade -menu .menu.apps -label "Applications"
        
        set accessibleApplicationList $applicationList
        lremove accessibleApplicationList config
        foreach app $applicationList {
            set cmd "$relaunchCmd -code $app &"
            set buttonCommand [subst {
                wm iconify .
                puts "Executing $cmd"
                exec $cmd 
            }]
            .menu.apps add command -label $app -command $buttonCommand
        }
        
        menu .menu.config
        .menu add cascade -menu .menu.config -label "Config"
        set cmd "$relaunchCmd -code config -file $configPath &"
        .menu.config add command -label "Edit config" -command [subst {
            puts "Executing $cmd"
            exec $cmd
            
        }]
        
        menu .menu.myproject
            .menu add cascade -menu .menu.myproject -label "FAQ"
            foreach faqname $faqdata(faqlist) {
                .menu.myproject add command -label $faqdata($faqname-title) -command [subst {Window_faq $faqname}]
            }
        if {$debug} {
            menu .menu.debug
            .menu add cascade -menu .menu.debug -label "Debug"
            .menu.debug add command -label "Show DSTree" -command {Window_DStree .treeviewDS}
            .menu.debug add command -label "Show tmpTree" -command {Window_tmptree .treeviewtmp}
            .menu.debug add command -label "Show XMLTree" -command { Window_XMLtree .treeviewXML}
            .menu.debug add separator
            .menu.debug add command -label "Console" -command { Window_console }
            .menu.debug add command -label "Memory" -command { Window_memory }
            if {$wrapper} {
                .menu.debug add command -label "Profiling" -command { Window_profiling }
            } else {
                .menu.debug add command -label "Profiling" -state disabled
                
            }
            .menu.debug add command -label "Packing" -command { Window_packing }
            .menu.debug add command -label "functions usage" -command {
                puts "=============================="
                puts "       functions usage (function sorted)"
                puts "=============================="
                puts ""
                puts "Function ; number of calls ; time spent (microsec)"
                set functionList [list]
                foreach function [lsort [array names FunctionUsed]] {
                    
                    set func "$function $FunctionUsed($function) $FunctionTime($function) 0"
                    if {$FunctionUsed($function) != 0} {
                        lset func 3 [expr {$FunctionTime($function) / $FunctionUsed($function)}]
                    }
                    
                    lappend functionList $func
                }
                
                foreach function [lsort -index 2 -integer $functionList] {
                    puts [join $function " ; "]
                }
            }
            .menu.debug add separator
            .menu.debug add command -label "AutoPython (dev)" -command { genPyFromXMLtree }        
        }
        
        . config -menu .menu
    }
}


proc quit_gui {} {
    global loadedProject widgetInfo log_channel
    
    
    set win .quit
    
    
    if {[winfo exists $win]} {
        wm deiconify $win
        raise $win
    } else {
        toplevel $win
        wm title $win "Quitting..."
        wm attributes $win 
        ttk::frame $win.f 
        pack $win.f -expand 1
        
        
        set wif $win.f
        
        
        
        ttk::label $wif.icon -image icon_gui_small
        grid $wif.icon  -column 1 -row 0 -pady 5
        
        
        
        # QUIT WOS 
        ttk::frame $wif.quitwos
        grid  $wif.quitwos -column 1 -row 1 -pady 5
            ttk::button $wif.quitwos.butt -text "Quit without saving" -command [subst {
                log  "Quitting  - without saving -"
                close $log_channel
                exit
            }]
            pack $wif.quitwos.butt -side top -pady 5
            ttk::label $wif.quitwos.label -wraplength  $widgetInfo(guiSmallWidgetWidth) -text "Brutal quit : if you did some actions since the last save, the GUI will forget them, BUT the files generated will not revert back to the last saved state."
            pack $wif.quitwos.label -side top -pady 5
            
       
        # CANCEL       
        ttk::frame $wif.cancel
        grid  $wif.cancel -column 1 -row 2 -pady 5 
            
            ttk::button $wif.cancel.butt -text "Cancel" -command [subst {
                destroy $win
                raise .
                focus .
            }]
            pack $wif.cancel.butt -side top -pady 5
            ttk::label $wif.cancel.label -wraplength  $widgetInfo(guiSmallWidgetWidth) -text "Go back to main GUI. Never mind."
            pack $wif.cancel.label -side top -pady 20
        
        
        # SAVE ONLY_GREEN    
        ttk::frame $wif.savegreen
        grid  $wif.savegreen -column 1 -row 3 -pady 5
        
            ttk::button $wif.savegreen.butt -text "Save only green tabs" -command [subst {
                Save_XML_File "save" "greenonly" 
                log  "Quitting  - green only -"
                close $log_channel
                exit
            }]
            pack $wif.savegreen.butt -side top -pady 5
        
            ttk::label $wif.savegreen.label  -wraplength  $widgetInfo(guiSmallWidgetWidth) -text   "only green tabs : only the validated tabs (green icon) will be saved, to avoid the storage of incoherences or spurious data." 
            pack $wif.savegreen.label -side top -pady 5
            
            ttk::label $wif.savegreen.l1 -image icon_ok
            pack $wif.savegreen.l1 -side left -pady 5
        
        
            
        # SAVE ALL
        ttk::frame $wif.saveall
        grid $wif.saveall -column 1 -row 4 -pady 5
            ttk::button $wif.saveall.butt -text "Save all" -command [subst {
                Save_XML_File "save" "all"
                log  "Quitting  - all -"
                close $log_channel
                exit
            }]
            pack $wif.saveall.butt -side top -pady 5
            ttk::label $wif.saveall.label -wraplength  $widgetInfo(guiSmallWidgetWidth) -text "all (Ctrl-S): anything in the project, including incoherent data and additionnal data, will be saved. When loaded, tabs will either be valid (green) or invalid (red)."
            pack $wif.saveall.label -side top -pady 5
            ttk::label $wif.saveall.l1 -image icon_ok
            pack $wif.saveall.l1 -side left -pady 5
            ttk::label $wif.saveall.l2 -image icon_question
            pack $wif.saveall.l2 -side left -pady 5
            ttk::label $wif.saveall.l3 -image icon_error -compound left -text "+ (...)" 
            pack $wif.saveall.l3 -side left -pady 5
         
         
         
        
            
        
        focus $wif.savegreen.butt
    }
    
    
    return

    
}

# Calls for saving and loading files

proc Save_XML_File {mode {typesave "all"} } {
    global DStree loadApplication metaTree loadedProject workingDir widgetInfo
    
    
    set initialDir $workingDir
    if {$loadedProject != "none"} {
        set initialDir [file dirname $loadedProject]
    } else {
        set mode "new"
    }
    
    
    switch $mode {
        
        
        "new" {
            set msgOpen "New : Choose location to save project"
            
            set browsemode [getConfig "config gui paths"]
            switch $browsemode {
                "constant" {
                    set constdir [getConfig "config gui paths constant workingDir"]
                    set where [tk_getSaveFile -parent .  -initialdir $constdir -title $msgOpen -filetypes [list {{XML file} {.xml}}]]
                }
                "auto" {
                    set initialdircommand " -initialdir $workingDir"
                    set where [tk_getSaveFile -parent .  -initialdir $workingDir -title $msgOpen -filetypes [list {{XML file} {.xml}}]]
                }
                "last" -
                default {
                    set where [tk_getSaveFile -parent .  -title $msgOpen -filetypes [list {{XML file} {.xml}}]]
                }
            }
            
            if {$where == "" } {
                return
            }
            set where "[file rootname $where].xml"
        }
        "save_as" {
            set msgOpen "Save as : Choose location to save project"
            set where [tk_getSaveFile -parent . -initialdir [file dirname $loadedProject] -title $msgOpen -filetypes [list {{XML file} {.xml}}]]
            if {$where == "" } {
                return
            }
            set where "[file rootname $where].xml"
            
            
            # copy directory
            set folderName [file rootname $where]
            set originFolderName [file rootname $loadedProject]
            catch {
                file copy $originFolderName $folderName
                log "Try to copy $originFolderName into $folderName"
            } err
            if {$err != ""} {warning "$err"}
            
        }
        "save" {
             set where "self"
        }
        
    }
    
    saveProject $where "xml" $typesave
    
}



# This proc is a brutal kill and load instruction
# This avoid the reminiscence of old data in the newly loaded project.
proc loadProject_as_new { } {
    global relaunchCmd  configPath loadApplication workingDir
    
    set msgOpen "Choose the project to load as a new window"
    set browsemode [getConfig "config gui paths"]
    switch $browsemode {
        "constant" {
            set constdir [getConfig "config gui paths constant workingDir"]
            set fileName [tk_getOpenFile -parent . -initialdir $constdir -title $msgOpen -filetypes [list {{XML file} {.xml}}]]
        }
        "auto" {
            set initialdircommand " -initialdir $workingDir"
            set fileName [tk_getOpenFile -parent . -initialdir $workingDir -title $msgOpen -filetypes [list {{XML file} {.xml}}]]
        }
        "last" -
        default {
            set fileName [tk_getOpenFile -parent .  -title $msgOpen -filetypes [list {{XML file} {.xml}}]]
     }
    }

    if {[file exists $fileName]} {
        
        set cmd "$relaunchCmd -code $loadApplication -file $fileName &"
        puts "Executing $cmd"
        if { [catch [subst { exec $cmd}] fout] } {
            popup_error "Could not launch application $loadApplication with file $fileName : \n $fout "
        } else {
           exit
        }
    }
}



# This is a redirection to the main load procedure,
# without killing the present data

proc Load_XML_File {  } {
    global workingDir
    set msgOpen "Choose the project to add as a part"
    
    set browsemode [getConfig "config gui paths"]
    switch $browsemode {
        "constant" {
            set constdir [getConfig "config gui paths constant workingDir"]
            set fileName [tk_getOpenFile -parent . -initialdir $constdir -title $msgOpen -filetypes [list {{XML file} {.xml}}]]
        }
        "auto" {
            set initialdircommand " -initialdir $workingDir"
            set fileName [tk_getOpenFile -parent . -initialdir $workingDir -title $msgOpen -filetypes [list {{XML file} {.xml}}]]
        }
        "last" -
        default {
            set fileName [tk_getOpenFile -parent .  -title $msgOpen -filetypes [list {{XML file} {.xml}}]]
     }
    }

    if {[file exists $fileName]} {
        loadProject_as_part $fileName                               
    }
}



proc Load_CSV_File { } {
    global workingDir
    set msgOpen "Choose the project to load"
    set fileName [tk_getOpenFile -parent . -title $msgOpen -filetypes [list {{CSV file} {.csv}}]]
    if {[file exists $fileName]} {
        loadProject $fileName                               
    }
}

proc Save_CSV_File { } {
    global DStree loadApplication workingDir
    set msgOpen "Choose location to save project"
    set fileName [tk_getSaveFile -parent .  -title $msgOpen -filetypes [list {{CSV file} {.CSV}}]]
    if {$fileName != "" } {
        set fileNamelpf "[file rootname $fileName].csv"
        saveProject $fileNamelpf  "csv" "greenonly"     
    }
}



# Preference window
proc Window_preferences {} {
    set win .preferences
    global theme focusCorrection widgetInfo
    
    if {[winfo exists $win]} {
        wm deiconify $win
        raise $win
    } else {
        toplevel $win
        wm title $win "Preferences"
        wm attributes $win 
        
        
        
        set win .preferences.bg
        
        ttk::frame $win
        pack $win -fill both
        
        # theme
        ttk::labelframe $win.f -text "Display theme" -width $widgetInfo(guiSmallWidgetWidth)
        pack $win.f -fill x -padx 10 -pady 10
        set theme [ getConfig "config gui appearance theme"] 
        set theme_list [ttk::style theme names]
        if {$theme ni $theme_list} {
             set theme [lindex $theme_list 0]
        }
        
        foreach th $theme_list {
            ttk::radiobutton $win.f.theme_rb_$th -variable theme -value $th -text $th -compound left -command [subst {
                set theme $th
                ThemeUpdate
            }] 
            pack  $win.f.theme_rb_$th -anchor w
        }
        
        ttk::labelframe $win.focus -text "Focus handling" -width $widgetInfo(guiSmallWidgetWidth)
        pack $win.focus -fill x  -padx 10 -pady 10
        ttk::checkbutton $win.focus.cb -variable focusCorrection -command [subst {smartpacker_focus_correction}] -text "Focus correction"
        pack $win.focus.cb
    }
}


# Preference window
proc Window_help_generator {} {
    global workingDir loadApplication help_name help_rootdir help_level
    set win .help_generator
    
    if {[winfo exists $win]} {
        wm deiconify $win
        raise $win
    } else {
        toplevel $win
        wm title $win "Help generation"
        wm attributes $win 
        
        ttk::labelframe $win.f -text "Location"
        pack $win.f -expand 1  -fill x
        
        set help_rootdir "."
        
        ttk::label $win.f.dir_lbl -text "Directory" 
        ttk::entry $win.f.dir_ent -textvariable help_rootdir
        ttk::button $win.f.dir_but -image  icon_folder -command {
            set help_rootdir [ tk_chooseDirectory -title "Choose directory to store html help document" -initialdir $workingDir]
        }
        
        set help_name $loadApplication
        
        ttk::label $win.f.bname_lbl -text "Basename" 
        ttk::entry $win.f.bname_ent -textvariable help_name
        
        grid $win.f.dir_lbl -column 0 -row 0 -pady 5
        grid $win.f.dir_ent -column 1 -row 0 -pady 5
        grid $win.f.dir_but -column 2 -row 0 -pady 5
       
        grid $win.f.bname_lbl -column 0 -row 1 -pady 5
        grid $win.f.bname_ent -column 1 -row 1 -pady 5
        
        ttk::labelframe $win.g -text "Generation parameters"
        pack $win.g -expand 1 -fill x 
        
        set help_level 1
        
        ttk::label $win.g.lvl_lbl -text "Level of detail:" 
        ttk::radiobutton $win.g.lvl0 -text "0 - Structure only" -value 0 -variable help_level
        ttk::radiobutton $win.g.lvl1 -text "1 - Descriptions and help" -value 1 -variable help_level
        ttk::radiobutton $win.g.lvl2 -text "2 - All parameters " -value 2 -variable help_level
        ttk::radiobutton $win.g.lvl3 -text "3 - XML names, existifs, requires" -value 3 -variable help_level
    
        grid $win.g.lvl_lbl -column 0 -row 0 -sticky w
        grid $win.g.lvl0 -column 1 -row 0 -sticky w -pady {5 0}
        grid $win.g.lvl1 -column 1 -row 1 -sticky w
        grid $win.g.lvl2 -column 1 -row 2 -sticky w
        grid $win.g.lvl3 -column 1 -row 3 -sticky w
        
        ttk::button $win.g.gh_butt -text "Generate help" -command {
            generate_help_html
        }
        
        grid $win.g.gh_butt -column 0 -columnspan 2 -row 4 -pady 10 
        
        ttk::frame $win.h
        pack $win.h -expand 1 -fill x 
        
        ttk::label $win.h.desc  -foreground [ThemeColor 0.20] -text "This dialog allows to generate an HTML file by\ncompiling all the help and descriptions present \nin the GUI. Any web browser can read this file."
        pack $win.h.desc -expand 1 -fill x -padx 10 -pady 20
        
    }
}


proc Window_about {} {
global engine_version
 set win .about
    
    if {[winfo exists $win]} {
        wm deiconify $win
        raise $win
    } else {
        toplevel $win
        wm title $win "About engine "
        wm attributes $win
        
        ttk::frame $win.frame
        pack $win.frame
        
        set wif $win.frame
        
        ttk::label $wif.icon -image icon_gui_small
        pack $wif.icon -side top
        ttk::labelframe $wif.v -text "Engine Version"
        
        ttk::label $wif.v.lbl -justify center -text "$engine_version "        
        pack $wif.v.lbl -expand 1        
        
        pack $wif.v -expand 1 -side top -padx 10 -pady 10
        #pack $wif.c -expand 1 -side top -padx 10 -pady 10
        pack $wif.d -expand 1 -side top -padx 10 -pady 10
        
         
        ttk::label $wif.cerfacs -image icon_cerfacs -compound left -text "OpenTEA engine is an open-source product of Cerfacs \n under Cecill-B license."
        pack $wif.cerfacs -side top -pady 10
    }
}


# create a splash screen to show documentation (and maybe other stuff)
proc CreateSplashScreen {  } {
    set win .splash
    if {[winfo exists $win]} {
     destroy $win
    }
    toplevel $win -background [ThemeColor 0.3]
    wm title $win ""
    #wm attributes $win -alpha 0.95
    wm geometry $win 600x500
    label $win.exit -text  "EXIT" -foreground [ThemeColor 0.9] -background [ThemeColor 0.3]
    place $win.exit -relx 0.5 -rely 0.90
    bind $win.exit <ButtonPress> [subst {destroy $win}]

    return $win
}


# Memory browsers

proc Window_DStree { win} {
    global DStree
    #set win .treeviewDS
    
    if {[winfo exists $win]} {
        wm deiconify $win
        raise $win
    } else {
        toplevel $win
        wm title $win "Check DS tree content"
        wm attributes $win 
        ttk::labelframe $win.f -text "Tree view"
        dTreeBrowser_create $win.f.tree
        dTreeBrowser_fill $win.f.tree $DStree
        pack $win.f.tree -expand 1 -fill both
        
        
        pack $win.f -side top -expand 1 -fill both
        pack $win.f.tree
            
        ttk::labelframe $win.bottom
        ttk::separator $win.bottom.s
        ttk::button $win.bottom.b -text "Refresh" -command [subst {
            destroy $win.f.tree
            dTreeBrowser_create $win.f.tree
            pack $win.f.tree -expand 1 -fill both
            dTreeBrowser_fill $win.f.tree \$DStree
        }]

        pack $win.bottom -side top -fill x
        pack $win.bottom.s -side top -fill x
        pack $win.bottom.b -side right -padx 5 -pady 5
    }
}

proc Window_tmptree { win} {
    global tmpTree
    #set win .treeviewDS
    
    if {[winfo exists $win]} {
        wm deiconify $win
        raise $win
    } else {
        toplevel $win
        wm title $win "Check tmp tree content"
        wm attributes $win 
        ttk::labelframe $win.f -text "Tree view"
        dTreeBrowser_create $win.f.tree
        dTreeBrowser_fill $win.f.tree $tmpTree 1
        pack $win.f.tree -expand 1 -fill both
        
        
        pack $win.f -side top -expand 1 -fill both
        pack $win.f.tree
            
        ttk::labelframe $win.bottom
        ttk::separator $win.bottom.s
        ttk::button $win.bottom.b -text "Refresh" -command [subst {
            destroy $win.f.tree
            dTreeBrowser_create $win.f.tree
            pack $win.f.tree -expand 1 -fill both
            dTreeBrowser_fill $win.f.tree \$tmpTree 1
        }]

        pack $win.bottom -side top -fill x
        pack $win.bottom.s -side top -fill x
        pack $win.bottom.b -side right -padx 5 -pady 5
    }
}

proc Window_XMLtree {win} {
    global XMLtree
    #set win .treeviewXML
    
    if {[winfo exists $win]} {
        wm deiconify $win
        raise $win
    } else {
        toplevel $win
        wm title $win "Check XML tree content"
        wm attributes $win 
        ttk::labelframe $win.f -text "Tree view"
        dTreeBrowser_create $win.f.tree
        dTreeBrowser_fill $win.f.tree $XMLtree
        pack $win.f.tree -expand 1 -fill both
        pack $win.f -side top -expand 1 -fill both 
        pack $win.f.tree
            
    }
}

# Console for debug
# Allow to execute short bit of code within the execution of the GUI
proc Window_console {} {
    set win .devConsole
    if {[winfo exists $win]} {
        wm deiconify $win
        raise $win
    } else {
        toplevel $win
        wm title $win "Execute some TclTk script on-the-fly"
        wm attributes $win 
        ttk::labelframe $win.f -text "Script to execute \n (use right click to insert widget)"
        text $win.f.input -relief sunken -background [ThemeColor 1.1]
        pack $win.f -side top -expand 1 -fill both 
        pack $win.f.input -expand 1 -fill both
            
        ttk::labelframe $win.bottom
        ttk::separator $win.bottom.s
        ttk::button $win.bottom.b -text "Execute" -command {
            eval [.devConsole.f.input get 1.0 end]
        }
        pack $win.bottom -side top -fill x
        pack $win.bottom.s -side top -fill x
        pack $win.bottom.b -side right -padx 5 -pady 5
        
        bind . <ButtonPress-2> { .devConsole.f.input insert end %W }
    }
}


# Creation de la fenetre de suivi de la memoire.
# Juge en particulier la taille en nombre de caracteres des donnees sotckees.
proc Window_memory { } {
    set win .devMemory
    if {[winfo exists $win]} {
        wm deiconify $win
        raise $win
    } else {
        toplevel $win
        wm title $win "Show current memory allocation"
        wm attributes $win 
        ttk::labelframe $win.f -text "Memory allocation"
        set width 640
        set height 480
        
        
        pack $win.f -side top -expand 1 -fill both 
        
        canvas $win.f.can -width $width -height $height
        pack $win.f.can -expand 1 -fill both -side top -padx 5 
        
        text $win.f.txt -width 50 -height 10
        pack $win.f.txt -expand 1 -fill both -side top -padx 5
        
        Window_memory_trace $win $width $height
        
        button $win.f.butt -text "Update" -command [subst {Window_memory_trace $win $width $height}]
        pack $win.f.butt -expand 1 -side top -fill both -padx 5 -pady 5
        
    }
    
}

# Plot de l'occupation memoire
proc Window_memory_trace { win width height} {
    
    $win.f.can delete all
    set mem_info [ Window_memory_get ]
    set mem_size [lindex $mem_info 0 ]
    set mem_list [lrange $mem_info 1 end ]
    
    
    # memory repartition
    set x0 [expr {int(0.1*$width)}]
    set y0 [expr {int(0.2*$height)}]
    set h [expr {int(0.2*$height)}]
    set span [expr {int(0.8*$width)}]
    
    
    canvas_text_vector $win.f.can $x0 [expr {$y0-20}] "Memory distribution" "e" 10 0 black ""
    
    
    canvas_text_vector $win.f.can $x0 $y0 "0%" "se" 10 45 black ""
    foreach item $mem_list {
        set name [lindex $item 0]
        set size [expr {int($span*[lindex $item 1])}]
        set ratio [format %0.0f [expr {[lindex $item 1]*100. }]]
        $win.f.can create rectangle $x0 $y0 [expr {$x0 +$size}] [expr {$y0 +$h}] -fill [randcolor] -tags "$name"
        incr x0 $size
        $win.f.can bind "$name" <Motion> [subst {
            $win.f.can delete "hl"
            canvas_text_highlighted $win.f.can %x %y  "$name ($ratio/100)" "hl"
        }]
    }
    canvas_text_vector $win.f.can $x0 $y0 "100%" "se" 10 45 black ""
    
    set x0 [expr {int(0.1*$width)}]
    set y0 [expr {int(0.6*$height)}]
    
    set bins 10
    set minbins 5
    
    set binspan [expr {int($span/$bins)}]
    set x1 [expr {int($x0 + $span * log10($mem_size)/$bins)}]
    
    
    
    canvas_text_vector $win.f.can $x0 $y0 "Memory total  [commify $mem_size] symbols"  "e" 10 0 black ""
    set y0 [expr {int(0.7*$height)}]
    set h [expr {int(0.05*$height)}]
    
    
    set cl [colorscale_define rainbow]
    set color [colorscale_get $cl [expr { (log10($mem_size) - $minbins)/($bins - $minbins) }]]
    for {set bin 0} {$bin <= $bins } {incr bin} {
        canvas_text_vector $win.f.can [expr {$x0+$bin*$binspan}] $y0 "[format %g [expr {10**$bin}]]"  "se" 10 45 black ""
    }
    $win.f.can create rectangle $x0 $y0 $x1 [expr {$y0 +$h}] -fill $color
    
    $win.f.txt insert end "\n [commify $mem_size] symbols"
    
}

# Recuperation de l'occupation memoire
proc Window_memory_get { } {
    uplevel #0 {

        set list_memory ""
        set total_char 0
        foreach var [info vars ] {
            if {[array exists $var]} {
                set names [array names $var]
                
                
                set ll 0   
                foreach name $names {
                    incr ll [string length [subst $[subst $var]($name)] ]
                }
                incr total_char $ll
                
                set tll 0
                foreach cat "status check refresh refreshstatus existIf require" {
                    set tl 0
                    foreach name $names {
                        if {[string match "*$cat" $name  ]} {
                            incr tl [string length [subst $[subst $var]($name)] ]
                        }
                        
                    }
                    lappend list_memory "$var.$cat $tl" 
                    incr tll $tl    
                }
                
                lappend list_memory "$var.misc [expr {$ll-$tll}]" 
                    
                
                
                
                
            } else {
                set ll [string length [subst $$var]]
                lappend list_memory "$var $ll" 
                incr total_char $ll
            }
            
        }

        set list_memory [lsort -index 1 -integer $list_memory]
        set result "$total_char"
        foreach rec $list_memory {
            set size [format "%0.4f" [expr {[lindex $rec 1]*1./$total_char}]]
            
            if {$size > 0} {
                lappend result "[lindex $rec 0] [format "%0.4f" [expr {[lindex $rec 1]*1./$total_char}]]"
            }
        }    
    return $result    
    }
    
}

# Creation de la fenetre de profiling des fonction.
# N'est accessible qu'en mode debug (wrapper.tcl a la place de" unwrap.tcl.
# la taile memoire utilisee explose, car on enregistre les debuts et fins de chaque fonction.
proc Window_profiling { } {
    set win .devProfiling
    if {[winfo exists $win]} {
        wm deiconify $win
        raise $win
    } else {
        toplevel $win
        wm title $win "Show  profiling "
        wm attributes $win 
        ttk::labelframe $win.f -text "Profiling"
        set width 640
        set height 480
        
        pack $win.f -side top -expand 1 -fill both 
        canvas $win.f.can -width $width -height $height
        pack $win.f.can -expand 1 -fill both -side top -padx 5 
        
        text $win.f.txt -width 50 -height 10
        pack $win.f.txt -expand 1 -fill both -side top -padx 5
        
        Window_profiling_trace $win $width $height
        
        button $win.f.butt -text "Clean" -command [subst {Window_profiling_clean $win}]
        pack $win.f.butt -expand 1 -side top -fill both -padx 5 -pady 5
        button $win.f.butt2 -text "Current profile" -command [subst {Window_profiling_trace $win $width $height}]
        pack $win.f.butt2 -expand 1 -side top -fill both -padx 5 -pady 5
        
        
    }
    
}

# plot e l'occupation memoire
proc Window_profiling_trace { win width height} {
    global TraceProfile
    
    
    set TraceProfile 0
    
    $win.f.can delete all
    set prof_info [ Window_profiling_get ]
    set prof_ttime [lindex $prof_info 0 ]
    set prof_ttocc [lindex $prof_info 1 ]
    set prof_list [lrange $prof_info 2 end ]
    
    
    
    
    ## profiling repartition
    set x0 [expr {int(0.3*$width)}]
    set y0 [expr {int(0.2*$height)}]
    set h 10
    set span [expr {int(0.7*$width)}]
    #
    #
    #canvas_text_vector $win.f.can $x0 [expr {$y0-20}] "Memory distribution" "e" 10 0 black ""
    #
    #
    #canvas_text_vector $win.f.can $x0 $y0 "0%" "se" 10 45 black ""
    foreach item $prof_list {
        set name [lindex $item 0]
        set sizet [expr {int($span*[lindex $item 1]*1./$prof_ttime)}]
        set sizeo [expr {int($span*[lindex $item 2]*1./$prof_ttocc)}]
        
        
        $win.f.can create rectangle $x0 $y0 [expr {$x0 +$sizet}] [expr {$y0 +$h}] -fill red -tags "$name"
        $win.f.can create rectangle $x0 [expr {$y0 +$h}] [expr {$x0 +$sizeo}] [expr {$y0 +2*$h}] -fill orange -tags "$name"
        
        canvas_text_vector $win.f.can $x0 $y0 "$name" "nw" 10 0 black ""
        
        incr y0 30
        
        $win.f.can bind "$name" <Motion> [subst {
            $win.f.can delete "hl"
            canvas_text_highlighted $win.f.can %x %y  "$name \ntime=[commify [lindex $item 1]] microsecondes \noccurences=[lindex $item 2] \n [lindex $item 2]/$prof_ttocc " "hl"
        }]
    }
    
    set TraceProfile 1
}

# nettoyage de la fenetre de profiling des fonctions
proc Window_profiling_clean {win } {
    global FunctionUsed
    global FunctionTime
    foreach name [array names FunctionUsed ] {
        set FunctionUsed($name) 0
    }
    set FunctionTime ""
    $win.f.can delete all
    return
}


# analyse de la donnee de profiling
proc Window_profiling_get { } {
   
    global FunctionUsed, FunctionTime, TraceProfile
    set list_profiling ""
    set total_char 0
    set tot_occur 0
    puts "Wrapper says : "
    
    #INITS
    set FuncNames [array names FunctionUsed ]
    foreach fun $FuncNames {
	set FuncCost($fun) 0
    	set FuncOccur($fun) 0
    }

    # DATA MINING
    set tim0 [lindex [split [lindex $FunctionTime 0] "#"] 2 ]
    set fun0 [lindex [split [lindex $FunctionTime 0] "#"] 1 ]
    
    set t0 $tim0
    
    foreach item [lrange $FunctionTime 1 end] {
	set state [lindex  [split $item "#"] 0]
	set fun [lindex  [split $item "#"] 1]
	set tim [lindex  [split $item "#"] 2]
	
	set dtim [expr {$tim-$tim0}]
	switch $state {
	    "IN" {
		#puts "$fun0 $dtim"
		incr FuncCost($fun0) $dtim
                incr FuncOccur($fun)
                incr tot_occur
                
	    }
	    "OUT" {
		#puts "$fun $dtim"
		incr FuncCost($fun) $dtim
	    }
	}
	set fun0 $fun
	set tim0 $tim
	
    }
    
    
    # SORTING
    set list_time ""
    set list_occur ""
    set tot_time 0
    
    
    foreach func $FuncNames {
	set dtim [format "%0.6f" [expr { $FuncCost($func)*0.000001 }]]
	if { $dtim > 0 } {
	    lappend list_time "$func $FuncCost($func) $FuncOccur($func)"
	    incr tot_time  $FuncCost($func)
	}
        
    }
    
    
    
    set list_time "$tot_time $tot_occur [lsort -index 1 -decreasing -real $list_time]"
    
    
    
    #puts "Total time  [format "%0.3f" [expr { $tot_time*0.000001 }]] s."
    #puts "Total time  [format "%0.3f" [expr { ($tim0-$t0)*0.000001 }]] s."
    return $list_time
    
}


# Console pemettant d'identifier l'historique du packing 

proc Window_packing {} {
    set win .devPacking
    if {[winfo exists $win]} {
        wm deiconify $win
        raise $win
    } else {
        toplevel $win
        wm title $win "Show the packing history of a widget"
        wm attributes $win 
        ttk::labelframe $win.f -text "Packing \n (use right click to insert widget)"
        text $win.f.input -relief sunken -background [ThemeColor 1.1]
        $win.f.input insert end "right click in the main window to get the packing history"
        $win.f.input configure -state disabled
        pack $win.f -side top -expand 1 -fill both 
        pack $win.f.input -expand 1 -fill both
        
        bind . <ButtonPress-2> [subst { Window_show_packing $win %W }]
    }
}

# action de recherche du packing
proc Window_show_packing { win widget} {
    global widgetInfo
        set win_list [split $widget "."]
        
        $win.f.input configure -state normal    
        $win.f.input delete 1.0 end
        
        set depth [llength $win_list]
        incr depth -1
        for {set lvl $depth} {$lvl>=1} {incr lvl -1} {
            set address "[join [lrange $win_list 0 $lvl] "." ]"
            $win.f.input insert end "\n" 
            $win.f.input insert end "[winfo class $address] $address \n"            
            $win.f.input insert end " manager:[winfo manager $address]\n"
            $win.f.input insert end " children:\n[join [winfo children $address] "\n"] \n\n"
            
            $win.f.input insert end "Absolute widthXheight     :[winfo width $address]X[winfo height $address]\n"
            $win.f.input insert end "Required widthXheight     :[winfo reqwidth $address]X[winfo reqheight $address]\n"
            $win.f.input insert end "Geometry  widthxheight+x+y:[winfo geometry $address]\n"
            $win.f.input insert end "Small Widgets  :$widgetInfo(guiSmallWidgetWidth) pixels\n"
            $win.f.input insert end "Big Widgets  :$widgetInfo(guiBigWidgetWidth) pixels\n"
            
        }
        $win.f.input configure -state disabled
}


#  update of title
proc updateTitle {} {
    global loadApplication
    global loadedProject
    
    wm title . "OpenTEA  [string toupper $loadApplication]  -  [file tail [file rootname $loadedProject]]"
}



proc Window_faq {faqname} {
    global faqdata
    set win [CreateSplashScreen]
    text $win.text  -foreground [ThemeColor 0.]  -background [ThemeColor 1.1] -highlightthickness 0 -yscrollcommand [list $win.sbary set]
    ttk::scrollbar $win.sbary -orient vertical -command [list $win.text yview]
    
     
    $win.text insert end "\n$faqdata($faqname-title) \n" "Title"
    
    $win.text configure -wrap word
    $win.text tag configure "Body" -font "helvetica 12 " -lmargin1 40 -lmargin2 30 -rmargin 30 -foreground [ThemeColor 0.50]
    $win.text tag configure "Title" -font "helvetica 16 bold" -justify center -foreground [ThemeColor 0.80]
    $win.text tag configure "Section" -font "helvetica 12 bold"  -lmargin1 40 -lmargin2 30 -rmargin 30  -foreground [ThemeColor 0.65]
    $win.text tag configure "Float" -font "helvetica 12 bold" -justify center -foreground [ThemeColor 0.50]
    $win.text tag configure "Item" -font "helvetica 12 " -lmargin1 70 -lmargin2 30 -rmargin 30 -foreground [ThemeColor 0.50]
    
    
        
    #$win.text insert end "\nFreezing application \n" "Section"

    $win.text insert end "\n $faqdata($faqname-body) \n" "Body"
  
    
    
    place  $win.text -relx 0.1 -rely 0.1 -relwidth 0.8 -relheight 0.8
    place  $win.sbary -relx 0.9 -rely 0.1  -relheight 0.8 -anchor ne
    $win.text configure -state disabled
    
}


# experiment.

proc temp_launch_py {} {
    global widgetInfo XDRpath 
    puts "plouf!"
    
    set win "plop"
    set address "bliblablo"
    
    set widgetInfo($address-cancellation) 0
    
    set widgetInfo($address-onScriptStarts) ""
    
    set widgetInfo($address-onScriptStops) ""
    
    set widgetInfo($address-updateProgress) ""
    
    set widgetInfo($address-onScriptEnds) ""
    
    set widgetInfo($address-scriptAddress) [file normalize [file join $XDRpath "hello.py"]]
    
    
    openPipe $win $address 
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
