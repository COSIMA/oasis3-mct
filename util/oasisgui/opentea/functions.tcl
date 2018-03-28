#  This program is under CECILL_B licence. See footer for details.

#################################################
# need description
#################################################
proc debug {txt } {
    global debug
    if {$debug} {
        log "$txt" "debug"
    }
}

################################################
# need description
#################################################
proc success {txt } {
    global debug
    if {$debug} {
        log "$txt \n" "ok"
    }
}

#################################################
# need description
#################################################
proc warning {txt } {
    log "$txt" "warning"
}

proc pause args {
    set txt "Info : [join $args]"
    tk_messageBox -type ok -title stop -message $txt 
    # -parent .
}

proc font_create {} {
    set graph_font [font create graph_font -family courrier -size 10 ]
    set graph_fonttitle [font create graph_fonttitle -family courrier -size 12 -weight bold -underline true ]
    set log_font [font create log_font -family courier -size 10 ]
}

proc log_create {} {
    global engine_version waypoint workingDir log_channel banner
    
    
    
    set waypoint 0
    
    
    # fenetre log
    ttk::frame .log -relief groove
    pack .log -side bottom -fill x
    set log_lines 7
    
    
    set coltxt [ThemeColor 1.05]
    text .log.text_log -height $log_lines -yscrollcommand {.log.ybar_log set} -wrap word -font log_font  -background [ThemeColor 1.05] -highlightbackground [ThemeColor 1.05]  -selectbackground skyblue1
    
    bind . <<ThemeUpdate>> +[subst {.log.text_log  configure -background \[ThemeColor 1.05\] -highlightbackground \[ThemeColor 1.05\]}]
    
    bind .log.text_log <Enter> [subst { set tabscroll 0 }]
    bind .log.text_log <Leave> [subst { set tabscroll 1 }]
    
  
    
    
    ttk::scrollbar .log.ybar_log -orient vertical -command {.log.text_log yview}
    
    ttk::button .log.copy  -image icon_clipboard -command {
        clipboard clear
        clipboard append [.log.text_log get -- 1.0 end]
        puts [.log.text_log get -- 1.0 end]
    }
    ttk::button .log.palm  -image icon_palm -command [subst {
        .log.text_log configure -state normal
        for {set i 0} {\$i< 5} {incr i} {
            .log.text_log insert end "\n"
        }
        .log.text_log insert end "Waypoint \$waypoint" debug
        incr waypoint
        .log.text_log yview moveto 1.0
        .log.text_log configure -state disabled
    }]
    ttk::button .log.zoom  -image icon_magnifierplus -command {
        set curh [.log.text_log cget -height]
        if {$curh == 7} {
            .log.text_log configure -height 30
            .log.zoom configure -image icon_magnifierminus
        } else {
            .log.text_log configure -height 7
            .log.zoom configure -image icon_magnifierplus
        }
    }
    
    balloon .log.palm "Add some space and a numbered waypoint"
    balloon .log.zoom "Increase/Decrease the size of the log"
    balloon .log.copy "Copy log content to clipboard"
    
    # cartouche Identite
    ttk::frame .log.id 
    set username [getConfig "config id user name"]
    set company [getConfig "config id user company"]
    set logo "icon_$company"
    if {$logo in [image names]} {
        ttk::label .log.id.logo -image "icon_$company"
    } else {
        puts "Warning , image $logo does not exists"
         ttk::label .log.id.logo
    }
    ttk::label .log.id.user -text $username
    ttk::label .log.id.power -style "Multiple.TLabel"  -text "by Cerfacs" -image icon_gui_tiny -compound left -justify center
    
    pack .log.id.logo -side top -anchor center -padx 2
    pack .log.id.user -side top -anchor center -padx 5
    pack .log.id.power -side bottom -anchor center -padx 5
    pack .log.id -side right -pady 2 -padx 2 -fill both
    
    
    pack .log.text_log -fill both -side left -expand true -pady 2 -padx {2 0}
    pack .log.ybar_log -side left -fill y -pady 2 -padx {0 2}
    pack .log.copy -side bottom -pady 2
    pack .log.palm -side bottom -pady 2
    pack .log.zoom -side bottom -pady 2
    
    
    
    
    
    
    
    
    log $banner
    log "Version : $engine_version"
     

    .log.text_log tag configure none -foreground black
    .log.text_log tag configure debug -foreground blue
    .log.text_log tag configure ok -foreground green4
    .log.text_log tag configure warning -foreground orange
    .log.text_log tag configure xdrerror -foreground red4
    .log.text_log tag configure xdrexecute -foreground grey50
    .log.text_log tag configure plugin -foreground violet
    
    
}

proc log {txt {tagid none}} {
    global log_channel
    
    .log.text_log configure -state normal
    .log.text_log insert end "\n$txt" $tagid
    # limit the length of text
    .log.text_log delete 1.0 end-4000lines
    .log.text_log yview moveto 1.0
    .log.text_log configure -state disabled
    
    puts $log_channel "$txt"
    
}

#################################################
# need description
#################################################

proc arginfo {level} {
   set proc [lindex [info level [expr -1-$level]] 0]
   set which [uplevel [list namespace which -command $proc]]
   puts "proc \[$which\]"
   set i -1
   foreach arg [info args $which] {
     incr i
     set value [uplevel [expr 1+$level] [list set $arg] ]
     if { [info default $which $arg def] } {
       puts "  arg\[$i\] \[$arg\] = \[$value\] default = \[$def\]"
     } else {
       puts "  arg\[$i\] \[$arg\] = \[$value\]"
     }
   }
 }

#################################################
# need description
#################################################

proc stackInfo {} {
    puts ""
    puts "====================================="
    set levels [expr [info level]-1]
    for {set i 0} {$i <= $levels} {incr i} {
        arginfo $i
    }
    puts "====================================="
    puts ""
}



proc exec_script {cmd} {
    
    set erreur [ catch {exec -ignorestderr {*}$cmd } err] 
    if {$erreur} {
        warning "Error file attempting to do : $cmd"
        warning $err
        return 0
    }
    return 1
}


#################################################
# retrieve the current folder whatever the OS is
##################################################

proc osPwd {} {
    # First try : UNIX
    if {[catch {set result [exec {pwd}]}]} {
        # Then : Windows
        if {[catch {set result [exec {echo %cd%}]}]} {
            # Unknown
            error "System unknown"
        }
    }
    return $result
}

proc copyFileInWkdir {path} {
    global workingDir
    set copiedfile [file tail $path]
    
    if {$copiedfile == $path} {
    } else {
        file copy -force $path [file join  $workingDir $copiedfile]
    }
    return $copiedfile
}

proc lremove {listVariable value} {
    upvar 1 $listVariable var
    set idx [lsearch -exact $var $value]
    if {$idx != -1} { 
        set var [lreplace $var $idx $idx]
    }
}

# get the max and min of a list
proc list_bounds {input {min 1e+40} {max -1e+40} } {
    foreach item $input {
        if {$item > $max} {set max $item}
        if {$item < $min} {set min $item}
        
    }
    return "$min $max"
}




##############################################################
# Handle unexpected errors in the tcl/tk code with a message #
# Replace the default tk procedure that display a code error #
##############################################################
proc bgerror {error} {
    global errorInfo conf engine_version
    # Save the stack error message and save it in an error file with the time and configuration information
    set bugReport $errorInfo 
    
    set current_dir [pwd]
    set bugFilePath [file join $current_dir "bug_Report.txt"]
    set bugFile [open $bugFilePath w]
    puts $bugFile "Error generated the [clock format [clock seconds] -format {%b. %d, %Y %I:%M:%S %p}]"
    puts $bugFile "Stack report:"
    puts $bugFile "$bugReport"
    puts $bugFile "-----------------------------------------------"
    puts $bugFile "User configuration:"
    foreach var [array names conf] {
        puts $bugFile "$var : $conf($var)"
    }
    close $bugFile

    # Display a pop up for the user
    set win ".error"
    set ierror 0

    
    if {[winfo exists $win]} {
        destroy $win
    } 

    toplevel $win
    wm title $win "Internal error"
    
    
    ttk::frame $win.f
    pack $win.f
    
    ttk::label  $win.f.mess -text "Internal error: \n $error" -image icon_garfield -compound left
    
    
    ttk::frame $win.f.report
    text $win.f.report.txt -yscrollcommand [list $win.f.report.sby set]  -width 70 -height 10 -wrap word
    ttk::scrollbar $win.f.report.sby -orient vertical -command [list $win.f.report.txt  yview]
    $win.f.report.txt insert end  "engine : $engine_version \n \n  $bugReport" 
    grid $win.f.report.txt -row 0 -column 0 -sticky news
    grid $win.f.report.sby -row 0 -column 1 -sticky news
    
    
    ttk::separator $win.f.sep 
    ttk::label $win.f.panel -text "This error is not handled . Contact the person in charge on your site\n On his/her behalf, post a full description on how it happened plus the file:\n $bugFilePath \n " -justify center
    pack $win.f.mess -pady 20
    pack $win.f.report -pady 10
    pack $win.f.sep -fill x
    pack $win.f.panel -pady 20
    
    raise $win .

}




proc balloon {w help} {
    bind $w <Any-Enter> "after 500 [list balloon:show %W [list $help]]"
    bind $w <Any-Leave> "destroy %W.balloon"
}

proc balloon:show {w arg} {
    if {[eval winfo containing  [winfo pointerxy .]]!=$w} {return}
    set top $w.balloon
    catch {destroy $top}
    toplevel $top -bd 1 -bg black
    wm overrideredirect $top 1
    
    if {[string equal [tk windowingsystem] aqua]}  {
        ::tk::unsupported::MacWindowStyle style $top help none
    }   
    pack [message $top.txt -aspect 10000 -bg lightyellow \
            -font fixed -text $arg]
    set wmx [winfo rootx $w]
    set wmy [expr [winfo rooty $w]+[winfo height $w]]
    wm geometry $top \
      [winfo reqwidth $top.txt]x[winfo reqheight $top.txt]+$wmx+$wmy
    raise $top
}

proc split_list {L} {
    
    set len [expr {int(0.5*[llength $L])}]
    set A ""
    set B ""
    for {set i 0} {$i< $len} {incr i} {
        lappend A [lindex $L [expr {$i*2}]]
        lappend B [lindex $L [expr {$i*2+1}]]
    }
    return "{$A} {$B}"
}

proc get_keys_listcsv {L} {
    
    set L [split $L ";"]
    set len [expr {int(0.5*[llength $L])}]
    set A ""
    for {set i 0} {$i< $len} {incr i} {
        lappend A [lindex $L [expr {$i*2}]]
    }
    return $A 
}

proc get_values_listcsv {L} {
    set L [split $L ";"]
    set len [expr {int(0.5*[llength $L])}]
    set A ""
    for {set i 0} {$i< $len} {incr i} {
        lappend A [lindex $L [expr {$i*2+1}]]
    }
    return $A
}

proc keyvalue_listcsv {L} {
    set L [split $L ";"]
    set len [expr {int(0.5*[llength $L])}]
    set A ""

    for {set i 0} {$i< $len} {incr i} {
        lappend A "[lindex $L [expr {$i*2}]] [lindex $L [expr {$i*2+1}]]"
    }
    return $A
}

# to  limit  huge chunks of text
proc limit_string {str max} {
    set str2 $str
    set bit [expr {int(0.5*$max)}]
    if {[string length $str] >$max} {
        set str2 "[string range $str 0 $bit](...) [string range $str end-$bit end] "
    }
    return $str2
}

#to get the parent of a dot  separated  string
proc crop_address {adr levels} {
    set ladr [split $adr "."]
    set end_adr [expr {[llength $ladr] - $levels -1 }]
    
    if {$end_adr <0 } {
        error "crop_address cannot crop $levels levels from $adr"
        return
    }
    set new_adr [join [lrange $ladr 0 $end_adr] "."]
    return $new_adr
}


proc canvas_text_vector {w x y txt pos size rotate color tags} {
    
    set hsize [expr {0.7 *$size}]
    set csize [expr {0.7 *$hsize}]
    
    set slant 0.0
    set rotate [expr {$rotate*1./180*3.1415}]
    
    set txtl [split  $txt ""]
    
    set ltxt [llength $txtl]
    set wtxt [expr {$ltxt*$hsize}]
    
    set x0 $x
    set y0 $y
   
    
    
    
    
    switch $pos {
        "center" {
            set xstart [expr {-$wtxt*0.5}]
            set ystart [expr {-$size*0.5}]
        }
        "ne" {
            set xstart [expr {+$wtxt*0.1}]
            set ystart [expr {+$size*0.1}]
        }
        "e" {
            set xstart [expr {+$wtxt*0.1}]
            set ystart [expr {-$size*0.5}]
        }
        "se" {
            set xstart [expr {+$wtxt*0.1}]
            set ystart [expr {-$size*1.1}]
        }
        "nw" {
            set xstart [expr {-$wtxt*1 -$size*0.1}]
            set ystart [expr {+$size*0.1}]
        }
        "w" {
            set xstart [expr {-$wtxt*1 -$size*0.1}]
            set ystart [expr {-$size*0.5}]
        }
        "sw" {
            set xstart [expr {-$wtxt*1 -$size*0.1}]
            set ystart [expr {-$size*1.1}]
        }
    }
    
    foreach char $txtl {
        switch $char {
            
            "0" {set draw {{0.7 0} {1 0.3}  {1 0.7} {0.7 1} {0.7 1} {0.3 1} {0 0.7} {0 0.3} {0.3 0} {0.7 0}  {0.3 1}}}
            "1" {set draw {{0.1 0} {0.9 0} {0.5 0} {0.5 1} {0.2 0.7}}}
            "2" {set draw {{1 0} {0 0} {1 0.7} {0.7 1} {0.7 1} {0.3 1} {0 0.7}}}
            "3" {set draw {{0 0} {0.7 0} {1 0.3} {0.8 0.6} {0.5 0.6} {0.8 0.6} {1 0.7} {1 0.8} {0.8 1} {0.7 1} {0 1} }}
            "4" {set draw {{0.7 0} {0.7 1} {0 0.3} {1 0.3} }}
            "5" {set draw {{1 1} {0 1} {0 0.55} {1 0.55} {1 0.2} {0.8 0} {0 0}}}
            "6" {set draw {{0.9 1} {0.3 1} {0 0.7} {0 0.2} {0.2 0} {0.8 0} {1 0.2} {1 0.4} {0.8 0.5} {0 0.5}}}
            "7" {set draw {{0 0} {0 0.2} {1 1} {0 1} {0 0.9}}}
            "8" {set draw {{0.7 0} {1 0.3} {1 0.4} {0.1 0.7} {0.1 0.8} {0.3 1} {0.7 1} {0.9 0.8} {0.9 0.7}  {0 0.4} {0 0.3} {0.3 0} {0.7 0}}}
            "9" {set draw {{0 0} {0.80 0} {1 0.2} {1 0.8} {0.8 1} {0.2 1} {0 0.8} {0 0.6} {0.1 0.5} {1 0.5} }}
            "A" {set draw {{0 0} {0.5 1} {0.75 0.5} {0.25 0.5} {0.75 0.5} {1 0} }}
            "a" {set draw {{1 0} {1 0.7} {0.2 0.7} {0 0.5} {0 0.1} {0.1 0} {0.9 0} {1 0.1} }}
            "B" {set draw {{0 0.5} {0.8 0.5} {1 0.7} {1 0.8} {0.8 1} {0 1} {0 0} {0.8 0} {1 0.2} {1 0.3} {0.8 0.5} }}
            "b" {set draw {{0 1} {0 0} {0.8 0} {1 0.1} {1 0.5} {0.5 0.7} {0 0.5}}}
            "C" {set draw {{1 1} {0.2 1} {0 0.8} {0 0.2} {0.2 0} {1 0}}}
            "c" {set draw {{1 0.7} {0.2 0.7} {0 0.5} {0 0.2} {0.2 0} {1 0}}}
            "D" {set draw {{0 1} {0 0} {0.8 0} {1 0.2} {1 0.8} {0.8 1} {0 1}}}
            "d" {set draw {{1 1} {1 0} {1 0.1} {0.9 0} {0.2 0} {0 0.2} {0 0.4} {0.5 0.7} {1 0.7}}}
            "E" {set draw {{1 0} {0 0} {0 0.5} {0.4 0.5} {0 0.5} {0 1} {1 1} }}
            "e" {set draw {{0 0.35} {1 0.35} {1 0.5} {0.8 0.7} {0.2 0.7} {0 0.5} {0 0.2} {0.2 0} {1 0}}}
            "F" {set draw {{0 0} {0 0.5} {0.4 0.5} {0 0.5} {0 1} {1 1} }}
            "f" {set draw {{0.5 0} {0.5 0.7} {0 0.7} {1 0.7} {0.5 0.7} {0.5 0.8} {0.8 1} {1 1} }}
            "G" {set draw {{1 1} {0.2 1} {0 0.8 } {0 0.2} {0.2 0} {1 0} {1 0.5} {0.8 0.5} }}
            "g" {set draw {  {0 -0.1} {0.2 -0.3} {0.8 -0.3} {1 -0.1} {1 0.7} {0.3 0.7} {0 0.3} {0 0.3} {0.3 0} {0.8 0} {1 0.2}  }}
            "H" {set draw {{0 0} {0 1} {0 0.5} {1 0.5} {1 1} {1 0}}}
            "h" {set draw {{0 1} {0 0} {0 0.5} {0.5 0.7} {0.8 0.7} {1 0.5} {1 0}}}
            "I" {set draw {{0.3 0} {0.7 0} {0.5 0} {0.5 1} {0.7 1} {0.3 1}}}
            "i" {set draw {{0.3 0.7} {0.5 0.7} {0.5 0} {0.8 0}}}
            "J" {set draw {{0 0} {0.5 0} {0.7 0.2} {0.7 1} {0.5 1} }}
            "j" {set draw {{0.1 0.7} {0.7 0.7} {0.7 -0.1} {0.5 -0.3} {0 -0.3}}}
            "K" {set draw {{0 0} {0 1} {0 0.6} {1 0} {0 0.6} {1 1} }}
            "k" {set draw {{0 0} {0 1} {0 0.4} {1 0} {0 0.4} {1 0.7}}}
            "L" {set draw {{0 1} {0 0} {1 0}}}
            "l" {set draw {{0.3 1} {0.5 1} {0.5 0} {0.8 0}}}
            "M" {set draw {{0 0} {0 1} {0.5 0.3} {1 1} {1 0}}}
            "m" {set draw {{0 0} {0 0.7} {0 0.5} {0.5 0.7} {0.5 0} {0.5 0.5} {0.8 0.7} {1 0.5} {1 0}}}
            "N" {set draw {{0 0} {0 1}  {1 0} {1 1} }}
            "n" {set draw {{0 0} {0 0.7} {0 0.5} {0.8 0.7} {1 0.5} {1 0}  }}
            "O" {set draw {{0.7 0} {1 0.3}  {1 0.7} {0.7 1} {0.7 1} {0.3 1} {0 0.7} {0 0.3} {0.3 0} {0.7 0}}}
            "o" {set draw {{0.7 0} {1 0.3}  {1 0.5} {0.7 0.7} {0.7 0.7} {0.3 0.7} {0 0.5} {0 0.3} {0.3 0} {0.7 0}}}
            "P" {set draw {{0 0} {0 1}  {0.8 1} {1 0.8} {1 0.7} {0.8 0.5} {0 0.5}}}
            "p" {set draw {{0 0.7} {0 -0.3}  {0 0} {0.8 0} {1 0.2} {1 0.5} {0.5 0.7} {0 0.5}}}
            "Q" {set draw {{0.7 0} {1 0.3}  {1 0.7} {0.7 1} {0.7 1} {0.3 1} {0 0.7} {0 0.3} {0.3 0} {0.7 0} {0.9 -0.2}}}
            "q" {set draw {{1 -0.3} {1 0.7} {0.2 0.7} {0 0.5} {0 0.1} {0.1 0} {0.9 0} {1 0.1} }}
            "R" {set draw {{0 0} {0 1}  {0.8 1} {1 0.8} {1 0.7} {0.8 0.5} {0 0.5} {1 0} }}
            "r" {set draw {{0 0.7} {0 0}  {0 0.5} {0.5 0.7} {1 0.7} {1 0.5}}}
            "S" {set draw {{0 0} {0.8 0}  {1 0.2} {1 0.3} {0 0.7} {0 0.8} {0.2 1} {1 1}}}
            "s" {set draw {{0 0} {0.8 0}  {1 0.2} {1 0.3} {0 0.5} {0 0.6} {0.2 0.7} {1 0.7}}}
            "T" {set draw {{0.5 0} {0.5 1}  {1 1} {0 1}}}
            "t" {set draw {{0 0.7} {0.8 0.7}  {0.4 0.7} {0.4 1} {0.4 0.2} {0.6 0} {1 0}}}
            "U" {set draw {{0 1} {0 0.2}  {0.2 0} {0.8 0} {1 0.2} {1 1}}}
            "u" {set draw {{0 0.7} {0 0.2}  {0.2 0} {0.8 0} {1 0.2} {1 0.7} {1 0}}}
            "V" {set draw {{0 1} {0.5 0}  {1 1} }}
            "v" {set draw {{0 0.7} {0.5 0}  {1 0.7} }}
            "W" {set draw {{0 1} {0.3 0}  {0.5 1} {0.7 0} {1 1}}}
            "w" {set draw {{0 0.7} {0.3 0}  {0.5 0.7} {0.7 0} {1 0.7}}}
            "X" {set draw {{0 0} {1 1}  {0.5 0.5} {1 0} {0 1}}}
            "x" {set draw {{0 0} {1 0.7}  {0.5 0.35} {1 0} {0 0.7}}}
            "Y" {set draw {{0.5 0} {0.5 0.6}  {0 1} {0.5 0.6} {1 1}}}
            "y" {set draw {{0 0.7} {0.5 0}  {1 0.7} {0.4 -0.1} {0 -0.3}}}
            "Z" {set draw {{1 0} {0 0}  {1 1} {0 1}}}
            "z" {set draw {{1 0} {0 0}  {1 0.7} {0 0.7}}}
            "," {set draw {{0.7 0.20} {0.7 0} {0.9 -0.3} }}
            "." {set draw {{0.7 0.20} {0.7 0} }}
            "+" {set draw {{0.1 0.5} {0.9 0.5} {0.5 0.5} {0.5 0.2} {0.5 0.8} }}
            "-" {set draw {{0.1 0.5} {0.9 0.5}}}
            "(" {set draw { {0.7 1} {0.3 1} {0 0.7} {0 0.3} {0.3 0} {0.7 0}  }}
            ")" {set draw { {0.3 1} {0.7 1} {1 0.7} {1 0.3} {0.7 0} {0.3 0}  }}
            "[" {set draw { {0.7 1} {0 1} {0 0} {0.7 0} }}
            "]" {set draw { {0.3 1} {1 1} {1 0} {0.3 0} }}
            
            default {set draw ""}
        }
        set line ""
        foreach point $draw {
            set xx [expr { [lindex $point 0] * $csize } ]
            set yy [expr { ( 1. - [lindex $point 1] ) * $size } ]
            
            set x1 [expr { $xstart + $xx - $slant*$yy }]
            set y1 [expr { $ystart + $yy }]
            
            set x2 [expr {$x0+cos($rotate)*$x1 + sin($rotate)*$y1} ]
            set y2 [expr {$y0-sin($rotate)*$x1 + cos($rotate)*$y1} ]
            
            
            lappend line $x2
            lappend line $y2
        }
        if {$draw != ""} {
        $w create line  $line -fill $color -width 1 -tags $tags
        }
        set xstart [expr { $xstart + $hsize }]
        
        #set ystart [expr { $ystart - sin($rotate)*$hsize + cos($rotate)*$size}]
    }
    
}

proc canvas_makegif2 {win filename} {
    global widgetInfo
    
    $win delete pointer
    set bbox [ $win bbox all]
    if {$bbox == ""} {return}
    set x0 [lindex $bbox 0]
    set y0 [lindex $bbox 1]
    set x1 [lindex $bbox 2]
    set y1 [lindex $bbox 3]
    
    set nx [expr {int ($x1 -$x0)}]
    set ny [expr {int ($y1 -$y0)}]
     
    image create photo fooble -width $nx -height $ny
    fooble blank
    foreach item [$win find all] {
        set rawcol [$win itemcget $item -fill]
        if {$rawcol == ""} {
            set rawcol [$win itemcget $item -outline]
        }
        if {$rawcol != ""} {
            set color [eval format "#%04x%04x%04x" [winfo rgb . $rawcol]]
            set type [$win type $item ]
            
            switch $type {
                "line" {
                    set coords [$win coords $item]
                    set xi0 [expr {([lindex $coords 0])}]
                    set yi0 [expr {([lindex $coords 1])}]
                    foreach {xi1 yi1} $coords {
                        set dx [expr {$xi1-$xi0}]
                        set dy [expr {$yi1-$yi0}]
                        set adx [expr {abs($dx)}]
                        set ady [expr {abs($dy)}]
                        if {$dx>0} {
                                set stepx 1
                        } else {
                                set stepx -1
                        }
                        if {$dy>0} {
                                set stepy 1
                        } else {
                                set stepy -1
                        }
                            
                        if { $adx > $ady} {
                            if {$adx > 1} {
                                if {$stepx == 1} {
                                for {set x [expr {int($xi0)}]} {$x <$xi1} {incr x 1} {
                                    set y [expr {int($yi0 + ($x - $xi0)/($xi1-$xi0) * ($yi1-$yi0))}]
                                    fooble put $color -to [expr {($x-$x0)}] [expr {($y-$y0)}]
                                }
                                } else {
                                  for {set x [expr {int($xi1)}]} {$x <$xi0} {incr x 1} {
                                    set y [expr {int($yi0 + ($x - $xi0)/($xi1-$xi0) * ($yi1-$yi0))}]
                                    fooble put $color -to [expr {($x-$x0)}] [expr {($y-$y0)}]
                                }  
                                }
                            }
                        }          
                        if {$ady > $adx } {
                            if {$ady > 1} {
                                if {$stepy == 1} {
                                    for {set y [expr {int($yi0)}]} {$y <$yi1} {incr y 1} {
                                        set x [expr {int($xi0 + ($y - $yi0)/($yi1-$yi0) * ($xi1-$xi0))}]
                                         fooble put $color -to [expr {($x-$x0)}] [expr {($y-$y0)}]
                                    }
                                } else {
                                    for {set y [expr {int($yi1)}]} {$y <$yi0} {incr y 1} {
                                        set x [expr {int($xi0 + ($y - $yi0)/($yi1-$yi0) * ($xi1-$xi0))}]
                                         fooble put $color -to [expr {($x-$x0)}] [expr {($y-$y0)}]
                                    }
                                    
                                }
                            }
                        }
                                      
                        set xi0 $xi1
                        set yi0 $yi1
                    }
                }
                "oval" {
                    set coords [$win coords $item]
                    set xmin [lindex $coords 0]
                    set ymin [lindex $coords 1]
                    set xmax [lindex $coords 2]
                    set ymax [lindex $coords 3]
                    set xmid [expr {(0.5*($xmax+$xmin))}]
                    set ymid [expr {(0.5*($ymax+$ymin))}]
                    set dx  [expr {($xmax-$xmin)}]
                    set rad  [expr {(0.5*$dx)}]
                    set hrad [expr {(0.36*$dx)}]
                    fooble put  $color -to [expr {int($xmid-$hrad-$x0)}] [expr {int($ymid-$hrad-$y0)}] [expr {int($xmid+$hrad-$x0)}] [expr {int($ymid+$hrad-$y0)}] 
                    
                    #for {set x [expr {int(-$hrad)}] } {$x <=$hrad} {incr x} {
                    #    for {set y [expr {int($hrad)}] } {$y <= $rad} {incr y} {
                    #        #puts "here $x $y"
                    #        set radius [expr {$x*$x+$y*$y}]
                    #        if {$radius < [expr {$rad*$rad}]} {
                    #            fooble put $color -to [expr {int($xmid+$x-$x0)}] [expr {int($ymid+$y-$y0)}]
                    #            fooble put $color -to [expr {int($xmid+$x-$x0)}] [expr {int($ymid-$y-$y0)}]
                    #            fooble put $color -to [expr {int($xmid+$y-$x0)}] [expr {int($ymid+$x-$y0)}]
                    #            fooble put $color -to [expr {int($xmid-$y-$x0)}] [expr {int($ymid+$x-$y0)}]
                    #        }
                    #    }    
                    #}   
                }
                "default" {
                    set bbox [$win bbox $item]
                    set xi0 [lindex $bbox 0]
                    set yi0 [lindex $bbox 1]
                    set xi1 [lindex $bbox 2]
                    set yi1 [lindex $bbox 3]
                    canvas_makegif_scan $win $xi0 $xi1 $yi0 $yi1 $x0 $y0
                }
            }
            #switch $type {
            #    "oval" {
            #        
            #        set coords [$win coords $item]
            #        set xmin [lindex $coords 0]
            #        set ymin [lindex $coords 1]
            #        set xmax [lindex $coords 2]
            #        set ymax [lindex $coords 3]
            #        fooble put  $color -to [expr {int($xmin-$x0)}] [expr {int($ymin-$y0)}] [expr {int($xmax-$x0)}] [expr {int($ymax-$y0)}]
            #    }
            #    "line" {
            #        set coords [$win coords $item]
            #        
            #        fooble put  blue -to [expr {int($xmin-$x0)}] [expr {int($ymin-$y0)}] [expr {int($xmax-$x0)}] [expr {int($ymax-$y0)}]
            #    }
            #}
        }
    }
    fooble write $filename -format gif
    puts "Done..."
}



proc canvas_makegif {win filename} {
    global widgetInfo
    
    $win delete pointer
    set bbox [ $win bbox all]
    if {$bbox == ""} {return}
    set x0 [lindex $bbox 0]
    set y0 [lindex $bbox 1]
    set x1 [lindex $bbox 2]
    set y1 [lindex $bbox 3]
    
    set nx [expr {int ($x1 -$x0)}]
    set ny [expr {int ($y1 -$y0)}]
     
    image create photo fooble -width $nx -height $ny
    fooble blank
    canvas_makegif_iterate $win $x0 $x1 $y0 $y1 $x0 $y0 
    fooble write $filename -format gif
    image delete fooble
    #puts "Done..."
}

proc canvas_makegif_scan {win xmin xmax ymin ymax x0 y0 } {
    set item [$win find overlapping $xmin $ymin $xmax $ymax]
    if {$item == ""} {
    #    #debug "Skipping square  $square"
        return
    }
    
    set multipleitem 1
    
    if {[llength $item] == 1} {
        set rawcol [$win itemcget $item -fill]
        if {$rawcol == ""} {
            set rawcol [$win itemcget $item -outline]
        }
        if {$rawcol != ""} {
            set color [eval format "#%04x%04x%04x" [winfo rgb . $rawcol]]
            set multipleitem 0
            #fooble put  $color -to [expr {$xmin-$x0}] [expr {$ymin-$y0}] [expr {$xmax-$x0}] [expr {$ymax-$y0}]
        } else {
            #set color ""
            #nocolor for single item, skip
            return
        } 
    }
    
    for {set x $xmin} {$x<=$xmax} {incr x} {
        for {set y $ymin} {$y<=$ymax} {incr y} {
            set item [$win find overlapping $x $y $x $y]
            if {$item != ""} {
                
                if {$multipleitem} {
                    set item [lindex $item end]
                    set rawcol [$win itemcget $item -fill]
                    if {$rawcol == ""} {
                        set rawcol [$win itemcget $item -outline]
                    }
                    if {$rawcol != ""} {
                        set color [eval format "#%04x%04x%04x" [winfo rgb . $rawcol]]
                    } else {
                        set color ""
                    }
                }
                if {$color != ""} {
                    fooble put  $color -to [expr {$x-$x0}] [expr {$y-$y0}]
                }
            }
        }
    }
}

proc canvas_makegif_iterate {win xmin xmax ymin ymax x0 y0 } {
    set item  [$win find overlapping $xmin $ymin $xmax $ymax] 

    if {$item== ""} {
        #debug "Skipping square  $square"
        return
    }
   
    set xmid [expr {int (0.5*($xmax+$xmin))}]
    set ymid [expr {int (0.5*($ymax+$ymin))}]
    
    set xmidp1 [expr {$xmid+1}]
    set ymidp1 [expr {$ymid+1}]
    
    if { [expr ($ymid-$ymin)]> 2} {
        canvas_makegif_iterate $win $xmin $xmid $ymin $ymid $x0 $y0 
        canvas_makegif_iterate $win $xmin $xmid $ymidp1 $ymax $x0 $y0 
        canvas_makegif_iterate $win $xmidp1 $xmax $ymin $ymid $x0 $y0 
        canvas_makegif_iterate $win $xmidp1 $xmax $ymidp1 $ymax $x0 $y0 
    } else {
        canvas_makegif_scan $win $xmin $xmax $ymin $ymax $x0 $y0
    }
    return
}


# function to create a text on a canvas with surrounding white
proc canvas_text_highlighted {wincan x y  txt tags} {
    set bbox [$wincan bbox all]
    set midx [expr {0.5*([lindex $bbox 0]+[lindex $bbox 2])}]
    set midy [expr {0.5*([lindex $bbox 1]+[lindex $bbox 3])}]
    
    if {$x>$midx} {
        set dx -5
        set posx e
    } else {
        set dx 5
        set posx w
    }
    if {$y>$midy} {
        set dy -5
        set posy s
    } else {
        set dy 5
        set posy n
    }
    
    set anchor "$posy$posx"
    
    $wincan create text [expr {$dx+$x-1}] [expr {$dy+$y-1}] -text $txt -tags $tags -anchor $anchor -fill white
    $wincan create text [expr {$dx+$x+1}] [expr {$dy+$y-1}] -text $txt -tags $tags -anchor $anchor -fill white
    $wincan create text [expr {$dx+$x-1}] [expr {$dy+$y+1}] -text $txt -tags $tags -anchor $anchor -fill white
    $wincan create text [expr {$dx+$x+1}] [expr {$dy+$y+1}] -text $txt -tags $tags -anchor $anchor -fill white
    $wincan create text [expr {$dx+$x}]   [expr {$dy+$y}]   -text $txt -tags $tags -anchor $anchor -fill black
}


proc commify {num {sep ,}} {
    while {[regsub {^([-+]?\d+)(\d\d\d)} $num "\\1$sep\\2" num]} {}
    return $num
}


proc printtime { start stop } {
    return [format "%0.3f" [expr { ($stop - $start)*0.001 }]]
}

proc stringclean { str } {
    set str_c [string map {"à" "a" "ç" "c" "é" "e" "è" "e" "ù" "u" "ú" "u" "â" "a" "\t" " " ">" ".gt." "<" ".lt."} $str ]
    return $str_c
}

proc stringcleanBalise { str } {
    set str_c [stringclean $str]
    # no spaces or operands
    set str_c [string map {" " "_" "*" "times" "+" "plus" "/" "divide" "\}" "\)" "\{" "\(" "%" "" "!" "" "?" ""} $str_c ]
    return $str_c
}



proc string2latex { str } {
    set str_c [string map {"à" "\\`{a}" "ç" "\\c{c}" "é" "\\'{e}" "è" "\\`{e}" "ù" "\\`{u}" "ú" "\\'{u}" } $str ]
    return $str_c
}

# return a random integer between min and max
proc myRand { min max } {
    set value [ expr {int(rand() * ($max - $min)) + $min}]
    return $value
}

# EASTER EGG
proc random_msg {} {
    set messages ""
    
    # envoie un message une fois sur dix seulement
    if {[myRand 1 100] > 10 } { return ""}
    
    lappend messages "Get outta here and go back to your boring programs."
    lappend messages "Just leave. When you come back, i'll be waiting with a bat."
    lappend messages "You're lucky i don't smack you for thinking about leaving."
    # TP spirit
    lappend messages "Vous avez verifie les bilans du calcul ? (T.P.)"
    lappend messages "Sur 100 calculs faux, 90 n'ont pas le bon point de fonctionnement, et 9 n'ont pas la bonne geometrie. (T.P.)"
    lappend messages "Si on a le bon signe et la bonne dimension, alors c'est un bon modele. (T.P.)"
    lappend messages "On ne change qu'un parametre a la fois. Un seul. Pas d'exceptions. (T.P.)"
    lappend messages "Il vaut mieux un qui sait que dix qui cherchent (T.P.)"
    
    lappend messages "Est ce que vous avez verifié Shannon? Tout est dans Shannon. (V.M.)"
    
    lappend messages "B****l de b**e (H.D.)"
    
    lappend messages "The answer is 42."
    lappend messages "He's dead, Jim!"
    lappend messages "Si vous pouvez lire ceci, c'est que vous n'avez probablement pas assez de travail."
    lappend messages "Dans simuler, y a -muler"
    #lappend messages "Je ne peux pas donner la seule chose que j'ai a vendre (Mestre Grrincha)"
    lappend messages "Tout calcul a le droit d’être présumé juste tant qu’il n’est pas déclaré faux, conformément à la loi, par un tribunal indépendant et impartial à l’issue d’un procès public et équitable. "
    lappend messages "La ou s'abat le désespoir, s'eleve la victoire des perséverants (Thomas Sankara)"
    
    
    
    set max_index [expr {[llength $messages]-1}]
    set msg [lindex $messages [myRand 0 $max_index] ]
    return $msg
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