#  This program is under CECILL_B licence. See footer for details.

proc generate_help_html { } {
    global XMLtree  help_name help_rootdir help_level
    
    set msgOpen "Choose location to save help document (images saved in the same folder)"
    
    set help_dir [file join [file normalize $help_rootdir ] "help_openTea_$help_name"]
    
    file delete -force $help_dir
    file mkdir $help_dir
    
    set help_file [file join $help_dir "help_openTea_$help_name.html"]
    
    

    
    
    # gradient colors
    set col1 "#c1c1c1"
    set col0 "#ffffff"
    
    # theme color
    set colA "#6E6E6E"
    set colB "#0B610B"
    
    set dir "left"
    switch $dir {
        "top" {
            set dir_webkit "0% 0%,0% 100%"
        }
        "left" {
            set dir_webkit "100% 0%,0% 0%"
        }
    }
    
    
    debug "Generating help in file $help_file"
    
    
    set chin [ open $help_file w ]
    puts $chin "<html>"
    puts $chin "<body>"
    puts $chin "<head>"
    puts $chin "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />"
    puts $chin "<style>"
    
    
    
    puts $chin  "html \{ min-height:100%; \} /* to get the gradient to stetch to the bottom of the view port */"
    puts $chin "body \{"
    puts $chin "	background: $col1;"
    puts $chin "	background: -moz-linear-gradient($dir, $col0 0%, $col1 100%), url(img.png) no-repeat 50% 300px;"
    puts $chin "	background: -webkit-gradient(linear, $dir_webkit, from($col0), to($col1)), url(img.png) no-repeat 50% 300px;"
    puts $chin "	background: -webkit-linear-gradient($dir, $col0 0%,$col1 100%), url(img.png) no-repeat 50% 300px;"
    puts $chin "	background: -o-linear-gradient($dir, $col0 0%,$col1 100%), url(img.png) no-repeat 50% 300px;"
    puts $chin "	background: -ms-linear-gradient($dir, $col0 0%,$col1 100%), url(img.png) no-repeat 50% 300px;"
    puts $chin "	background: linear-gradient($dir, $col0 0%,$col1 100%), url(img.png) no-repeat 50% 300px;"
   
    puts $chin "        margin-top:100px;"
    puts $chin "        margin-bottom:100px;"
    puts $chin "        margin-right:50px;"
    puts $chin "        margin-left:50px;"
    #puts $chin "        font-family:\"Verdana\";"
    
    
    puts $chin "\}"
    
    puts $chin "h1 \{background-color:$colA;color:white;\}"
    puts $chin "h2 \{background-color:$colB;color:white;\}"
    
  
    puts $chin "p \{"
    puts $chin "padding-left:50px;"
    puts $chin "\}"
    
    puts $chin "div.stage1 \{padding-left:40px;\}"
    puts $chin "div.stage2 \{padding-left:30px;\}"
    puts $chin "div.stage3 \{padding-left:20px;\}"
    puts $chin "div.stage4 \{padding-left:10px;\}"
    puts $chin "div.stage5 \{padding-left:10px;\}"
    puts $chin "div.stage6 \{padding-left:10px;\}"
    puts $chin "div.stage7 \{padding-left:10px;\}"
    puts $chin "div.stage8 \{padding-left:10px;\}"
    
   
    
    
    puts $chin "div.multiple"
    puts $chin "\{"
    puts $chin "padding:10px;"
    puts $chin "border:5px solid $colA;"
    puts $chin "margin:0px;"
    puts $chin "\}"
    puts $chin "div.xor"
    puts $chin "\{"
    puts $chin "padding:10px;"
    puts $chin "border:5px solid $colB;"
    puts $chin "margin:0px;"
    puts $chin "\}"

    puts $chin "div.inxor"
    puts $chin "\{"
    puts $chin "padding:20px;"
    puts $chin "background:white;"
    puts $chin "border:1px solid $colB;"
    puts $chin "margin:0px;"
    puts $chin "\}"

    puts $chin "div.choice"
    puts $chin "\{"
    puts $chin "padding:10px;"
    puts $chin "border:1px solid $colA;"
    puts $chin "margin:0px;"
    puts $chin "\}"
    
    
    puts $chin "div.model"
    puts $chin "\{"
    puts $chin "background:white;"
    puts $chin "border:1px solid $col1;"
    puts $chin "padding:10px;"
    puts $chin "border:Opx ;"
    puts $chin "margin:0px;"
    puts $chin "\}"
    
    
    puts $chin "</style>"
    puts $chin "</head>"
    
    # remplissage du fichier d'aide
   
    
    
    foreach solv [dTree_getChildren $XMLtree "root"] {
        if {$solv == "DATA"} {
            continue
        }
        
        iterative_help root $solv solver $chin $help_dir 0 "root"
        
    }
    
   
    
    # fermeture du fichioer d'aide
    puts $chin "</html>"
    puts $chin "</body>"
    
    close $chin
}

proc iterative_help {path name part_class help_channel help_dir stage parent_class} {
    global  XMLtree help_level
    set address "$path.$name"
     set full_address_XML [split $address "."]
    
    incr stage
    
    puts $help_channel "<div class=\"stage$stage\">"
    
    set decorator ""
    set header ""
    
    if {$help_level >= 3} {
        set header "($part_class:$name)"
    }
    
    set title ""
    if {[dTree_attrExists $XMLtree $full_address_XML "title"]} {
        set title [dTree_getAttribute $XMLtree $full_address_XML "title"]
    }
    set default ""
    if {[dTree_attrExists $XMLtree $full_address_XML "default"]} {
        set default [dTree_getAttribute $XMLtree $full_address_XML "default"]
    }    
    set type ""
    if {[dTree_attrExists $XMLtree $full_address_XML "type"]} {
        set type [dTree_getAttribute $XMLtree $full_address_XML "type"]
    }    
    set require ""
    if {[dTree_attrExists $XMLtree $full_address_XML "require"]} {
        set require [dTree_getAttribute $XMLtree $full_address_XML "require"]
    }    
    set existif ""
    if {[dTree_attrExists $XMLtree $full_address_XML "existif"]} {
        set existif [dTree_getAttribute $XMLtree $full_address_XML "existif"]
    }
    set visibility ""
    if {[dTree_attrExists $XMLtree $full_address_XML "visibility"]} {
        set visibility [dTree_getAttribute $XMLtree $full_address_XML "visibility"]
    }
    set script ""
    if {[dTree_attrExists $XMLtree $full_address_XML "script"]} {
        set script [dTree_getAttribute $XMLtree $full_address_XML "script"]
    }   
    
     
    switch $part_class {
        "solver" {
            
            solverframe_docu $address $help_channel $help_dir
            
         }
        "tab" {
            tabs_docu $address $help_channel $help_dir
            set header "$header [container_docu $address  $help_dir]"
            
        } 
        "model" {
            set decorator "model"
            if {$parent_class=="xor"} {
                set decorator "inxor"
            }
            if {$title !=""} {
                set header "$header <b> $title </b> <br>"
            }
            if {$help_level >= 3} {
                if {$existif !=""} {
                    set header "$header <font color=red> !!! This model is existing only if the following condition is verified : $existif !!! </font> <br>"
                }
            }
            
            set header "$header [container_docu $address  $help_dir]"
            
        }
        "multiple" {
            set decorator "multiple"
             if {$title !=""} {
                set header "$header <b> $title </b><br>"
            }
           
            set header "$header MULTIPLE. Each item is composed as : "
            
            if {$help_level >= 3} {
                if {$require!= ""} {
                    set header "$header \n This multiple is depending on the value of element <b> $require </b>."
                } else {
                    set header "$header \nThis multiple is standalone : the user can add/delete items." 
                }
            }
        }
        "container" {}
        "action" -
        "info" -
        "choice" -
        "option" -
        "param" {
            if {$help_level >= 2} {
                set header "$header <li>"
                
                
                if {$title !=""} {
                    set header "$header <b>  $title .</b> "
                } else {
                    set header "$header <b>  $name </b> "
                }
                
                if {$part_class == "choice"} {
                    set header "$header <b> choice among : </b> <br>"
                    set decorator "choice"
                }
                
                
                if {$part_class == "info"} {
                    set header "$header information "
                    set decorator "choice"
                }
                
                
                if {$type !=""} {
                    switch -glob $type {
                        "integer*" {
                            set header "$header Integer value."
                        }
                        "list_integer*" {
                            set header "$header List of integers."
                        }
                        "double*" {
                            set header "$header Real value."
                        }
                        "list_double*" {
                            set header "$header List of values."
                        }
                        "fraction*" {
                            set header "$header Real value between 0 and 1."
                        }
                        "list_fraction*" {
                            set header "$header Real value between 0 and 1."
                        }
                        "complex*" {
                            set header "$header Complex value."
                        }
                        "string*" {
                            set header "$header string of characters."
                        }
                        "file*" {
                            set header "$header File."
                        }
                        "avbpfile*" {
                            set header "$header AVBP HDF5 mesh file (mymesh.mesh.h5)."
                        }
                        "folder*" {
                            set header "$header Folder."
                        }
                        "default" {
                            set header "$header $type."   
                        }
                    }
                    
                }
                
                if {$default !=""} {
                    set header "$header default: $default. <br>"
                }
                if {$help_level >= 3} {
                    set header "$header type: $type. <br>"
                    
                    if {$visibility =="hidden"} {
                        set header "$header (hidden)<br>"
                    }
                    if {$script !=""} {
                        set header "$header for script <b>$script</b>.<br>"
                    }
                }
                set header "$header </li>"
            
            }
           
        }
        "xor" {
            set decorator "xor"
            if {$title !=""} {
                set header "$header <b> Exclusive Selection $title</b> <br>"
            }
            set header "XOR - Exclusive choice among :"
            
            if {$help_level >= 3} {    
                if {$default !=""} {
                    set header "$header The Default  value for this exclusive selection Default is $default <br>"
                }
            }
        }
        default {
            puts $help_channel "<p> $part_class $name</p>"
        }
    }
   
   
   
    set full_address_XML [split $address "." ]
    
    ######## Sort the children according to their "order" attribute
    set unsorted_children [dTree_getChildren $XMLtree $full_address_XML]
    set sortedChildren ""
    set childrenAndOrder ""
    # First, get children and associated orders
    foreach child $unsorted_children {
        set order "0"
        set childAndOrder ""
        # Retrieve order from attributes of the child
        catch {set order [dTree_getAttribute $XMLtree "$full_address_XML $child" order]}
        lappend childAndOrder $order
        lappend childAndOrder $child
        lappend childrenAndOrder $childAndOrder
    }
    # Then, sort
    set childrenAndOrder [lsort -index 0 -integer $childrenAndOrder]
    # Finally, store them
    foreach child $childrenAndOrder {
        lappend sortedChildren [lindex $child 1]
    }
    
   
   
    if { $part_class != "multiple" } {   
        set full_address_XML [split $address "."]
        if {$decorator != ""} {
            puts $help_channel "<div class=\"$decorator\">"    
            }
        if {$header != ""} {
            puts $help_channel " $header <br>"    
        }
        foreach child $sortedChildren {
            # Case of ";" type nodes (options)
            if {[string match "*;*" $child]} {
                set XML_node_address_child "$full_address_XML \{$child\}"
            } else {
                set XML_node_address_child [concat $full_address_XML $child]
            }
            if {[catch {set widget_child_class "[dTree_getAttribute $XMLtree $XML_node_address_child nodeType]"}]} {
                warning "WARNING : $XML_node_address_child doesn't have a node type ...\n Setting it to container ..."
                set widget_child_class "container"
            }
            
            iterative_help $address $child  $widget_child_class $help_channel $help_dir $stage $part_class
        }
        if {$decorator != ""} {
            puts $help_channel "</div><br>"
        }
        
    } else {
        set widget_child_class "container"
        if {$decorator != ""} {
            puts $help_channel "<div class=\"$decorator\">"    
        }
        if {$header != ""} {
            puts $help_channel " $header  <br>"    
        }
        iterative_help $address item  $widget_child_class $help_channel $help_dir $stage parent_class
        if {$decorator != ""} {
            puts $help_channel "</div><br>"
        }
    }
    
    puts $help_channel "</div>"
}






#####################
# HELP Creation
#####################

proc CreateHelpText { win address} {
    global ContainerHelpInfo
    
    destroy $win.text
    destroy $win.sbary
    
    text $win.text  -foreground [ThemeColor 0.]  -background [ThemeColor 1.1] -highlightthickness 0 -yscrollcommand [list $win.sbary set]
    
    ttk::scrollbar $win.sbary -orient vertical -command [list $win.text yview]
    
     
    
    $win.text insert end "\n    $ContainerHelpInfo($address-title) \n" "Title"
    ShowHelpElement $win.text $ContainerHelpInfo($address-docu) $address help_text    
    
    $win.text configure -wrap word
    $win.text tag configure "Body" -font "helvetica 12 " -lmargin1 40 -lmargin2 30 -rmargin 30 -foreground [ThemeColor 0.50]
    $win.text tag configure "Title" -font "helvetica 16 bold" -justify center -foreground [ThemeColor 0.80]
    $win.text tag configure "Section" -font "helvetica 12 bold"  -lmargin1 40 -lmargin2 30 -rmargin 30  -foreground [ThemeColor 0.65]
    
    $win.text tag configure "Float" -font "helvetica 12 bold" -justify center -foreground [ThemeColor 0.50]
    
    $win.text tag configure "Item" -font "helvetica 12 " -lmargin1 70 -lmargin2 30 -rmargin 30 -foreground [ThemeColor 0.50]
    
    $win.text configure -state disabled
    
    place  $win.text -relx 0.1 -rely 0.1 -relwidth 0.8 -relheight 0.8
    place  $win.sbary -relx 0.9 -rely 0.1  -relheight 0.8 -anchor ne
   
    return $win.text
}


# a parser to present the help with pictures 
proc ShowHelpElement { win helptext address textOrlabel {help_dir dummy}} {
    global  widgetInfo pathEngine solverPath
    
    set html_help ""
    
    set fileroot [file join $solverPath "XML"]
    foreach item [lrange [split $address "."] 1 end-1] {
        set fileroot [file join $fileroot $item]
    }
    
   
    # Searching for corresponding directory
    while {![file exists $fileroot] && $fileroot != [file dirname $fileroot]} {
        set fileroot [file dirname $fileroot]
        
    }
    
    set index 0    
    set newline ""
    set bit ""

    # Convert html codes into plain text (e.g. "&gt;" => ">")
    set helptext [string map -nocase {
    "&lt;"      "<"
    "&gt;"      ">"
    "&le;"      "<="
    "&ge;"      ">="
    "&eacute"   "é"
    } $helptext]

    
    foreach char [split $helptext ""] {
        switch $char {
            "[" {
                # print last arm of text
                
                set newline [string trim $newline$bit]
                
                
                if {$newline != ""} {
                    switch $textOrlabel {
                        help_text {
                            $win insert end "$newline \n" "Body"
                        }
                        desc_label {
                            incr index
                            ttk::label $win.item$index -text $newline -wraplength $widgetInfo(guiSmallWidgetWidth) -foreground [ThemeColor 0.20]
                            pack $win.item$index -side top
                        }
                        help_html {
                            set html_help "$html_help <p> $newline </p> <br>"
                        }
                    }
                }
                # clean the buffer
                set bit ""
                set newline ""
            }
            
            "]" {
                # special items
                set newline "$newline$bit"
                foreach item [split $newline ";"] {
                    set key [lindex [split $item "="] 0 ]
                    set value [string trim [lindex [split $item "="] 1]]
                    switch $key {
                        "logo" {
                            switch $textOrlabel {
                                help_text {
                                    $win insert end " " "Float"
                                    $win image create end -image icon_$value 
                                    $win insert end "\n"
                                }
                                desc_label {
                                    incr index
                                    ttk::label $win.item$index -image icon_$value
                                    pack $win.item$index -side top
                                }
                                help_html {
                                    set html_image "[join [split $address "."] "_"]_$value"
                                    file copy -force [file join $pathEngine IMAGES logo_$value.gif]  [file join $help_dir $html_image]
                                    set html_help "$html_help <div id=\"wrapper\" style=\"width:100%; text-align:center\">"
                                    set html_help "$html_help <img src=\"$html_image\"; style=\"margin:auto;\">"
                                    set html_help "$html_help </div> <br>"
                                    
                                }
                            }
                        }
                        "image" {
                            if {[file exists [file join $fileroot $value]] == 0} {
                                log "Error : file [file join $fileroot $value] doesn't exist, skipping ..."
                            } else {
                                image create photo $value -file [file join $fileroot $value]
                                switch $textOrlabel {
                                    help_text {
                                        $win insert end " " "Float"
                                        $win image create end -image $value 
                                        $win insert end "\n"
                                    }
                                    desc_label {
                                        incr index
                                        ttk::label $win.item$index -image $value 
                                        pack $win.item$index -side top
                                    }
                                    help_html {
                                        set html_image "[join [split $address "."] "_"]_$value"
                                        file copy -force [file join $fileroot $value]  [file join $help_dir $html_image]
                                        set html_help "$html_help <div id=\"wrapper\" style=\"width:100%; text-align:center\">"
                                        set html_help "$html_help <img src=\"$html_image\"; style=\"margin:auto;\">"
                                        set html_help "$html_help </div> <br>"
                                        
                                    }
                                }
                            }
                        }
                        "caption" {
                            switch $textOrlabel {
                                help_text {
                                    $win insert end "$value \n" "Float"
                                }
                                desc_label {
                                    incr index
                                    ttk::label $win.item$index -text $value -foreground [ThemeColor 0.20] -wraplength $widgetInfo(guiSmallWidgetWidth)
                                    pack $win.item$index -side top
                                }
                                help_html {
                                    set html_help "$html_help <div id=\"wrapper\" style=\"width:100%; text-align:center\">"
                                    set html_help "$html_help <p> $value </p>"
                                    set html_help "$html_help </div> <br>"
                                }
                            }
                        }
                        "section" {
                            switch $textOrlabel {
                                help_text {
                                    $win insert end "\n" "Body"
                                    $win insert end "$value \n" "Section"
                                }
                                desc_label {
                                    incr index
                                    ttk::separator $win.item$index -orient horizontal
                                    pack $win.item$index -side top -fill both -pady {3 0}
                                    incr index
                                    ttk::label $win.item$index -text $value -foreground [ThemeColor 0.40] -wraplength $widgetInfo(guiSmallWidgetWidth) 
                                    pack $win.item$index -side top -anchor w
                                }
                                help_html {
                                    set html_help "$html_help <br> <p> $value </p> <hr> <br>"
                                }
                            }
                        }
                        "item" {
                            switch $textOrlabel {
                                help_text {
                                    $win insert end "- $value\n" "Item"
                                }
                                desc_label {
                                    incr index
                                    ttk::label $win.item$index -text "- $value" -foreground [ThemeColor 0.20] -wraplength $widgetInfo(guiSmallWidgetWidth) 
                                    pack $win.item$index -side top -anchor w -padx {10 0}
                                }
                                help_html {
                                    set html_help "$html_help  &#8226; $value  <br>"
                                }
                                
                            }
                        }
                        "subitem" {
                            switch $textOrlabel {
                                help_text {
                                    $win insert end "              - $value\n" "Item"
                                }
                                desc_label {
                                    incr index
                                    ttk::label $win.item$index -text "- $value" -foreground [ThemeColor 0.20] -wraplength $widgetInfo(guiSmallWidgetWidth) 
                                    pack $win.item$index -side top -anchor w -padx {20 0}
                                }
                                help_html {
                                    set html_help "$html_help  &#160 &#160 &#8211; $value  <br>"
                                }
                                
                            }
                        }
                    }
                }
                
                set bit ""
                set newline ""
            }
            default {
                # capture des accents pour corrections
                #puts $char
                #if {$char == ""} { puts "jj $char" }  # do  not work
                #if {$textOrlabel == "help_html"} {
                #    switch $char {
                #        "" {
                #            debug " switching "
                #            set char "&eacute;"
                #        }
                #        default {
                #            
                #        }
                #    }
                #}
                set bit "$bit$char"
            }
        }
    }
    # last arm of text
    set newline "$newline$bit"
    switch $textOrlabel {
        help_text {
            $win insert end "$newline \n" "Body"
        }
        desc_label {
            incr index
            ttk::label $win.item$index -text $newline -wraplength $widgetInfo(guiSmallWidgetWidth) -foreground [ThemeColor 0.20]
            pack $win.item$index -side top 
        }
        help_html {
            set html_help "$html_help <p> $newline </p>"
        }
    }
    
    return $html_help

}


# documentation

proc container_docu {address  help_dir} {

    global widgetInfo XMLtree modelPath  help_level
    set full_address_XML [split $address "."]
    
    #set fileroot "[file join $modelPath]"
    #foreach item [lrange [split $address "."] 1 end] {
    #    set fileroot [file join $fileroot $item]
    #}
    #set logofile [dTree_getAttribute $XMLtree $full_address_XML "image"]
    #set path_logofile [file join $fileroot $logofile]
    #set html_image "[join $full_address_XML "_"]_$logofile"
    #file copy -force $path_logofile  [file join $help_dir $html_image]
    #debug [file join $help_dir $html_image]
    #puts $chin "<img src=\"$html_image\">"

    set header ""
    
    if {$help_level >= 1} {
        if {![catch {set desc [dTree_getAttribute $XMLtree "$full_address_XML" "desc"]}]} {   
           #puts $chin "<h5> Description </h5>"
           set header "$header [ShowHelpElement dummy $desc $address help_html $help_dir]"
       }
       if {![catch {set docu [dTree_getAttribute $XMLtree "$full_address_XML" "docu"]}]} {   
           #puts $chin "<h5> Documentation </h5>"
           set header "$header [ShowHelpElement dummy $docu $address help_html $help_dir]"
       }
    }
    
    return $header
}


proc help_add_desc_docu_to_widget {  } {
    uplevel 1 {} {
        
        global ContainerHelpInfo widgetInfo
        
        
        set windoc $docu_prefered_place
        set ContainerHelpInfo($address-title) [dTree_getAttribute $XMLtree $full_address_XML_clean "title"]
        #add desc if needed
        if {![catch {set desc [dTree_getAttribute $XMLtree "$full_address_XML" "desc"]}]} {   
            ttk::frame $windoc.desc
            pack $windoc.desc -side $docu_prefered_side -anchor w -fill x
            ShowHelpElement $windoc.desc $desc $address desc_label
    
        }
        
        #add help if needed
        if {![catch {set docu [dTree_getAttribute $XMLtree "$full_address_XML" "docu"]}]} {
            set ContainerHelpInfo($address-docu) $docu
            #add help
            ttk::label $windoc.help -text "Learn more..." -image icon_info -compound right 
        #    place $win.help -anchor se -relx 1 -rely 1 -width 20 -height 20 
            pack $windoc.help -anchor se -side $docu_prefered_side 
        
            bind $windoc.help <ButtonPress> [subst {
                set win \[ CreateSplashScreen \]
                CreateHelpText \$win $address
            }]
        } else {    
            set ContainerHelpInfo($address-docu) ""
        }
       
       
       # add the unpackme method for help and desc if needed
       set widgetInfo(unpackme-$windoc.desc) [subst {
            pack forget $windoc.desc
       }]
       set widgetInfo(unpackme-$windoc.help) [subst {
            pack forget $windoc.help
       }]
       
        
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