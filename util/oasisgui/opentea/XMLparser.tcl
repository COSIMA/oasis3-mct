#  This program is under CECILL_B licence. See footer for details.

proc XMLnewNode {command attributes icurrentNodes itree} {
    upvar $icurrentNodes currentNodes
    upvar $itree tree
   if {[llength $attributes] == 0} { # No attributes, name is then the command name
        set name $command
   } elseif {[llength $attributes] == 2} { # If only one attribute, name is the value whatever it is
        set name [lindex $attributes 1]
   } else { # Searches for node name in the attributes and node value for option command
        set name [lindex $attributes [expr [lsearch -nocase $attributes name]+1]]
   }
    
    if {$command == "include"} {set name "[join $name "_"]_include" }
    if {$command == "option"} {set name [lindex $attributes [expr [lsearch -nocase $attributes value]+1]]} 
    if {$name == ""} {error 0 "No name in the node #$currentNodes $command# with attributes : $attributes"}
    
    # Build the key of the node
    set key "root"
    foreach item $currentNodes {
        lappend key [lindex $item 1]
    }
    
    # Check uniqueness of the name
    if {[dTree_nodeExists $tree "$key $name"]} {error 0 "The node $key $name already exists"}
    dTree_addNode tree $key $name 
    
    # Updates the path in tree
    lappend currentNodes [list $command $name]
    lappend key $name
    
    #Adds the attributes
    dTree_setAttribute tree $key "nodeType" $command
    foreach {attr value} $attributes {
        dTree_setAttribute tree $key $attr $value
    }
}

proc modelNewNode {command attributes icurrentNodes itree} {
    upvar $icurrentNodes currentNodes
    upvar $itree tree
    set name $command
    
    if {$name == ""} {error 0 "No name in the node #$currentNodes $command# with attributes : $attributes"}
    
    # Build the key of the node
    set key "root"
    foreach item $currentNodes {
        lappend key [lindex $item 1]
    }
    
    # Check uniqueness of the name
    if {[dTree_nodeExists $tree "$key $name"]} {error 0 "The node $key $name already exists"}
        
    dTree_addNode tree $key $name
    # Updates the path in tree
    lappend currentNodes [list $command $name]
    lappend key $name
    
    #Adds the attributes
    
    foreach {attr value} $attributes {
        dTree_setAttribute tree $key $attr $value
    }
}

proc XMLendNode {command icurrentNodes itree} {
    upvar $icurrentNodes currentNodes
    upvar $itree tree
    
    # Controls that the end tag is correct
    set currentNode [lindex $currentNodes end 0]
    if {$currentNode != $command} {
        error 0 "end tag unexpected : expected $currentNode but detected $command, current nodes : $currentNodes"
    }
    set currentNodes [lrange $currentNodes 0 end-1]
}

proc modelEndNode {command icurrentNodes itree} {
    upvar $icurrentNodes currentNodes
    upvar $itree tree
    
    # Controls that the end tag is correct
    set currentNode [lindex $currentNodes end 0]
    if {$currentNode != $command} {
        error 0 "end tag unexpected : expected $currentNode but detected $command, current nodes : $currentNodes"
    }
    set currentNodes [lrange $currentNodes 0 end-1]
}

proc XMLaddContent {line icurrentNodes itree} {
    upvar $icurrentNodes currentNodes
    upvar $itree tree
    set line [string trim $line]
    if { $line == ""} {return 0}

    # Build the key of the node
    set key "root"
    foreach item $currentNodes {
        lappend key [lindex $item 1]
    }
    #lappend key
    

    # If no existing content, creates the XMLContent node
    if {![dict exists $tree $key "XMLContent"]} {
        dTree_setAttribute tree $key "XMLContent" "$line"
    } else {
        dTree_setAttribute tree $key "XMLContent" "[dTree_getAttribute tree $key XMLContent] $line"
    }
    
    
}

proc analyseTag {contentTag itree icurrentNodes fileType} {
    
    upvar $itree tree
    upvar $icurrentNodes currentNodes
    
    set tagType "begin"
    
    #Clean content
    set contentTag [string trim $contentTag]
    if {$contentTag == ""} {
        error "Error : one tag seems to be empty"
    }
    # Extract tag name and test if slash is present to detect type of tag
    set tagName [lindex [split $contentTag " "] 0]
    if {$tagName!=[string trimleft $tagName "/"]} {
        set tagName [string trimleft $tagName "/"]
        set tagType "end"
    }
    
    if {$tagName!=[string trimright $tagName "/"]} {
        set tagName [string trimright $tagName "/"]
        set tagType "simple"
    }
    
    
    # Extract rawAttributes
    set rawAttributes [join [lrange [split $contentTag " "] 1 end] " "]
    set rawLine $rawAttributes
    if {$rawAttributes!=[string trimright $rawAttributes "/"]} {
        set rawAttributes [string trimright $rawAttributes "/"]
        set tagType "simple"
    }

    
    # The easiest way to split attributes and take into account the possibility not to quote one word long attribute
    # is to split list of attributes using "=". The result is a list of couple : value(n) attrName(n+1) except for
    # the first one and the last one

    set rawAttributes [split $rawAttributes "="]
    set attributes ""
    set dummy 1
    foreach element $rawAttributes {  # Parsing rawAttributes to get a proper list 
        set element [string trim $element]
        
        set length1 [llength [split $element {"}]]
            
        #" This comment purpose is to correct syntaxic coloration with eclipse
            
        set length2 [llength [split $element " "]]
        if {$length1 == 1} { # No quote in element
            if {$length2 == 1} { # First param or last value
                if {$dummy == 1 || $dummy == [llength $rawAttributes]} {
                    lappend attributes [string trim $element]
                } else {
                    error "Error during parsing XML : attributes seem not to be well-formed in tag : $tagName \n The parser doesn't understand ...$element..."
                }
            }
            if {$length2 == 2} { # is |value(n) param(n+1)|
                set value [string trim [lindex $element 0]]
                set param [string trim [lindex $element end]]
                lappend attributes $value
                lappend attributes $param
                
            }
            if {$length2 > 2} { # Problem in parsing
                warning "During parsing XML : attributes seem not to be well-formed in tag : $tagName near ...$element... \n It seems that one value for an attribute contains a white space but no quotes to surround it \n The parser has tried to continue but that could lead to errors or unexpected results"
                if {$dummy == [llength $rawAttributes]} {
                    set value [string trim $element]
                    lappend attributes $value
                } elseif {$dummy == 1} {
                    error "Error during parsing XML : attributes seem not to be well-formed in tag : $tagName near $element..."
                } else {
                    set value [string trim [lrange $element 0 end-1]]
                    set param [string trim [lindex $element end]]
                    lappend attributes $value
                    lappend attributes $param
                }
            }
        } elseif {$length1 == 3} { # Two quotes in element
            set element [string trim $element]
            if {[lindex [split $element {}] 0] == {"}} { # element begins with a quote
                if {[lindex [split $element {}] end] == {"} && $dummy == [llength $rawAttributes]} { # element finish with a quote and it's the final element
                    set value [string trim [lindex [split $element {"}] 1]]
                    lappend attributes $value
                } elseif { $dummy > 1 && $dummy < [llength $rawAttributes]} { # element is made of two elements, the first one surrounded by quotes
                    set element [split $element {"}]
                    #" this comment is present to avoid editors to detect unexistant strings
                    set value [string trim [lindex $element 1]]
                    set param [string trim [lindex $element 2]]
                    lappend attributes $value
                    lappend attributes $param
                } else {
                    error "Error : During parsing XML : attributes seem not to be well-formed in tag : $tagName . It may be misplaced quotes in ...$element..."
                }
            } else {
                error "Error : During parsing XML : attributes seem not to be well-formed in tag : $tagName near ...$element... \n It may be caused by misplaced quotes"
            }
            
            
        } else {
            # It appears there are equals in the arguments, rawArgument needs to be parsed by a more specific and more restrictive parser
            AnalyseStrictRawArguments
            break
            #error "Error with quotes in tag : $tagName near ...$element..."
        }
        incr dummy
    }
    
    if {$fileType == "folderParameters"} {
        set attributes_temp $attributes
        set attributes ""
        foreach {key value} $attributes_temp {
            if {$key != "name"} {
                lappend attributes $key
                lappend attributes $value
            }
        }
        lappend attributes "name"
        lappend attributes "folderParameters"
        set fileType "XMLtree"
    }
    
    if {$fileType == "XMLtree"} {
        # Act on the tree depending on tag type
        switch $tagType {
            simple {
                XMLnewNode $tagName $attributes currentNodes tree
                XMLendNode $tagName currentNodes tree
            }
            
            begin {
                XMLnewNode $tagName $attributes currentNodes tree
            }
            
            end {
                XMLendNode $tagName currentNodes tree
            }
        }
    }
    
    if {$fileType == "DStree"} {
        switch $tagType {
            simple {
                modelNewNode $tagName $attributes currentNodes tree
                modelEndNode $tagName currentNodes tree
            }
            
            begin {
                modelNewNode $tagName $attributes currentNodes tree
            }
            
            end {
                modelEndNode $tagName currentNodes tree
            }
        }
    }
}

proc AnalyseStrictRawArguments {} {
    uplevel 1 {
        set attributes ""
        set rawAttributes [split $rawLine {"}]
            
        if {[llength $rawAttributes] % 2 == 0} {error "The parser was unable to retrieve the structure of [join $rawAttributes {"}] "} 
            
        
        for {set i 0} {$i < [expr [llength $rawAttributes]-2 ]} {incr i ; incr i} {
            set rawArg [lindex $rawAttributes $i]
            set value [lindex $rawAttributes [expr $i + 1]]
            
            set rawArg [string trim $rawArg]
            set arg [string trimright $rawArg "="]
            if {$arg == $rawArg} {error "Equal missing in XML file near $rawArg"}
            set arg [string trim $arg]
            set value [string trim $value]
            
            lappend attributes $arg
            lappend attributes $value
        }
            
    }
}



proc parseFile {fileName itree startNode fileType} {
    # This function reads an OpenTeaXML file whose format is fileType and insert it into tree under the node startNode 
    # Please note that startNode has to be a list of 2-elements lists {node type (eg. model, param, etc))  and node name }
    
    upvar $itree tree
    set currentNodes $startNode
    
    # Read XML file
    set filePointer [open [file join $fileName]]
    set XMLfile [read $filePointer]
    close $filePointer
    
    # Remove comments
    regsub -all {<!--.*?-->} $XMLfile {} XMLfile
    
    # Remove xml info
    regsub -all {<\?.*?\?>} $XMLfile {} XMLfile    
    
    # include XML files if any
    #set inclusion [lsearch -glob $XMLfile  "#!include"]
    #puts "position $inclusion "
    #set fileInclude [ lindex $XMLfile $inclusion+1]
    #set filePointer [open [file join $fileInclude ]]
    #set XMLfileInclude [read $filePointer]
    #close $filePointer
    ##set XMLfile [lrange $XMLfile 0 end]
    #set XMLfile [lreplace $XMLfile $inclusion $inclusion+1  [join $XMLfileInclude] ]
    #puts "xxx"
    #puts "$XMLfile"
    #exit
    
    # Explode XMLfile
    set XMLfile [split $XMLfile ""]
    
    # Initialize some dummy variables
    set previousChar ""
    
    # Initialize states
    set tagState "out"
    set valueState "out"
    
    # Initialize buffers
    set tagBuffer ""
    set contentBuffer ""
    
    # states descriptors
    #   - tagState
    #       "out"   : waiting for smthg to happen
    #       "in"    : insideTag
    #   - valueState
    #       "out"   : out of a string
    #       "in"    : in a string, special char are automatically escaped
    
    foreach char $XMLfile {
        switch $tagState {
            "out" {
                if {$char=="<" && $previousChar!="\\"} { # Entering a tag
                    set tagState "in"
                    set tagBuffer ""
                    XMLaddContent $contentBuffer currentNodes tree
                    set contentBuffer ""
                } else { # Still out a tag
                    if {$char == " " && $previousChar == " "} {
                        # don't store character
                    } elseif {$char == "\t"} {
                        # don't store character et keep the previousChar
                        set char $previousChar
                    } elseif {$previousChar != "\\" && $char == "\\"} {
                        # don't store
                    } elseif {$char == "\n" && $previousChar != "\n"} {
                        #don't store
                        append contentBuffer " "
                    } else {
                        append contentBuffer $char
                    }
                }
            }
            "in" {
                if {$char==">" && $previousChar!="\\" && $valueState=="out"} { # Leaving a tag
                    set tagState "out"
                    # Tag is now well-known, it is send to a special procedure 
                    analyseTag $tagBuffer tree currentNodes $fileType
                } else { # Still in a tag
                    append tagBuffer $char
                }
            }
            
        }
        
        switch $valueState {
            "out" {
                if {$char=="\"" && $previousChar!="\\"} { # Entering a tag
                    set valueState "in"
                }
            }
            "in" {
                if {$char=="\"" && $previousChar!="\\"} {
                    set valueState "out"
                }
                }
            
        }
        set previousChar $char
    }
    
    # At the end currentNodes has to be equal to startNode, otherwise, it means that end tags are missing in the XML file
    if {$currentNodes != $startNode} {
        set msg_err "Error in the XML file $fileName : closing tag is missing for node $currentNodes "
        popup_error $msg_err strict
    }
    
    # At the end of the file, valueState has to be equal to out. If not, that means a tag isn't closed properly
    if {$valueState == "in"} {
        set msg_err "Error : in the file $fileName, a tag is not properly closed (Normally, this one : [lindex [split $tagBuffer { }] 0])"
        popup_error $msg_err strict
    }
}


proc dir2tree {modelPath startNode itree} {
    # This function create a subtree of the path "modelPath" and add it to the tree itree under the node startNode
    # Please note that the node has to be a list of the ancestors (without "root")
    upvar $itree tree
    set currentPath [file join $modelPath [join $startNode [file separator]]]
    set files [lsort [glob -nocomplain -tails -directory $currentPath *]]
    
    foreach fileid $files {
        # fileid is a directory
        if {[lindex [split $fileid ""] 0] == "."} {
            continue
        }
        if {[file isdirectory [file join $currentPath $fileid]]} {
            if { [glob -nocomplain -tails -directory [file join $currentPath $fileid] *.xml] == "" && [glob -nocomplain -directory [file join $currentPath $fileid] -type d *] == "" } {
                continue
            }
            set currentNode [concat $startNode $fileid]
            # Adds this directory to the tree
            dTree_addNode tree [concat "root" $startNode] $fileid
            dTree_setAttribute tree [concat "root" $startNode $fileid] nodeType "folder"
            dTree_setAttribute tree [concat "root" $startNode $fileid] name "$fileid"            
            
            # reads the directory
            dir2tree $modelPath $currentNode tree
            
        } else {
            if {[file extension [file join $currentPath $fileid]] == ".xml" } {
                # fileid is a XML file
                set startNodeXML ""
            
                # Translates the format of the node from dir2tree format to file2tree format
                foreach value $startNode {
                    lappend startNodeXML [list "directory" $value]
                }
                
                if {[file tail [file join $currentPath $fileid]] == "folderParameters.xml"} {
                    parseFile [file join $currentPath $fileid] tree $startNodeXML "folderParameters"
                } else {
                    if {[lindex [lindex $startNodeXML end] end] == "DATA" && "1"=="0"} {
                        parseFile [file join $currentPath $fileid] tree $startNodeXML "DStree"                        
                    } else {
                        parseFile [file join $currentPath $fileid] tree $startNodeXML "XMLtree"
                    }
                }
            }
        }
    }
    
}


proc OpenTeaXML2tree {itree modelPath dataPath} {
    upvar $itree tree
    dTree_init tree

    # check if modelPath exists
    if {![file isdirectory $modelPath]} {
        error "The folder containing the XML files cannot be found. Should be : $modelPath"
    }

    # add solver directory
    dir2tree $modelPath "" tree
			    
    # add DATA node
    dTree_addNode tree "root" DATA
    dTree_setAttribute tree "root DATA" nodeType "folder"
    dir2tree [file join $dataPath] DATA tree
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