#  This program is under CECILL_B licence. See footer for details.




proc loadProject {fileName } {
    global widgetInfo loadedProject workingDir
    global DStree metaTree 
    
    
    # start timer
    set load_start_time [clock milliseconds]
    
    
    # load projet into a loadtree
    dTree_init loadTree
    
    
   
    
    xml2tree $fileName loadTree "DStree"    
    set loadedProject $fileName
    updateTitle
    cd [file dirname $loadedProject]
    # If it doesn't exist, create directory according to file name
    set dirName [file tail [file rootname $fileName]]
    if {$widgetInfo(classApp) != "nanoapp"} {
        if {![file exists $dirName]} {
            file mkdir $dirName
        }
        cd $dirName
    }           
    set workingDir [pwd]             

  
    startup_log_channel
    
    
    # Moving meta into metaTree
    # Moving dataset into DStree
    dTree_init DStree
    
    if {[dTree_nodeExists $loadTree "root dataset"] && [dTree_nodeExists $loadTree "root dataset meta"]} {
        dTree_init metaTree        
        dTree_moveBranch loadTree "root dataset meta" metaTree "root"
        foreach child [dTree_getChildren $loadTree "root dataset"] {
            dTree_moveBranch loadTree "root dataset $child" DStree "root"
        }
    }
     
   
    RefreshFamily "root"
    set nb_of_validations 1
    for {set x 0} {$x < $nb_of_validations } {incr x} {
        CheckValidationStatus "root"
    }
        
    # update Meta data
    dTree_setAttribute metaTree "root meta project address" "value" "[file normalize $fileName]"        
    dTree_setAttribute metaTree "root meta project name" "value" "[file tail [file rootname $fileName]]"     
    
    # monitor time
    set load_stop_time [clock milliseconds]
    set msgloadtime "Loading time : [printtime $load_start_time $load_stop_time ] s."
    puts $msgloadtime
    log $msgloadtime
   
}


proc startup_log_channel {} {
    global workingDir log_channel loadedProject loadApplication

    if {$log_channel != "stdout"} {
        close $log_channel
    }
    set log_channel [ open [file join $workingDir "trace.log"] a+ ]
    
    fconfigure $log_channel -blocking 0 
    
    puts $log_channel  "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
    puts $log_channel  "Application $loadApplication"
    puts $log_channel  "Project  $loadedProject"
    puts $log_channel  "Date   [clock format [clock seconds]]"
    puts $log_channel  "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
    
   
}

proc loadProject_as_part {fileName } {
    global widgetInfo loadedProject workingDir
    global DStree metaTree
    
    Start_think ""
    # start timer
    set load_start_time [clock milliseconds]
    
    
    # load projet into a loadtree
    dTree_init loadTree
    
    
    ####################################
    # fork btw CSV and XML , deprecated
    # set fileExtension [string tolower [file extension $fileName]]    
    #if  $fileExtension == ".xml"
    ####################################
    
    
    xml2tree $fileName loadTree "DStree"    
    
    
    
    
    set ProjectToAdd $fileName
    updateTitle
    set filesToCopy [glob -nocomplain -directory [file rootname $ProjectToAdd] "*"]
    
    foreach subfile $filesToCopy {
        debug "Copying  $subfile in $workingDir ..."
        if {[catch { file copy -force $subfile $workingDir } pbcopy ]} {
            warning "$pbcopy"
        }
    }
    
    ####################################
    # fork btw CSV and XML , deprecated
    #if  $fileExtension == ".xml"
    #elseif $fileExtension == ".csv"
    #if $loadedProject == "none"
    #    error "An xml project needs to be first loaded"
    #    return   
    #csv2tree $fileName loadTree
    ####################################
    
    
    
    # Moving meta into metaTree
    # Moving dataset into dummy tree
    dTree_init dummyTree    
    if {[dTree_nodeExists $loadTree "root dataset"] } {
        foreach child [dTree_getChildren $loadTree "root dataset"] {
            dTree_moveBranch loadTree "root dataset $child" dummyTree "root"
        }
    }
    
    
    
    
    # compare the present tree with the loaded tree
    set refreshList ""
    set comparisonList ""
    dTree_compareBranch $DStree $dummyTree "root" 0 comparisonList
    # Merging first and fourth list 
    set modificationList [lsort -unique [concat [lindex $comparisonList 0] [lindex $comparisonList 1]]]
    
    
    # Applying differences
    foreach addr [lsort -unique [lindex $modificationList]] {
        debug ">>> Grafting branch on current memory : [lindex $addr end] ..."
        
        dTree_copyBranch dummyTree $addr DStree [lrange $addr 0 end-1]
    }
    RefreshFamily "root"
    CheckValidationStatus "root"
    
    
    # monitor time
    set load_stop_time [clock milliseconds]
    set msgloadtime "Loading time : [printtime $load_start_time $load_stop_time ] s."
    puts $msgloadtime
    log $msgloadtime
    Stop_think normal
   
}









proc loadDataset {fileName} {
    global DStree
    global metaTree
    global tmpTree
    
    dTree_init loadTree
    parseFile $fileName loadTree "" "DStree"
    
    dTree_init metaTree
    dTree_rmBranch loadTree "root dataset meta config"
    dTree_moveBranch loadTree "root dataset meta" metaTree "root"    
    
    #dTree_rmBranch loadTree "root dataset DATA"
    
    dTree_init dummyTree
    foreach child [dTree_getChildren $loadTree "root dataset"] {
        dTree_moveBranch loadTree "root dataset $child" dummyTree "root"
    }    
    
    
    set refreshList ""
    
    set comparisonList ""
    dTree_compareBranch $tmpTree $dummyTree "root" 1 comparisonList
    
    # Merging first and fourth list 
    set modificationList [lsort -unique  [concat [lindex $comparisonList 0] [lindex $comparisonList 3]]]  
    
    
    # Detecting if all the tree is to be copied 
    if {[lsearch -exact $modificationList "root"] != -1} {
        set DStree $dummyTree
        RefreshFamily "root"
        for {set x 0} {$x < $::NUMBER_OF_VALIDATION } {incr x} {
            CheckValidationStatus "root"
        }           
        return
    }

    # Applying Differences
    foreach addr  $modificationList {
        dTree_copyBranch dummyTree $addr DStree [lrange $addr 0 end-1]
    }
    
    # We need to reconstruct interface (multiple which depends on an unknown list have to be initialized)
    foreach a  $modificationList {    
        set addr [tree2gui $a]
        RefreshFamily "$addr"
        CheckValidationStatus "$addr"
    }    
    
}





proc saveProject { where filetype mode } {
    # "where" : self or filename, for save as or save
    # "filetype" : csv and xml
    # "mode" : all , green only
    
    
    global DStree metaTree tmpTree
    global loadApplication loadedProject widgetInfo workingDir log_channel
    
    
    
    # build saveTree
    set saveTree ""
    dTree_init saveTree
    dTree_addNode saveTree "root" "dataset"
    switch $mode {
        "all" {
            foreach child [dTree_getChildren $tmpTree "root"] {
                dTree_copyBranch tmpTree "root $child" saveTree "root dataset"
            }
        }
        "greenonly" {
            foreach child [dTree_getChildren $tmpTree "root"] {
                dTree_copyBranch DStree "root $child" saveTree "root dataset"
            }
        }
        default {
            error "saveProject mode $mode not understood"
        }
    }
    
    
   switch $where {
        "self" {
            set fileName $loadedProject
            log "Saving Project in file $fileName "
        }
        default {
            set fileName $where
            log "Saving Project As file $fileName "
        }
    }
   
   
    
       
    
    # re-setting the projet   
    #newRecentProject $fileName $loadApplication
    set loadedProject $fileName  
    updateTitle            
    cd [file dirname $loadedProject]
    # If it doesn't exist, we create directory according to file name
    set dirName [file tail [file rootname $fileName]]
    if {$widgetInfo(classApp) != "nanoapp" } {
        if {![file exists $dirName]} {
            file mkdir $dirName
        }
        cd $dirName
    }                

    dTree_setAttribute metaTree "root meta project name" "value" "[file tail [file rootname $fileName]]"        
    dTree_setAttribute metaTree "root meta project address" "value" "[file normalize $fileName]"
    dTree_copyBranch metaTree "root meta" saveTree "root dataset"
   
    set workingDir [pwd]
    
    
    switch $where {
        "self" {
            flush $log_channel
        }
        default {
            startup_log_channel
        }
    }
    
    
    # actual saving
    switch $filetype {
        "xml" {
            tree2xml $fileName $saveTree
        }
        "csv" {
            tree2csv $fileName $saveTree
        }
        default {
             error "saveProject filetype $filetype not understood"
        }
    }
}

proc saveDataset {file callingAddress} {
    global metaTree configTree tmpTree XMLtree loadedProject
    global configPath
    fill_metaTree
    set pipeTree ""
    dTree_init pipeTree
    dTree_addNode pipeTree "root" "dataset"
    dTree_setAttribute metaTree "root meta project name" "value" "[file tail [file rootname $loadedProject]]"        
    dTree_setAttribute metaTree "root meta project address" "value" "[file normalize $loadedProject]"
    dTree_copyBranch metaTree "root meta" pipeTree "root dataset"
    
    dTree_setAttribute pipeTree "root dataset meta scriptSuccess" "value" 1
    dTree_setAttribute pipeTree "root dataset meta action callingAddress" "value" $callingAddress
    
    
    # Adding DATA
    #dTree_copyBranch XMLtree "root DATA" pipeTree "root dataset"
    
    
    # Adding extra-informations
    
    dTree_init configTree
    if {[file exists $configPath]} {
        parseFile $configPath configTree "" "DStree"
        dTree_copyBranch configTree "root dataset config" pipeTree "root dataset meta"
    } else {
        close [open $configPath "w"]
    }    
    
    
    foreach child [dTree_getChildren $tmpTree "root"] {
        dTree_copyRelevantBranch tmpTree "root $child" pipeTree "root dataset"           
    }
    
    tree2xml $file $pipeTree    
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