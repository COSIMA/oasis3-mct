#  This program is under CECILL_B licence. See footer for details.

# this meta widget is meant to show a full solver
# all the next publishing will be done in a tabnotebook inside

# initialize the solver frame
proc solverframe_create { args } {
    global widgetInfo

    set mandatory_arguments { path_father }
    
    read_arguments
    
   
    ttk::notebook $path_father.s
    place $path_father.s -relx 0.0 -rely 0 -relwidth 1 -relheight 1

    return $path_father.s
}

# add a solver as  tab
proc solverframe_add { args } {
    global widgetInfo  solverPath 

    set mandatory_arguments { path_father address }
    
    initWidget
    
    
    set win $path_father.$name
    set fileroot "[file join $solverPath "XML"]"
    foreach item [lrange [split $address "."] 1 end] {
        set fileroot [file join $fileroot $item]
    }
    
    lappend widgetInfo($path_father-solvernames) $name
    lappend widgetInfo($path_father-solver-$name-valid) 1
    
    
    
    if {[dTree_attrExists $XMLtree $full_address_XML "image"]} {
        set logofile [dTree_getAttribute $XMLtree $full_address_XML "image"]
        image create photo $logofile -file [file join $fileroot $logofile]
        $path_father add [ttk::frame $win] -image $logofile -compound left -sticky news
    } else {
        $path_father add [ttk::frame $win] -compound left -text [dTree_getAttribute $XMLtree $full_address_XML "title"]
    }
    
    # tabnotebook
    set win_tn [tabs_create -path_father $win -address "root"]
    pack $win_tn -side top -fill both -expand true
    
    # clean memory if destroyed
    bind $win <Destroy> "solverframe_destroy $win"
    
    set widgetInfo($address-refreshStatus) [subst {
        solverframe_refreshStatus $win $address $name
    }]

    finishWidget
    # return the widget path for next publishing
    return $win_tn
}


# Utilities

proc solverframe_destroy {win} {
    global widgetInfo
    foreach item [array names widgetInfo "$win-*"] {
        unset widgetInfo($item)
    }
}

proc solverframe_refreshStatus {win address name} {
    global widgetInfo
     
    set status $widgetInfo($address-status)
    # update the graphical state
    
   
    switch $status {
        "-2" {
                 #$win configure -text " "
        }
        "-1" {
                #$win configure  -text "Error(s) in the set up"
        }
        "0" {
               # $win configure -text "Waiting..."
        }
        "1" {
                #$win configure -text "Set up is ready for this solver"
        }
    }
}

proc solverframe_docu {address chin help_dir} {

    global widgetInfo XMLtree solverPath    
    set full_address_XML [split $address "."]
    
     if {[dTree_attrExists $XMLtree $full_address_XML "image"]} {
        set fileroot "[file join $solverPath "XML"]"
        foreach item [lrange [split $address "."] 1 end] {
            set fileroot [file join $fileroot $item]
        }
        set logofile [dTree_getAttribute $XMLtree $full_address_XML "image"]
        set path_logofile [file join $fileroot $logofile]
        set html_image "[join $full_address_XML "_"]_$logofile"
        file copy -force $path_logofile  [file join $help_dir $html_image]
        puts $chin "<img src=\"$html_image\">"
        
    }
    
    
    set title [dTree_getAttribute $XMLtree $full_address_XML "title"]
    puts $chin "<h1> Solver $title </h1>"
    
    return
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