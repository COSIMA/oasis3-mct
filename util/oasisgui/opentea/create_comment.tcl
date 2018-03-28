#  This program is under CECILL_B licence. See footer for details.



# COMMENT CREATION

proc comment_create { args } {
    set mandatory_arguments { path_father address }
    
    
    
    # Initializes the widget
    initWidget  
    
    set widgetInfo($address-status) 1
    
    ttk::frame $win 
    eval $widgetInfo(packme-$win)
   

    set title [dTree_getAttribute $XMLtree $full_address_XML "title"]   
    ttk::label $win.lb -text "$title:" 
    ttk::frame $win.t 
    
    pack $win.lb -side top -pady 3
    pack $win.t -side top -pady {0 5}
    
    set size_v 5
    set size_h 40
    set widgetInfo($address-cleanstring) [dTree_tryGetAttribute $XMLtree  $full_address_XML_clean "cleanstring" "on"] 
    
    set size [split [dTree_tryGetAttribute $XMLtree  $full_address_XML_clean "size" "1;1"] ";"]
    if {[llength $size] != 2 } {puts "Error, size attribute in comment should be formatted as size=x;y"}    
    set size_h [expr {int([lindex $size 0]*1.*$size_h)}]
    set size_v [expr {int([lindex $size 1]*1.*$size_v)}]
    
    
    set widgetInfo($address-state)  [dTree_tryGetAttribute $XMLtree $full_address_XML_clean "state" "normal"]
    
    switch $widgetInfo($address-state) {
        "normal" {
            text $win.t.text  -yscrollcommand [list $win.t.sby set]  -width $size_h -height $size_v -wrap word -background khaki1 -highlightcolor black -highlightbackground [ThemeColor 0.9] -selectbackground skyblue1 -highlightthickness 1
            label $win.t.text.corner -image icon_corner -compound left -background khaki1 -borderwidth 0
            place $win.t.text.corner -relx 1.0 -rely 1.0 -anchor se
        }
        "disabled" {
            text $win.t.text  -yscrollcommand [list $win.t.sby set]  -width $size_h -height $size_v -wrap word -state disabled -background [ThemeColor 1]  -highlightcolor black -highlightbackground black -selectbackground skyblue1 -highlightthickness 1
        }
    }
    
    bind  $win.t.text <Enter> [subst { set tabscroll 0 }]
    bind  $win.t.text <Leave> [subst { set tabscroll 1 }]
   
    
    bind $win.t.text <FocusOut>  [subst {eval \$widgetInfo($address-check)}]
    ttk::scrollbar $win.t.sby -orient vertical -command [list $win.t.text yview]
    
    grid $win.t.text -row 1 -column 0 -sticky nes 
    grid $win.t.sby -row 1 -column 1 -sticky news
    
    if {$widgetInfo($address-cleanstring) == "latex"} {
        ttk::frame $win.t.latex
        grid $win.t.latex -row 2 -column 0 -sticky news -pady 2
        
        ttk::button $win.t.latex.bold -text "Bold" -width 4  -command [subst {
            comment_decoration $win $address "BSLASHtextbf{" "}"
        }]
        pack  $win.t.latex.bold -side left 
        
        ttk::button $win.t.latex.ital -text "Ital." -width 4  -command [subst {
            comment_decoration $win $address "BSLASHtextit{" "}"
        }]
        pack  $win.t.latex.ital -side left 
        
        ttk::button $win.t.latex.item -text "Item." -width 4  -command [subst {
            comment_decoration $win $address "BREAKLINE BSLASHbegin{itemize} BREAKLINE BSLASHitem " "BREAKLINE BSLASHend{itemize} BREAKLINE"
        }]
        pack  $win.t.latex.item -side left 
        ttk::button $win.t.latex.enum -text "Enum." -width 4  -command [subst {
            comment_decoration $win $address "BREAKLINE BSLASHbegin{enumerate} BREAKLINE BSLASHitem " "BREAKLINE BSLASHend{enumerate} BREAKLINE"
        }]
        pack  $win.t.latex.enum -side left 
        
        
        ttk::button $win.t.latex.circum -text "^" -width 1  -command [subst {
            comment_decoration $win $address "BSLASH^{" "}"
        }]
        pack  $win.t.latex.circum -side left -padx {2 0}
        
        ttk::button $win.t.latex.trema -text "¨" -width 1  -command [subst {
            comment_decoration $win $address "BSLASH¨{" "}"
        }]
        pack  $win.t.latex.trema -side left
        
        
        
        
        bind $win.t.text <<Selection>>  [subst {
            set sel \[$win.t.text tag ranges "sel"\]
            if {\$sel != ""} {
                set widgetInfo($address-selection) \$sel    
            }
            log ">>  \$widgetInfo($address-selection)"
        }]
    }
    
    
    #add the check/refresh procedure to the bindings of the variable
   
    append widgetInfo($address-refresh) [subst { comment_refresh $win $address}]
    append widgetInfo($address-check) [subst { comment_check $win $address}]
    finishWidget
    
    
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    
    return $win
}


proc comment_decoration {win address prefix suffix} {
    global widgetInfo
    set i0 [lindex $widgetInfo($address-selection) 0]
    set i1 [lindex $widgetInfo($address-selection) 1]
    
    set txt [$win.t.text get $i0 $i1]
    
    set bit [string map {"BSLASH" "\\" "BREAKLINE" "\n"} "$prefix$txt$suffix"]
    $win.t.text replace  $i0 $i1 $bit

}

proc comment_refresh {win address} {
    global widgetInfo
    
    $win.t.text configure -state normal
    $win.t.text delete 0.0 end
    set txt [string map {"BREAKLINE" "\n"  "&gt;" "/" "&quot;" "'" "&amp;" "&"}  $widgetInfo($address-variable)  ]
    $win.t.text insert end $txt
    $win.t.text configure -state $widgetInfo($address-state)
    
    smartpacker_update_visibility $win $address
}


proc comment_check {win address} {
    global widgetInfo
    set txt [string trim [$win.t.text get 0.0 end]]
    
    
    switch $widgetInfo($address-cleanstring) {
        "latex" {   set txt [ string2latex $txt ] }
        "on" {   set txt [ stringclean $txt ] }
        "off" {}
        default {}
    }
    
    
   
   #enleve les accents dans l'affichage
    $win.t.text configure -state normal
    $win.t.text delete 0.0 end
    $win.t.text insert end $txt
    
    $win.t.text configure -state $widgetInfo($address-state)
    
    # stocke dans la variable
    set txt [string map {\n BREAKLINE}  $txt  ]
    
    set widgetInfo($address-variable) $txt
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