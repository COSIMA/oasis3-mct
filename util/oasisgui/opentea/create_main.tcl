#  This program is under CECILL_B licence. See footer for details.

# fenetre d'accueil si aucune application n'est choisie au lancement.

proc main_create {} {
    global pathEngine libraryPath applicationList relaunchCmd configPath
    
    wm geometry . 640x640
        
    # Get the recent projects 
    set recents [getRecentProjects] 
       
    ttk::frame .main
    pack .main -fill both -expand true
    
    ttk::frame .main.header
    place  .main.header -relx 0.05 -rely 0.0 -relwidth 0.9 -relheight 0.2
    
    
    ttk::label .main.header.icon -image icon_gui_small
    
    wm iconphoto .  icon_gui_small
    pack .main.header.icon     

   
    
    # applications
    ttk::labelframe .main.app -text "Applications"
    
    
    #pack .main.app  -fill both -expand true
    place  .main.app -relx 0.05 -rely 0.3 -relwidth 0.9 -relheight 0.65
    
    canvas .main.app.c  -yscrollcommand [list .main.app.sby set] -xscrollcommand [list .main.app.sbx set] -background [ThemeColor 1.0] -highlightbackground [ThemeColor 1.0]
    ttk::scrollbar .main.app.sbx -orient horizontal -command [list .main.app.c xview]
    ttk::scrollbar .main.app.sby -orient vertical -command [list .main.app.c yview]
    #grid .main.app.c -row 0 -column 0 
    #grid .main.app.sby -row 0 -column 1 -sticky news
    #grid .main.app.sbx -row 1 -column 0 -sticky news
    
    place  .main.app.c -relx 0 -rely 0 -relwidth 0.95 -relheight 0.95
    place  .main.app.sby -relx 0.95 -rely 0  -relheight 0.95
    place  .main.app.sbx -relx 0 -rely 0.95  -relwidth 0.95 
    
    
    ttk::frame .main.app.c.f
    .main.app.c  create window 1 1 -window .main.app.c.f
    
    #pack .main.app.c.f
    
    set win ".main.app.c.f"
    
    
    
    
    set colmax 3
    set colid 0
    set rowid 0
    set appid 0
  
    foreach app $applicationList {
        if {$app == "config"} {continue}
        if {$colid > $colmax } {
            set colid 0
            incr rowid  2      
        }
        set winref "appico_$appid"
        
        
        set filename [file join $libraryPath $app XML icon.gif]
        if {[ file exists $filename]} {
            image create photo icon_$winref -file $filename        
            ttk::label $win.$winref -image  icon_$winref 
            grid $win.$winref -column $colid -row $rowid -padx 10 -pady {10 0}
            set cmd "$relaunchCmd -code $app &"
            bind $win.$winref <ButtonPress-1>  [subst {
                puts "Executing $cmd"
                exec $cmd
                exit
            }]
        }
            
        set winref "applbl_$appid"
        ttk::label $win.$winref -text $app
        set cmd "$relaunchCmd -code $app &"
        bind $win.$winref <ButtonPress-1>  [subst {
            puts "Executing $cmd"
            exec $cmd
            exit
        }]
        grid $win.$winref -column $colid -row [expr $rowid + 1] -padx 10 -pady {0 10}
        
        
        incr appid
        incr colid
    }
    
    
    
    bind .main.app.c <Configure> [subst {
        .main.app.c configure -scrollregion \[ .main.app.c bbox all\]
        .main.app.c xview moveto 0.0
        .main.app.c yview moveto 0.0
    }]
    
  
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