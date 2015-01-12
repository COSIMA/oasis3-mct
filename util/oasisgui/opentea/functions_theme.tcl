#  This program is under CECILL_B licence. See footer for details.


#################################################
# Procedure to swithch from theme to theme
# still need a fix on the smartpaker
#################################################

proc ThemeUpdate {  } {
    global theme focusColor
    set themelist [ttk::style theme names]
    
    if {[lsearch $themelist $theme] ==-1} {
	popup_error "Theme $theme not available on this platform\n switching to default theme clam" exitcontinue
	ttk::style theme use clam
    } else {
	   # log "GUI theme : $theme" 
	ttk::style theme use $theme
    }
    
    
#  
    
    # label des modeles
    ttk::style configure Model.TLabelframe.Label -foreground [ThemeColor 0.30]
    # labels multiples et xor
    ttk::style configure Xor.TLabelframe.Label -foreground [ThemeColor 0.30]    
    ttk::style configure Multiple.TLabel -foreground [ThemeColor 0.50]
  
    ttk::style configure Disabled.TLabel -foreground [ThemeColor 0.50]
    #ttk::style configure Flat.TLabelframe -relief flat -text ""
    
    # disparition des Tabs sur les notebooks
    ttk::style layout NoTabs.TNotebook.Tab null
    ttk::style layout NoTabs.TNotebook null
    
    event generate . <<ThemeUpdate>> 
}


#################################################
# Procedure to get the main theme color
# and redurn any shade of the same color
#
# 1.0 is the initial bg color
# factor below 1 gives a darker color
# factor beyond 1 gives a lighter color
#################################################
proc ThemeColor { factor {item background}  } {
    global theme
    if {$theme== "aqua"} {
	#set rgb "60909 60909 60909"
	set rgb "59898 59898 59898"
	
    } else {
        set rgb [winfo rgb . [ttk::style lookup .activefield -$item]]
    }
    
    set R [expr {round(min(255,[lindex $rgb 0]*1.*$factor/257.))}]
    set G [expr {round(min(255,[lindex $rgb 1]*1.*$factor/257.))}]
    set B [expr {round(min(255,[lindex $rgb 2]*1.*$factor/257.))}]

    set newrgb [format #%02x%02x%02x $R $G $B ]

    return $newrgb
    
    
}


proc FocusColor {  } {
    global theme
     switch $theme {
	"aqua" {
	    set focusColor "#77acdc"
	}
	"clam" {
	    set focusColor "#77acdc"
	}
	default {
	    set focusColor "black"
	}
    }

    return $focusColor
    
    
}

proc getcharsize { sizeinpx {fontname courier} } {
    set width0 [font measure $fontname "0"]
    set charsize [expr {int(1.0*$sizeinpx/$width0)}]
    return $charsize
}

proc getcharheight { sizeinpx {fontname courier} } {
    set height [font metrics  $fontname -linespace] 
    set charheight [expr {int(1.0*$sizeinpx/$height)}]
    return $charheight
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