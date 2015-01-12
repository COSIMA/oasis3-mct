#  This program is under CECILL_B licence. See footer for details.



###############################################
# Switch form,based upon the Tabnotebook
# Beware of multiple matches bindings like in scrollbars
################################################

# creation of the notebook without tabs 
proc switchform_create { win } {
    global widgetInfo
    ttk::frame $win
    ttk::frame $win.blank
    label $win.blank.logo -image bg_200 -text "Nothing selected..." -compound top -font "helvetica 30" -bg [ThemeColor 1.0] -fg [ThemeColor 0.9]
    
    bind . <<ThemeUpdate>> +[subst {$win.blank.logo configure -bg \[ThemeColor 1.0\] -fg \[ThemeColor 0.9\]}]
    
    pack $win.blank.logo -side left -padx 200 -pady 20
    
    pack $win -side top -expand 1 -fill both
}

# add a tab to the switchform
proc switchform_add { win name } {
		ttk::frame $win.$name
}

# remove a tab to the switchform
proc switchform_del { win name } {
		destroy $win.$name
}


# raise a tab in the switchform
proc switchform_raise { win name } {
    global widgetInfo
    catch {
        if { [winfo ismapped $win.$name] == 0 } {
            foreach wc [winfo children $win] {
    	    pack forget $wc 
    	}
    	pack $win.$name  -side top -fill x
	set widgetInfo(fixedview) 1
    	event generate . <<SetView>>
	set widgetInfo(fixedview) 0
        }
    }
    
}


# raise a tab in the switchform
proc switchform_blank { win  } {
    global widgetInfo
    catch {
	foreach wc [winfo children $win] {
	    pack forget $wc
	}
    }
    pack $win.blank -side top
    
    set widgetInfo(fixedview) 1
    event generate . <<SetView>>
    set widgetInfo(fixedview) 0
}


# return where child widgets must be packed
proc switchform_interior { win name } {
	return $win.$name
}





# former version using tabnotebooks

proc switchform_create2 { win } {
    ttk::notebook $win -style "NoTabs.TNotebook"
    ttk::notebook::enableTraversal $win
    pack $win -side top -expand 1 -fill both
}

proc switchform_add2 { win name } {
		$win add [ttk::frame $win.$name] -sticky news
}

proc switchform_raise2 { win name } {
    
    if { [$win select] != "$win.$name" } {
	$win select $win.$name
	$win configure -height -1
	update idletasks
	set height 0
        foreach wc [winfo children $win.$name] {
	    incr height [winfo reqheight $wc]
	}
	$win configure -height $height
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