#  This program is under CECILL_B licence. See footer for details.




# ENTRY CREATION

proc status_create { args } {
    set mandatory_arguments { path_father address }
    # Initializes the widget
    initWidget
    
     
    set widgetInfo($address-status) "0"
    set widgetInfo($address-status_txt) "" 
    
    ttk::frame $win -height 40p -width $widgetInfo(guiSmallWidgetWidth)
    eval $widgetInfo(packme-$win)
    ttk::label $win.status -textvariable widgetInfo($address-status_txt) -justify center -compound left
    $win.status configure -wraplength [expr { 0.9*$widgetInfo(guiSmallWidgetWidth)}]
    pack $win.status
    
    
    set widgetInfo($address-status) [dTree_tryGetAttribute $XMLtree $full_address_XML "default" "-1"]
    
    
    set widgetInfo($address-msgerr) [dTree_tryGetAttribute $XMLtree $full_address_XML "msgerr" ""]
    set widgetInfo($address-msgunknown) [dTree_tryGetAttribute $XMLtree $full_address_XML "msgunknown" ""]
    set widgetInfo($address-msgtrue) [dTree_tryGetAttribute $XMLtree $full_address_XML "msgtrue" ""]
    
    
    
    append widgetInfo($address-refreshStatus) [ subst { status_refreshStatus $win $address}]
    
    eval $widgetInfo($address-refreshStatus)
    
    finishWidget
    # clean the widget callBack on dstruction
    bind $win <Destroy> [ subst {widget_destroy $win $address}]
    return $win
}

proc status_refreshStatus {win address} {
    global widgetInfo
    set widgetInfo($address-status) $widgetInfo($address-variable)
    
    switch  $widgetInfo($address-variable) {
        "-1" {
            set widgetInfo($address-status_txt) $widgetInfo($address-msgerr)
            $win.status configure -image icon_flag -foreground red
        }
        "0" {
            set widgetInfo($address-status_txt) $widgetInfo($address-msgunknown)
             $win.status configure -image "" -foreground orange2
        }
        "1" {
            set widgetInfo($address-status_txt) $widgetInfo($address-msgtrue)
             $win.status configure -image "" -foreground green4
        }
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