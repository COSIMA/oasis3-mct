#  This program is under CECILL_B licence. See footer for details.

proc shadeColor { colorname shade} {
   set color [winfo rgb . $colorname]
   set R [lindex $color 0]
   set G [lindex $color 1]
   set B [lindex $color 2]
   
   
   if {$shade > 0 } {
      set DR [expr {65535 - $R}]
      set DG [expr {65535 - $G}]
      set DB [expr {65535 - $B}]
      
      set newR [expr { int($R+ $DR*$shade)}]
      set newG [expr { int($G+ $DG*$shade)}]
      set newB [expr { int($B+ $DB*$shade)}]
   } else {
      set newR [expr { int($R*(1.+$shade))}]
      set newG [expr { int($G*(1.+$shade))}]
      set newB [expr { int($B*(1.+$shade))}]  
   }
   
   set color [format "#%04x%04x%04x" $newR $newG $newB]
   return $color
}


proc RGBtoCOLCODE {col} {
   set x [lindex $col 0]
   set y [lindex $col 1]
   set z [lindex $col 2]
   set x [expr int($x)]
   set y [expr int($y)]
   set z [expr int($z)]
   set color [format "#%02x%02x%02x" $x $y $z]
  return $color
}

proc mix_color {mix_color} {
   set ftot 0
   foreach { f r g b} $mix_color {
        set ftot [expr {$ftot+$f*1.}]
   }
   if {$ftot == 0.} {return {0 0 0}}
   set red 0.
   set green 0.
   set blue 0.
   foreach { f r g b} $mix_color {
        set red [expr {$red+$f*1./$ftot*$r}]
        set green [expr {$green+$f*1./$ftot*$g}]
        set blue [expr {$blue+$f*1./$ftot*$b}]
   }
   set result [list $red $green $blue]
    return $result
}


proc mix_color2 { colorname colorname2 shade} {
   set color [winfo rgb . $colorname]
   set R1 [lindex $color 0]
   set G1 [lindex $color 1]
   set B1 [lindex $color 2]
   
   set color [winfo rgb . $colorname2]
   set R2 [lindex $color 0]
   set G2 [lindex $color 1]
   set B2 [lindex $color 2]
   
   
   if {$shade >= 0 } {
     
      set newR [expr { int($R2*$shade+(1.-$shade)*$R1)}]
      set newG [expr { int($G2*$shade+(1.-$shade)*$G1)}]
      set newB [expr { int($B2*$shade+(1.-$shade)*$B1)}]
   } 
   
   set color [format "#%04x%04x%04x" $newR $newG $newB]
   return $color
}



proc max_color { col } {
   set x [lindex $col 0]
   set y [lindex $col 1]
   set z [lindex $col 2]
   

   set dx [expr {1.*$x/100.}]
   set dy [expr {1.*$y/100.}]
   set dz [expr {1.*$z/100.}]
   
   set done 0
   while {$done ==0} {
      set x [expr {$x+$dx}]
      set y [expr {$y+$dy}]
      set z [expr {$z+$dz}]
      if { $x >= 255. } {
         set x 255
         set done 1
      }
      if { $y >= 255. } {
         set y 255
         set done 1
      }
      if { $z>= 255. } {
         set z 255
         set done 1
      }
      set x [expr int($x)]
      set y [expr int($y)]
      set z [expr int($z)]
   }
   
   set result [list $x $y $z]
   return $result
   
}




proc colorscale_define { name } {
    
    set colorscale ""
    
    switch $name {
         "rainbow" {
            lappend colorscale {0.00 0.0 0.0 1.0}
            lappend colorscale {0.25 0.0 1.0 1.0}
            lappend colorscale {0.50 0.0 1.0 0.0}
            lappend colorscale {0.75 1.0 1.0 0.0}
            lappend colorscale {1.00 1.0 0.0 0.0}
         }
         "Xrainbow" {
            lappend colorscale {0.00 0.0 0.0 0.0}
            lappend colorscale {0.166 0.0 0.0 1.0}
            lappend colorscale {0.333 0.0 1.0 1.0}
            lappend colorscale {0.50 0.0 1.0 0.0}
            lappend colorscale {0.666 1.0 1.0 0.0}
            lappend colorscale {0.833 1.0 0.0 0.0}
            lappend colorscale {1.00 1.0 1.0 1.0}
         }
        default {
            lappend colorscale {0.0 0 0 0}
            lappend colorscale {1.0 1.0 1.0 1.0}
        }
    }
    
    return $colorscale
}



proc colorscale_get { colorscale value } {
    
    set R1 ""
    set G1 ""
    set B1 ""
    set R2 ""
    set G2 ""
    set B2 ""
    set len [llength $colorscale]
    set arm "no"
    for {set i 0} {$i < $len} {incr i} {
        set j [expr {$i+1}]
        set V1 [lindex [lindex $colorscale $i] 0 ]
        set R1 [lindex [lindex $colorscale $i] 1 ]
        set G1 [lindex [lindex $colorscale $i] 2 ]
        set B1 [lindex [lindex $colorscale $i] 3 ]
        set V2 [lindex [lindex $colorscale $j] 0 ]
        set R2 [lindex [lindex $colorscale $j] 1 ]
        set G2 [lindex [lindex $colorscale $j] 2 ]
        set B2 [lindex [lindex $colorscale $j] 3 ]
        if {$value > $V1 && $value <= $V2} {
            set A [expr {($value - $V1)*1./($V2 - $V1)}]
            set R [expr { int(255.* ($A*$R2 +(1.0-$A)*$R1))}]
            set G [expr { int(255.* ($A*$G2 +(1.0-$A)*$G1))}]
            set B [expr { int(255.* ($A*$B2 +(1.0-$A)*$B1))}]
            break
        } elseif {$value == $V1} {
            set R [expr { int(255.*$R1)}]
            set G [expr { int(255.*$G1)}]
            set B [expr { int(255.*$B1)}]
            break               
        } else {
            set R 0
            set G 0
            set B 0
        }
        
    }
    return [format "#%02x%02x%02x"  $R $G $B] 
}


proc randcolor {} {
    set r [expr { int(256 * rand()) }]
    set g [expr { int(256 * rand()) }]
    set b [expr { int(256 * rand()) }]

    set gap [expr {300 - $r -$g -$b }]
    if {$gap > 0} {
        set plus [expr {int($gap*1./3.)}]
        incr r $plus
        incr g $plus
        incr b $plus
    }
    
    set newColor [format "#%02x%02x%02x" $r $g $b]
    return $newColor
}

proc blendcolor { c1 c2 c3} {
    #puts "blending $c1 $c2 $c3"
    set R1 [expr 0x[string range $c1 1 2]]
    set G1 [expr 0x[string range $c1 3 4]]
    set B1 [expr 0x[string range $c1 5 6]]
    set R2 [expr 0x[string range $c2 1 2]]
    set G2 [expr 0x[string range $c2 3 4]]
    set B2 [expr 0x[string range $c2 5 6]]
    set R3 [expr 0x[string range $c3 1 2]]
    set G3 [expr 0x[string range $c3 3 4]]
    set B3 [expr 0x[string range $c3 5 6]]
    
    set r [expr {int(($R1 +$R2 +$R3)*1./3)}]
    set g [expr {int(($G1 +$G2 +$G3)*1./3)}]
    set b [expr {int(($B1 +$B2 +$B3)*1./3)}]
    
    set newColor [format "#%02x%02x%02x" $r $g $b]
    return $newColor  
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