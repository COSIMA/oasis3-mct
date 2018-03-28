#  This program is under CECILL_B licence. See footer for details.




# catch error using :
# if { [test_vartype] != 1}
proc test_vartype {value type} {
    global ndim
    switch -glob $type {
        "integer*" {
            if { [string is integer $value ] } {
                set test_result 1
            } else {
                set test_result "\"$value\" not integer"
            }
            if {$test_result==1 && [string match "integer_*" $type]} {
                set test_result [test_vartype_conditions $value $type]
            }
        }
        "double*" {
            if { [string is double $value ] || [string is integer $value]} {
                set test_result 1
            } else {
                set test_result "\"$value\" not real"
            }
            
            if {$test_result==1 && [string match "double_*" $type]} {
                
                set test_result [test_vartype_conditions $value $type]
            }
        }
        "vector" {
        # THE USE OF VECTOR IS DEPRECATED - Remains  for bckward Compatibility
            set test_result 1
            if {[llength $value] != $ndim } {
                set test_result "\"$value\" is not a $ndim-dimension vector"
            } else {
                foreach component $value {
                    if {  [string is double $component ] == 0 } {
                        set test_result "\"$component\" not real"
                    }
                }
            }
        }
        "date" {
             set test_result 1
            if { [string length $value] != 8 } {
                set test_result "\"$value\" is not in jj/mm/aa format"
            } else {
                set jj [string trimleft [string range $value 0 1] "0"]
                set sep [string index $value 2]
                set mm [string trimleft [string range $value 3 4] "0"]
                set sep2 [string index $value 5]
                set aa [string range $value 6 7]
                
                
                
                
                if {$sep != "/" || $sep2 != "/" } {
                    set test_result "Date separator must be \/"
                    return $test_result
                }
                
                if {![string is integer $jj] || ![string is integer $mm] || ![string is integer $aa]} {
                   set test_result "One of  jj/mm/aa is not an integer"
                   return $test_result
                } 
                
                if {$jj < 1 || $jj > 31 } {
                   set test_result "Day must be btw 01 and 31"
                   return $test_result
                }
                if {$mm < 1 || $mm > 12 } {
                   set test_result "Month must be btw 01 and 12"
                   return $test_result
                }
                
                if {$aa < 13 || $mm > 99 } {
                   set test_result "Year must be btw 13 and 99"
                   return $test_result
                }
            }
        }        
        "fraction" {
            if { [string is double $value ]
                 && $value >= 0.0
                 && $value <= 1.0
                } {
                set test_result 1
            } else {
                set test_result "\"$value\" not btw 0 and 1"
            }
        }
        "string*" {
            set test_result 1
            if {[string is ascii $value ] == 0 } {
                 set test_result "\"$value\" invalid (accents, special chars)"  
            }
            if {$test_result==1 && [string match "string_*" $type]} {
                set test_result [test_vartype_conditions $value $type]
            }
            
        }
        "word" -
        "ascii*" {
            set test_result 1
            if {[string is ascii $value ] == 0 } {
                 set test_result "\"$value\" invalid (accents, special chars)"  
            }
            if { [llength $value] > 1} {
                set test_result "\"$value\" invalid (blank spaces)"    
            }
            if {$test_result==1 && [string match "ascii_*" $type]} {
                set test_result [test_vartype_conditions $value $type]
            }
        }
        "liststring" {
            if { [llength $value] > 0 } {
                set test_result 1
                foreach subvalue $value {
                    if {![string is ascii $subvalue ]} {
                        set test_result "\"$subvalue\" not ASCII"
                        break
                    }
                }
            } else {
                set test_result "(...)"
            }
        }
        "complex" {
            set test_result [test_vartype_complex $value]
        }      
        default {
            set test_result "Type \"$type\" unknown."
        }
    }
    if {$value == ""} {
        set test_result "(...)"
    }
    return $test_result
}

proc test_vartype_complex {c} {
    # Test un seul ou pas de i
    if {[llength [split $c "i"]] > 2 || $c == ""} {
        return "Not complex number"
    }
    
    set decomp [detection_structure_complex $c]
   # puts "decomp : #$decomp#"
    #Analyse of the result :
    if {$decomp == "false" || $decomp == ""} {
        return "Not complex"
    }
    
    # Test of double vide ou i sur première partie
    if {[llength $decomp] == 2} {
        if { [llength [split [lindex $decomp 0] "i"]] == 2 || [llength [split $c "i"]] == 1} {
        return "Not complex"
        }
    }
    
    
    foreach element $decomp {
        set atester [string trimright $element "i"]
        if {![string is double $atester] || $element == ""} {
            return "Not complex"
        }
    }
    
    return 1
}

proc test_vartype_conditions {value type} {
    set conditions [lrange [split $type "_"] 1 end]
    foreach cond $conditions {
        switch -glob $cond {
            "ge*" {
                set bound [string range $cond 2 end]
                if {$value < $bound} {
                    return "\"$value\" too small (min $bound)"
                }
                
            }
            "gt*" {
                set bound [string range $cond 2 end]
                if {$value <= $bound} {
                    return "\"$value\" too small (min $bound)"
                }
                
            }
            "le*" {
                set bound [string range $cond 2 end]
                if {$value > $bound} {
                    return "\"$value\"  too big (max $bound)"
                }
                
            }
            "lt*" {
                set bound [string range $cond 2 end]
                if {$value >= $bound} {
                    return "\"$value\" too big (max $bound)"
                }
            }
            "streq*" {
                set bound [string range $cond 5 end]
                if {[string length $value] > $bound} {
                    return "\"$value\" too long (exactly $bound chars)"
                }
                if {[string length $value] < $bound} {
                    return "\"$value\" too short (exactly $bound chars)"
                }
            }
            "strmax*" {
                set bound [string range $cond 6 end]
                if {[string length $value] > $bound} {
                    return "\"$value\" too long (max $bound)"
                }
            }
            "strmin*" {
                set bound [string range $cond 6 end]
                if {[string length $value] < $bound} {
                    return "\"$value\" too short (min $bound)"
                }
            }
        
        }
    
    
    }
    
    return 1

}

proc detection_structure_complex {c} {
    
    # Nettoyage des elements
    set splitted_p ""
    foreach el [split $c "+"] {
        lappend splitted_p [string trim $el]
    }
    set nb_split_p [llength $splitted_p]
    
    if {$nb_split_p==1} {
        # Pas de + dans la chaine : split sur - ou simple nb
    } elseif { $nb_split_p == 2 && [lindex $splitted_p 0]==""} {
        # Simple nombre ou complexe separe par un -
        
    } elseif {$nb_split_p == 2} {
        # Complexe de deux parties sans entourloupe
        return $splitted_p
    } elseif {$nb_split_p == 3 && [lindex $splitted_p 0] == ""} {
        # Complexe à deux parties avec un plus au début
        return [lrange $splitted_p 1 2]
    } else {
        # Ce n'est pas un complexe ...
        return "false"
    }
    
    # Si le script passe cette ligne, il y a, au plus, un + dans la chaîne
    # Si il y a un +, alors il est au début comme dans "+3.4" ou "+3.4-8i"
    
    set splitted_m ""
    foreach el [split $c "-"] {
        lappend splitted_m [string trim $el]
    }
    
    set nb_split_m [llength $splitted_m]
    
    if {$nb_split_m == 1} {
        # Pas de moins ...
        return [string trimleft [lindex $splitted_m 0] "+"]
    } elseif {$nb_split_m == 2 && [lindex $splitted_m 0] == ""} {
        # Simple nombre négatif
        return [lindex $splitted_m 1]
    } elseif {$nb_split_m == 2} {
        # Nombre complexe séparé par un -
        return [list [string trimleft [lindex $splitted_m 0] "+"] [lindex $splitted_m 1]]
    } elseif {$nb_split_m == 3 && [lindex $splitted_m 0]== ""} {
        # Nombre complexe à deux parties qui commence par un moins
        return [list [lindex $splitted_m 1] [lindex $splitted_m 2]]
    } else {
        return "false"
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