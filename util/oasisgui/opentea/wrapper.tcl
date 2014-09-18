#  This program is under CECILL_B licence. See footer for details.

rename proc _proc


_proc proc {name params body} {
    global FunctionUsed
    global FunctionTime
    global TraceProfile
    
    
    
    set FunctionTime ""
   
    if { $name in "::tk::TearOffMenu ::tk::MenuDup Window_profiling Window_profiling_trace" } {
        puts "skipping $name"
    } else {
        #puts "adding $name"
        set FunctionUsed($name) 0
        set body "global FunctionUsed \n global FunctionTime \n global TraceProfile \n if {\$TraceProfile} { \n incr FunctionUsed($name) \n lappend FunctionTime IN#$name#\[clock microseconds\] \n } \n $body \n \n if {\$TraceProfile} { \n lappend FunctionTime OUT#$name#\[clock microseconds\] \n }"
    } 
    _proc $name $params $body
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