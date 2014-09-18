#  This program is under CECILL_B licence. See footer for details.



set faqdata(faqlist) ""


set faqname "validation"

lappend faqdata(faqlist) $faqname
set faqdata($faqname-title) "Validation of tabs"
set faqdata($faqname-body) "The information written in a tab is validated ONLY through the process button, at the lower left. The states are :\n  - A red cross for an invalid information \n  - An orange interrogation mark for an information in unknown state \n  - A green V mark for a validated information \n \n After the tab processing, the validation of a level is the weakest state of all of its childrens. For example, if a tab is red (invalid), there is at least and without a doubt one of its children or grandchildren information which is invalid."


set faqname "freezing"

lappend faqdata(faqlist) $faqname
set faqdata($faqname-title) "Freezing application"
set faqdata($faqname-body) "During a process, the application will refuse any interaction, in order to prevent the competition of multiple processes. However, the user can get back to normal state using the Escape key to interrupt the process if it takes too much time. \n Note that some low level processes such as external tool execution (Paraview, Fortran) will refuse the interruption. These process must be taken care using the task manager of the computer."



set faqname "logerror"
lappend faqdata(faqlist) $faqname
set faqdata($faqname-title) "What to do when the log console shows an error in red?"
set faqdata($faqname-body) "If a process is giving an error : \n 1 - Read carefully the full length of logs. Most of the errors are explicitely named here. Use the magnifier button to increase the size of the log window.  \n 2 - Check that you are using the correct Plug-in for your situation. (see nest FAQ item)  \n 3 - Check that the inputs given to the plugin are correctly set : login, writable directory, etc...  \n 4 - Share this error, either by reporting it to the manager of your installation, or by sumitting a query on the Redmine website, with the data needed to reproduce the problem.  "

set faqname "plugin"
lappend faqdata(faqlist) $faqname
set faqdata($faqname-title) "Select the correct plug-in : what is the meaning of this ?"
set faqdata($faqname-body) "Each computing situation is different. Your computing situation  is the combination of two informations : where do you run the GUI ? where do you run the tools associated to your application ? \nTherefore each site have its own set of plugins. At cerfacs, we have for example cerfacs_formation (for GUI and tool execution on the formation room) or cerfacs_neptune_ssh (for massively parallel code execution on the cluster NEPTUNE). \nIt is a good thing to know clearly what is the range of applications given by your plugin. Ask your code manager about this topic for precise informations. "

    



set faqname "focus"
lappend faqdata(faqlist) $faqname
set faqdata($faqname-title) "(Mac OSX) Impossible to write in the entries"
set faqdata($faqname-body) "On Mac OSX remote execution , e.g. execution on a UNIX platform, trough a terminal to a Mac OSX display, some text entries canot be written. This is due to compatibility issues between different layers of window managers. To resolve the problem, set the parameter Focus correction OSX/X11 to 1 in your config.xml "



set faqname "circumflex"
lappend faqdata(faqlist) $faqname
set faqdata($faqname-title) "(Mac OSX) The key ^ or ¨ crashes the application"
set faqdata($faqname-body) "This is a bug due to the way Mac OSX encode these keys, which can provoke unexpected errors on key bindings.\n
For applications generating LaTeX documents, a special widget gives among other shortcuts, a macro to insert \^{ } directly on a selected letter."

    
    


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