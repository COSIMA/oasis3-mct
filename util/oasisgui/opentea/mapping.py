#  This program is under CECILL_B licence. See footer for details.

# the purpose of this program is to add headers and footers for Licensing sources.
# Its far from generic 


import os
import random
import string
import glob

path=os.getcwd()
nameraw=os.listdir(path)
name = []
for item in nameraw:
    if os.path.isfile(item):
        if not item.startswith('.'):
            if not item.startswith('mapping'):
                name.append(item)  
            

fileExtension=[{} for i in range(len(name))]
fileName=[{} for i in range(len(name))]
completeName=[{} for i in range(len(name))]

# initialisation


year ="2014"
company= "CERFACS"
headerdb={}
headerdb["licences"]=[]

content="""Copyright """+company+" "+year+"""
 
antoine.dauptain@cerfacs.fr
 
This software is a computer program whose purpose is to ensure technology
transfer between academia and industry.
 
This software is governed by the CeCILL-B license under French law and
abiding by the rules of distribution of free software.  You can  use, 
modify and/ or redistribute the software under the terms of the CeCILL-B
license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info". 
 
As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability. 
 
In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or 
data to be ensured and,  more generally, to use and operate it in the 
same conditions as regards security. 
 
The fact that you are presently reading this means that you have had
knowledge of the CeCILL-B license and that you accept its terms."""

headerdb["licences"].append("cecill")
headerdb["cecill","header"] = "This program is under CECILL_B licence. See footer for details."
headerdb["cecill","footer"] = content.split("\n")
headerdb["cecill","name"] = "Cecill-B"


content="""Copyright """+company+" "+year+"""
 
antoine.dauptain@cerfacs.fr
 
This software is a computer program whose purpose is to ensure technology
transfer between academia and industry.
 
This software is governed by a OpenScience license.
You can  use,  modify and/ or redistribute the software.
 
As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights. 
 
In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or 
data to be ensured and,  more generally, to use and operate it in the 
same conditions as regards security. 
 
The fact that you are presently reading this means that you have had
knowledge of the OpenScience license and that you accept its terms"""

headerdb["licences"].append("openscience")
headerdb["openscience","header"] = "This program is under OpenScience licence. See footer for details"
headerdb["openscience","footer"] = content.split("\n")
headerdb["openscience","name"] = "openscience"


content="Copyright CERFACS-IFP-SAFRAN "+year+"""
 
antoine.dauptain@cerfacs.fr
 
Ce logiciel est un programe informatique dont l'objectif est de proposer
des outils numeriques pour l'etude de la combustion, des turbomachines,
de l'acoustique, de la thermique et du rayonmment.
 
Ce logiciel est gouverne par l'ACCORD DE COOPERATION AVBP (N. IFP 31.293)
sous loi francaise, et fait explicitement partie de la -version commune- d'AVBP.
 
Ce logiciel est la copropriete de CERFACS, SAFRAN et l'IFPEN, nommees ci apres les Parties,
dans les termes de l'ACCORD DE COOPERATION AVBP.
En cela:
-  Chaque Partie pourra utiliser la -version commune- du code AVBP
incluant la modalite 1 et la modalite 2 pour ses besoins propres
de recherche librement, gratuitement, et sans limitation de duree.
Par besoins propres de recherche, on entend une utilisation en
interne de la -version commune- du code AVBP par une Partie pour
des activites commerciales.
 
-le CERFACS et l'IFPEN pourront librement entreprendre l'exploitation
commerciale de la -version commune- du code AVBP incluant la modalite 1
et la modalite 2 en toute independance et gratuitement a l'egard de
l'autre partie, sous reserve de ne pas l'utiliser au profit de concurrents
de SAFRAN ni de la diffuser a des tiers sous quelque forme que ce soit.
 
-SAFRAN pourra exploiter librement la -version commune- du code AVBP
incluant la modalite 1 dans le cadre de son activite d'etude et de
conception de ses produits, sous reserve de ne pas l'utiliser au profit
de concurrents du CERFACS et le l'IFP, ni de la diffuser a des tiers
sous quelque forme que ce soit.
 
L'utilisation de la -version commune- du code AVBP sera aux seuls
risques de la partie exploitante et sous la seule responsabilite de
la dite Partie. La -version commune- et toute documentation eventuelle
y afferente sont fournis en l'etat, sans garantie d'aucune sorte.
Nottament, aucune garantie en ce qui concerne l'utilisation initerrompue
du code, ou les resultats obtenus lors de l'utilisation de la
-version commune-, que ce soit leur exactitude, fiabilite ou autre,
ou la jouissance paisible de la -version commune- n'est donnee.
En consequence chacune des parties renonce a tout recours vis a vis des
autres parties en ce qui concerne les consequences resultant de
l'utilisation de la -version commune-, de quelque nature que ce soit. 
 
A cet egard  l'attention de l'utilisateur est attiree sur les risques
associes au chargement,  a l'utilisation,  a la modification et/ou au
developpement et a la reproduction du logiciel par l'utilisateur etant 
donne sa specificite de logiciel scientifique, qui peut le rendre complexe a 
manipuler et qui le reserve donc a des developpeurs et des professionnels
avertis possedant  des  connaissances  informatiques approfondies. Les
utilisateurs sont donc invites a charger et tester l'adequation  du
logiciel a leurs besoins dans des conditions permettant d'assurer la
securite de leurs systemes et ou de leurs donnees et, plus generalement, 
a l'utiliser et l'exploiter dans les memes conditions de securite. 
 
Le fait que vous puissiez acceder a cet en-tete signifie que vous avez 
pris connaissance de l'ACCORD DE COOPERATION AVBP, et que vous en avez
accepte les termes."""

headerdb["licences"].append("accordAVBP")
headerdb["accordAVBP","header"] = "Ce programme depend des accords l'ACCORD DE COOPERATION AVBP (N. IFP 31.293). Voir la fin du programme pour plus de details."
headerdb["accordAVBP","footer"] = content.split("\n")
headerdb["accordAVBP","name"] = "accordAVBP"

mapping=""

ii=0
_python=0
_fortran=0
_tcl=0
_html=0
_XML=0
_others=0
_files=0
_cecill=0
_cerfacs=0
_openscience=0

if os.path.isfile('mapping.txt'):
    fichier=open('mapping.txt',"r")
    mapp=fichier.read().split("\n")
    fichier.close()
    for i in range(len(name)):
        (fileName[i], fileExtension[i]) = os.path.splitext(name[i])
        completeName[i]=path + '/' + fileName[i]
    for i in range(len(name)):
        fichier=open(fileName[i]+fileExtension[i],"r")
        contenu=fichier.read().split("\n")
        fichier.close()
        contenu3=""
        _files=_files+1
        
# Boucle global
        
        if completeName[i] in mapp[ii]:

            ##### Compte Lincences #####
            
            if 'CERFACS' in mapp[ii]:
                _cerfacs=_cerfacs+1
            elif 'CECILL' in mapp[ii]:
                _cecill=_cecill+1
            elif 'OPENSCIENCE' in mapp [i]:
                _openscience=_openscience+1
            
# Extension python
            
            if fileExtension[i]=='.py':
                
                _python=_python+1
                
                mapping += completeName[i]+" "*(95-len(completeName[i]))+fileExtension[i]+" "*(15-len(fileExtension[i]))
                
                ##### CERFACS #####
                
                if 'CERFACS' in mapp[ii]:
                    
                    ###### Header #####
                    
                    if contenu[0].startswith("#  This program"):
                        contenu[0]="#  "+headerdb["accordAVBP","header"]
                    elif contenu[0].startswith("#  Ce programme"):
                        contenu[0]="#  "+headerdb["accordAVBP","header"]
                    else:
                        contenu[0]="#  "+headerdb["accordAVBP","header"]+"\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("#  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("#  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("#  "+headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("#  "+headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n"
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                        
                    mapping += ' / CERFACS'
                    
                    
                ##### CECILL #####
                            
                elif 'CECILL' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[0].startswith("#  This program"):
                        contenu[0]="#  "+headerdb["cecill","header"]
                    elif contenu[0].startswith("#  Ce programme"):
                        contenu[0]="#  "+headerdb["cecill","header"]
                    else:
                        contenu[0]="#  "+headerdb["cecill","header"]+"\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("#  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("#  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("#  "+headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("#  "+headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n"
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["cecill","footer"][num]
                            num=num+1
                        
                    mapping += ' / CECILL'
                    
                    
                ##### OPENSCIENCE #####
                
                elif 'OPENSCIENCE' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[0].startswith("#  This program"):
                        contenu[0]="#  "+headerdb["openscience","header"]
                    elif contenu[0].startswith("#  Ce programme"):
                        contenu[0]="#  "+headerdb["openscience","header"]
                    else:
                        contenu[0]="#  "+headerdb["openscience","header"]+"\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("#  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("#  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("#  "+headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("#  "+headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n"
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["openscience","footer"][num]
                            num=num+1
                        
                    mapping += ' / OPENSCIENCE'
                 
                 
                ##### Pas de licence #####
                
                elif 'NONE' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[0].startswith("#  This program is under CECILL_B"):
                        contenu.remove("#  "+headerdb["cecill","header"])
                    elif contenu[0].startswith("#  This program is under OpenScience"):
                        contenu.remove("#  "+headerdb["openscience","header"])
                    elif contenu[0].startswith("#  Ce programme"):
                        contenu.remove("#  "+headerdb["accordAVBP","header"])
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("#  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("#  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("#  "+headerdb["cecill","footer"][num])
                            num=num+1
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("#  "+headerdb["openscience","footer"][num])
                            num=num+1
                            
                    mapping += ' / NONE'
                    
                else:
                    mapping += ' / PRE NONE'
                    
                ##### Ecriture #####
                
                mapping += '\n'
                    
                for k in range(len(contenu)):
                    contenu[k]=contenu[k]+'\n'
                    contenu3+=contenu[k]
                    
# Extension Fortran
                        
            elif fileExtension[i]=='.f90':
                
                _fortran=_fortran+1
                
                mapping += completeName[i]+" "*(95-len(completeName[i]))+fileExtension[i]+" "*(15-len(fileExtension[i]))
                
                ##### CERFACS #####
                
                if 'CERFACS' in mapp[ii]:
                    
                    ###### Header #####
                    
                    if contenu[0].startswith("!  This program"):
                        contenu[0]="!  "+headerdb["accordAVBP","header"]
                    elif contenu[0].startswith("!  Ce programme"):
                        contenu[0]="!  "+headerdb["accordAVBP","header"]
                    else:
                        contenu[0]="!  "+headerdb["accordAVBP","header"]+"\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("!  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("!  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("!  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("!  "+headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("!  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("!  "+headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n"
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                        
                    mapping += ' / CERFACS'
                    
                    
                ##### CECILL #####
                            
                elif 'CECILL' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[0].startswith("!  This program"):
                        contenu[0]="!  "+headerdb["cecill","header"]
                    elif contenu[0].startswith("!  Ce programme"):
                        contenu[0]="!  "+headerdb["cecill","header"]
                    else:
                        contenu[0]="!  "+headerdb["cecill","header"]+"\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("!  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("!  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("!  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("!  "+headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("!  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("!  "+headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n"
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["cecill","footer"][num]
                            num=num+1
                        
                    mapping += ' / CECILL'
                    
                    
                ##### OPENSCIENCE #####
                
                elif 'OPENSCIENCE' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[0].startswith("!  This program"):
                        contenu[0]="!  "+headerdb["openscience","header"]
                    elif contenu[0].startswith("!  Ce programme"):
                        contenu[0]="!  "+headerdb["openscience","header"]
                    else:
                        contenu[0]="!  "+headerdb["openscience","header"]+"\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("!  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("!  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("!  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("!  "+headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("!  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("!  "+headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n"
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n!  "+headerdb["openscience","footer"][num]
                            num=num+1
                        
                    mapping += ' / OPENSCIENCE'
                    
                ##### Pas de licence #####
                
                elif 'NONE' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[0].startswith("!  This program is under CECILL_B"):
                        contenu.remove("!  "+headerdb["cecill","header"])
                    elif contenu[0].startswith("!  This program is under OpenScience"):
                        contenu.remove("!  "+headerdb["openscience","header"])
                    elif contenu[0].startswith("!  Ce programme"):
                        contenu.remove("!  "+headerdb["accordAVBP","header"])
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("!  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("!  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                            
                    elif contenu[len(contenu)-1].startswith("!  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("!  "+headerdb["cecill","footer"][num])
                            num=num+1
                            
                    elif contenu[len(contenu)-1].startswith("!  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("!  "+headerdb["openscience","footer"][num])
                            num=num+1
                            
                    mapping += ' / NONE'
                    
                else:
                    mapping += ' / PRE NONE'
                            
                ##### Ecriture #####
                
                mapping += '\n'
                    
                for k in range(len(contenu)):
                    contenu[k]=contenu[k]+'\n'
                    contenu3+=contenu[k]
                    
                    
                    
########## Extension tcl ##########
                
            elif fileExtension[i]=='.tcl':
                
                _tcl=_tcl+1
                
                mapping += completeName[i]+" "*(95-len(completeName[i]))+fileExtension[i]+" "*(15-len(fileExtension[i]))
                
                ##### CERFACS #####
                
                if 'CERFACS' in mapp[ii]:
                    
                    ###### Header #####
                    
                    if contenu[0].startswith("#  This program"):
                        contenu[0]="#  "+headerdb["accordAVBP","header"]
                    elif contenu[0].startswith("#  Ce programme"):
                        contenu[0]="#  "+headerdb["accordAVBP","header"]
                    else:
                        contenu[0]="#  "+headerdb["accordAVBP","header"]+"\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("#  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("#  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("#  "+headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("#  "+headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n"
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["accordAVBP","footer"][num]
                            num=num+1
                        
                    mapping += ' / CERFACS'
                    
                    
                ##### CECILL #####
                            
                elif 'CECILL' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[0].startswith("#  This program"):
                        contenu[0]="#  "+headerdb["cecill","header"]
                    elif contenu[0].startswith("#  Ce programme"):
                        contenu[0]="#  "+headerdb["cecill","header"]
                    else:
                        contenu[0]="#  "+headerdb["cecill","header"]+"\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("#  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("#  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("#  "+headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("#  "+headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n"
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["cecill","footer"][num]
                            num=num+1
                        
                    mapping += ' / CECILL'
                    
                    
                ##### OPENSCIENCE #####
                
                elif 'OPENSCIENCE' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[0].startswith("#  This program"):
                        contenu[0]="#  "+headerdb["openscience","header"]
                    elif contenu[0].startswith("#  Ce programme"):
                        contenu[0]="#  "+headerdb["openscience","header"]
                    else:
                        contenu[0]="#  "+headerdb["openscience","header"]+"\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("#  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("#  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("#  "+headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("#  "+headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n"
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n#  "+headerdb["openscience","footer"][num]
                            num=num+1
                        
                    mapping += ' / OPENSCIENCE'
                
                ##### Pas de licence #####
                
                elif 'NONE' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[0].startswith("#  This program is under CECILL_B"):
                        contenu.remove("#  "+headerdb["cecill","header"])
                    elif contenu[0].startswith("#  This program is under OpenScience"):
                        contenu.remove("#  "+headerdb["openscience","header"])
                    elif contenu[0].startswith("#  Ce programme"):
                        contenu.remove("#  "+headerdb["accordAVBP","header"])
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-1].startswith("#  accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove("#  "+headerdb["accordAVBP","footer"][num])
                            num=num+1
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove("#  "+headerdb["cecill","footer"][num])
                            num=num+1
                            
                    elif contenu[len(contenu)-1].startswith("#  knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove("#  "+headerdb["openscience","footer"][num])
                            num=num+1
                            
                    mapping += ' / NONE'
                    
                else:
                    mapping += ' / PRE NONE'
                    
                ##### Ecriture #####
                
                mapping += '\n'
                    
                for k in range(len(contenu)):
                    contenu[k]=contenu[k]+'\n'
                    contenu3+=contenu[k]
                    
# Extension html
            
            elif fileExtension[i]=='.html':
                
                _html=_html+1
                
                mapping += completeName[i]+" "*(95-len(completeName[i]))+fileExtension[i]+" "*(15-len(fileExtension[i]))
                
                ##### CERFACS #####
                
                if 'CERFACS' in mapp[ii]:
                    
                    ###### Header #####
                    
                    if contenu[0].startswith("<!--  This program"):
                        contenu[0]="<!--  "+headerdb["accordAVBP","header"]+"  -->"
                    elif contenu[0].startswith("<!--  Ce programme"):
                        contenu[0]="<!--  "+headerdb["accordAVBP","header"]+"  -->"
                    else:
                        contenu[0]="<!--  "+headerdb["accordAVBP","header"]+"  -->\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-2].startswith("accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove(headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+"\n"+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove(headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+"\n"+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove(headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+"\n"+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n\n<!--"
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n"+headerdb["accordAVBP","footer"][num]
                            num=num+1
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n-->"
                        
                    mapping += ' / CERFACS'
                    
                    
                ##### CECILL #####
                            
                elif 'CECILL' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[0].startswith("<!--  This program"):
                        contenu[0]="<!--  "+headerdb["cecill","header"]+"  -->"
                    elif contenu[0].startswith("<!--  Ce programme"):
                        contenu[0]="<!--  "+headerdb["cecill","header"]+"  -->"
                    else:
                        contenu[0]="<!--  "+headerdb["cecill","header"]++"  -->\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-2].startswith("accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove(headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+"\n"+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove(headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+"\n"+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove(headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+"\n"+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n\n<!--"
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n"+headerdb["cecill","footer"][num]
                            num=num+1
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n-->"
                        
                    mapping += ' / CECILL'
                    
                    
                ##### OPENSCIENCE #####
                
                elif 'OPENSCIENCE' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[0].startswith("<!--  This program"):
                        contenu[0]="<!--  "+headerdb["openscience","header"]+"  -->"
                    elif contenu[0].startswith("#  Ce programme"):
                        contenu[0]="<!--  "+headerdb["openscience","header"]+"  -->"
                    else:
                        contenu[0]="<!--  "+headerdb["openscience","header"]+"  -->\n\n"+contenu[0]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-2].startswith("accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove(headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+"\n"+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove(headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+"\n"+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove(headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+"\n"+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n\n<!--"
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n"+headerdb["openscience","footer"][num]
                            num=num+1
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n-->"
                        
                    mapping += ' / OPENSCIENCE'
                    
                ##### Pas de licence #####
                
                elif 'NONE' in mapp[ii]:
                    
                    ##### Header #####

                    if contenu[0].startswith("<!--  This program is under CECILL_B"):
                        contenu.remove("<!--  "+headerdb["cecill","header"]+"  -->")
                    elif contenu[0].startswith("<!--  This program is under OpenScience"):
                        contenu.remove("<!--  "+headerdb["openscience","header"]+"  -->")
                    elif contenu[0].startswith("<!--  Ce programme"):
                        contenu.remove("<!--  "+headerdb["accordAVBP","header"]+"  -->")
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-2].startswith("accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove(headerdb["accordAVBP","footer"][num])
                            num=num+1
                        contenu[len(contenu)-2]=""
                        contenu[len(contenu)-1]=""
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove(headerdb["cecill","footer"][num])
                            num=num+1
                        contenu[len(contenu)-2]=""
                        contenu[len(contenu)-1]=""
                        
                    elif contenu[len(contenu)-1].startswith("knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove(headerdb["openscience","footer"][num])
                            num=num+1
                        contenu[len(contenu)-2]=""
                        contenu[len(contenu)-1]=""
                        
                    mapping += ' / NONE'
                    
                else:
                    mapping += ' / PRE NONE'
                            
                ##### Ecriture #####
                
                mapping += '\n'
                    
                for k in range(len(contenu)):
                    contenu[k]=contenu[k]+'\n'
                    contenu3+=contenu[k]
                    
# Extension XML
                       
            elif fileExtension[i]=='.xml':
                
                _XML=_XML+1
                
                mapping += completeName[i]+" "*(95-len(completeName[i]))+fileExtension[i]+" "*(15-len(fileExtension[i]))
                
                # Si declaration xml, pas de commentaires avant
                
                gg=0
                if '?xml' in contenu[0]:
                        gg=1
                
                ##### CERFACS #####
                
                if 'CERFACS' in mapp[ii]:
                    
                    ###### Header #####
                     
                    if contenu[gg+1].startswith("<!--  This program"):
                        contenu[gg+1]="<!--  "+headerdb["accordAVBP","header"]+"  -->"
                    elif contenu[gg+1].startswith("<!--  Ce programme"):
                        contenu[gg+1]="<!--  "+headerdb["accordAVBP","header"]+"  -->"
                    else:
                        contenu[gg]="\n<!--  "+headerdb["accordAVBP","header"]+"  -->\n\n"+contenu[gg]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-2].startswith("accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove(headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+'\n'+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove(headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+'\n'+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove(headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+'\n'+headerdb["accordAVBP","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n\n<!--"
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n"+headerdb["accordAVBP","footer"][num]
                            num=num+1
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n-->"
                        
                    mapping += ' / CERFACS'
                    
                    
                ##### CECILL #####
                            
                elif 'CECILL' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[gg+1].startswith("<!--  This program"):
                        contenu[gg+1]="<!--  "+headerdb["accordAVBP","header"]+"  -->"
                    elif contenu[gg+1].startswith("<!--  Ce programme"):
                        contenu[gg+1]="<!--  "+headerdb["accordAVBP","header"]+"  -->"
                    else:
                        contenu[gg]="\n<!--  "+headerdb["accordAVBP","header"]+"  -->\n\n"+contenu[gg]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-2].startswith("accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove(headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+'\n'+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove(headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+'\n'+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove(headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+'\n'+headerdb["cecill","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n\n<!--"
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n"+headerdb["cecill","footer"][num]
                            num=num+1
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n-->"
                        
                    mapping += ' / CECILL'
                    
                    
                ##### OPENSCIENCE #####
                
                elif 'OPENSCIENCE' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[gg+1].startswith("<!--  This program"):
                        contenu[gg+1]="<!--  "+headerdb["accordAVBP","header"]+"  -->"
                    elif contenu[gg+1].startswith("<!--  Ce programme"):
                        contenu[gg+1]="<!--  "+headerdb["accordAVBP","header"]+"  -->"
                    else:
                        contenu[gg]="\n<!--  "+headerdb["accordAVBP","header"]+"  -->\n\n"+contenu[gg]
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-2].startswith("accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove(headerdb["accordAVBP","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+'\n'+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove(headerdb["cecill","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+'\n'+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove(headerdb["openscience","footer"][num])
                            num=num+1
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-2]=contenu[len(contenu)-2]+'\n'+headerdb["openscience","footer"][num]
                            num=num+1
                            
                            
                    else:
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n\n\n<!--"
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n"+headerdb["openscience","footer"][num]
                            num=num+1
                        contenu[len(contenu)-1]=contenu[len(contenu)-1]+"\n-->"
                        
                    mapping += ' / OPENSCIENCE'
                    
                ##### Pas de licence #####
                
                elif 'NONE' in mapp[ii]:
                    
                    ##### Header #####
                    
                    if contenu[gg+1].startswith("<!--  This program is under CECILL_B"):
                        contenu.remove("<!--  "+headerdb["cecill","header"]+"  -->")
                    elif contenu[gg+1].startswith("<!--  This program is under OpenScience"):
                        contenu.remove("<!--  "+headerdb["openscience","header"]+"  -->")
                    elif contenu[gg+1].startswith("<!--  Ce programme"):
                        contenu.remove("<!--  "+headerdb["accordAVBP","header"]+"  -->")
                        
                    ##### Footer #####
                    
                    if contenu[len(contenu)-2].startswith("accepte"):
                        num=0
                        for line in headerdb["accordAVBP","footer"]:
                            contenu.remove(headerdb["accordAVBP","footer"][num])
                            num=num+1
                        contenu[len(contenu)-2]=""
                        contenu[len(contenu)-1]=""
                            
                    elif contenu[len(contenu)-2].startswith("knowledge of the CeCILL-B"):
                        num=0
                        for line in headerdb["cecill","footer"]:
                            contenu.remove(headerdb["cecill","footer"][num])
                            num=num+1
                        contenu[len(contenu)-2]=""
                        contenu[len(contenu)-1]=""
                        
                    elif contenu[len(contenu)-1].startswith("knowledge of the OpenScience"):
                        num=0
                        for line in headerdb["openscience","footer"]:
                            contenu.remove(headerdb["openscience","footer"][num])
                            num=num+1
                        contenu[len(contenu)-2]=""
                        contenu[len(contenu)-1]=""
                        
                    mapping += ' / NONE'
                    
                else:
                    mapping += ' / PRE NONE'
                    
                ##### Ecriture #####
                
                mapping += '\n'
                    
                for k in range(len(contenu)):
                    contenu[k]=contenu[k]+'\n'
                    contenu3+=contenu[k]
                    
# Fichiers sans extension ou autre que voulue
            
            else:
                _others=_others+1
                mapping += completeName[i]+" "*(95-len(completeName[i]))+fileExtension[i]+" "*(15-len(fileExtension[i]))+' / NONE\n'
                
            contenu3=contenu3[0:-1]
            fichier=open(fileName[i]+fileExtension[i],"w")
            fichier.write(contenu3)
            fichier.close()
                
            ii=ii+1
            
# Si rajout de fichiers
           
        else:
            mapping += completeName[i]+" "*(95-len(completeName[i]))+fileExtension[i]+" "*(15-len(fileExtension[i]))+' / PRE NONE\n'
            
# Ecriture du mapping
    
    mapping += '\n\nFiles : \t\t\t\t\t\tLicenses :\n\t- f90 = '+str(_fortran)+' files\t\t\t\t\t\t- Cerfacs = '+str(_cerfacs)+' files\n\t- py = '
    mapping += str(_python)+' files\t\t\t\t\t\t- CeCILL-B = '+str(_cecill)+' files\n\t- XML = '+str(_XML)+' files\t\t\t\t\t\t- Openscience = '
    mapping += str(_openscience)+' files\n\t- tcl = '+str(_tcl)+' files\n\t- html = '+str(_html)+' files\n\t- Others = '+str(_others)+' files\n\nTOTAL = '
    mapping += str(_files)+' files\n\nInfo : PRE --> There still is a header in this file. Please, execute again mapping\n'
    mapping=mapping[0:-1]
    fichier=open('mapping.txt',"w")
    fichier.write(mapping)
    fichier.close()
    
else:
    for i in range(len(name)):
        (fileName[i], fileExtension[i]) = os.path.splitext(name[i])
        completeName[i]=path + '/' + fileName[i]
        mapping += completeName[i]+" "*(95-len(completeName[i]))+fileExtension[i]+" "*(15-len(fileExtension[i]))+' / PRE NONE\n'
    fichier=open('mapping.txt',"w")
    fichier.write(mapping)
    fichier.close()






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