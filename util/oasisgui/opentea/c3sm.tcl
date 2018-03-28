#  Ce programme depend des accords l'ACCORD DE COOPERATION AVBP (N. IFP 31.293). Voir la fin du programme pour plus de details.



global additionalWidgets


set pathEngine [file normalize [file dirname [info script]]]


# Presonalization of C3Sm 
image create photo icon_gui_small -file [file join $pathEngine IMAGES_PRIV logo_c3sm_small.gif]
image create photo icon_gui_tiny -file [file join $pathEngine IMAGES_PRIV logo_c3sm_tiny.gif]

# icones d'application
image create photo icon_hmg -file [file join $pathEngine IMAGES_PRIV logo_hmg.gif]
image create photo icon_mspec -file [file join $pathEngine IMAGES_PRIV logo_mspec.gif]
image create photo icon_flammable -file [file join $pathEngine IMAGES_PRIV logo_flammable.gif]
image create photo icon_calc -file [file join $pathEngine IMAGES_PRIV logo_calc.gif]
image create photo icon_flow -file [file join $pathEngine IMAGES_PRIV logo_flow.gif]
image create photo icon_flowtpf -file [file join $pathEngine IMAGES_PRIV logo_flowtpf.gif]


# for the config and the footer of the app
# entreprises

image create photo icon_coria -file [file join $pathEngine IMAGES_PRIV logo_coria.gif]
image create photo icon_imft -file [file join $pathEngine IMAGES_PRIV logo_imft.gif]
image create photo icon_em2c -file [file join $pathEngine IMAGES_PRIV logo_em2c.gif]
image create photo icon_ifpen -file [file join $pathEngine IMAGES_PRIV logo_ifpen.gif]

# snecma
image create photo icon_snecma -file [file join $pathEngine IMAGES_PRIV logo_snecma.gif]
image create photo icon_turbomeca -file [file join $pathEngine IMAGES_PRIV logo_turbomeca.gif]
image create photo icon_herakles -file [file join $pathEngine IMAGES_PRIV logo_herakles.gif]
image create photo icon_gdtech -file [file join $pathEngine IMAGES_PRIV logo_gdtech.gif]



###############################
# Additionnal widget must be of the form "create_mywidget.tcl", stored in folder SOURCES_PRIV
# The node "mywidget" will be automatically recognized


# widget for the coolant GUI
lappend additionalWidgets "coolac"







source [file join $pathEngine "opentea.tcl"]



#  Copyright CERFACS-IFP-SAFRAN 2014
#   
#  antoine.dauptain@cerfacs.fr
#   
#  Ce logiciel est un programe informatique dont l'objectif est de proposer
#  des outils numeriques pour l'etude de la combustion, des turbomachines,
#  de l'acoustique, de la thermique et du rayonmment.
#   
#  Ce logiciel est gouverne par l'ACCORD DE COOPERATION AVBP (N. IFP 31.293)
#  sous loi francaise, et fait explicitement partie de la -version commune- d'AVBP.
#   
#  Ce logiciel est la copropriete de CERFACS, SAFRAN et l'IFPEN, nommees ci apres les Parties,
#  dans les termes de l'ACCORD DE COOPERATION AVBP.
#  En cela:
#  -  Chaque Partie pourra utiliser la -version commune- du code AVBP
#  incluant la modalite 1 et la modalite 2 pour ses besoins propres
#  de recherche librement, gratuitement, et sans limitation de duree.
#  Par besoins propres de recherche, on entend une utilisation en
#  interne de la -version commune- du code AVBP par une Partie pour
#  des activites commerciales.
#   
#  -le CERFACS et l'IFPEN pourront librement entreprendre l'exploitation
#  commerciale de la -version commune- du code AVBP incluant la modalite 1
#  et la modalite 2 en toute independance et gratuitement a l'egard de
#  l'autre partie, sous reserve de ne pas l'utiliser au profit de concurrents
#  de SAFRAN ni de la diffuser a des tiers sous quelque forme que ce soit.
#   
#  -SAFRAN pourra exploiter librement la -version commune- du code AVBP
#  incluant la modalite 1 dans le cadre de son activite d'etude et de
#  conception de ses produits, sous reserve de ne pas l'utiliser au profit
#  de concurrents du CERFACS et le l'IFP, ni de la diffuser a des tiers
#  sous quelque forme que ce soit.
#   
#  L'utilisation de la -version commune- du code AVBP sera aux seuls
#  risques de la partie exploitante et sous la seule responsabilite de
#  la dite Partie. La -version commune- et toute documentation eventuelle
#  y afferente sont fournis en l'etat, sans garantie d'aucune sorte.
#  Nottament, aucune garantie en ce qui concerne l'utilisation initerrompue
#  du code, ou les resultats obtenus lors de l'utilisation de la
#  -version commune-, que ce soit leur exactitude, fiabilite ou autre,
#  ou la jouissance paisible de la -version commune- n'est donnee.
#  En consequence chacune des parties renonce a tout recours vis a vis des
#  autres parties en ce qui concerne les consequences resultant de
#  l'utilisation de la -version commune-, de quelque nature que ce soit. 
#   
#  A cet egard  l'attention de l'utilisateur est attiree sur les risques
#  associes au chargement,  a l'utilisation,  a la modification et/ou au
#  developpement et a la reproduction du logiciel par l'utilisateur etant 
#  donne sa specificite de logiciel scientifique, qui peut le rendre complexe a 
#  manipuler et qui le reserve donc a des developpeurs et des professionnels
#  avertis possedant  des  connaissances  informatiques approfondies. Les
#  utilisateurs sont donc invites a charger et tester l'adequation  du
#  logiciel a leurs besoins dans des conditions permettant d'assurer la
#  securite de leurs systemes et ou de leurs donnees et, plus generalement, 
#  a l'utiliser et l'exploiter dans les memes conditions de securite. 
#   
#  Le fait que vous puissiez acceder a cet en-tete signifie que vous avez 
#  pris connaissance de l'ACCORD DE COOPERATION AVBP, et que vous en avez
#  accepte les termes.