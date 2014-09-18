#  This program is under CECILL_B licence. See footer for details.


# Engine icons
image create photo icon_cerfacs -file [file join $pathEngine IMAGES logo_cerfacs.gif]

# if these images are not set before hand, switch back to open tea.
if {"icon_gui_small" ni [image names] } {
    image create photo icon_gui_small -file [file join $pathEngine IMAGES logo_opentea_small.gif]    
}
if {"icon_gui_tiny" ni [image names] } {
    image create photo icon_gui_tiny -file [file join $pathEngine IMAGES logo_opentea_tiny.gif]    
}


image create photo icon_garfield -file [file join $pathEngine IMAGES garfield.gif]

# corner icons
image create photo icon_corner -file [file join $pathEngine IMAGES corner.gif]

# softs de visu
image create photo icon_paraview -file [file join $pathEngine IMAGES logo_paraview.gif]
image create photo icon_ensight -file [file join $pathEngine IMAGES logo_ensight.gif]
image create photo icon_grace -file [file join $pathEngine IMAGES logo_grace.gif]

image create photo icon_magnifierplus -file [file join $pathEngine IMAGES magnifierplus.gif]
image create photo icon_magnifierminus -file [file join $pathEngine IMAGES magnifierminus.gif]
image create photo icon_addrow -file [file join $pathEngine IMAGES icon_row_add.gif]
image create photo icon_delrow -file [file join $pathEngine IMAGES icon_row_delete.gif]
image create photo icon_graph -file [file join $pathEngine IMAGES icon_graph.gif]

image create photo bullet_1 -file [file join $pathEngine IMAGES bullet_1.gif]
image create photo bullet_2 -file [file join $pathEngine IMAGES bullet_2.gif]
image create photo bullet_3 -file [file join $pathEngine IMAGES bullet_3.gif]

image create photo icon_plugin_missing -file [file join $pathEngine IMAGES logo_plugin_missing.gif]
image create photo icon_plugin_ssh -file [file join $pathEngine IMAGES logo_plugin_ssh.gif]
image create photo icon_plugin_robot -file [file join $pathEngine IMAGES logo_plugin_robot.gif]
image create photo icon_plugin_local -file [file join $pathEngine IMAGES logo_plugin_local.gif]

image create photo bg_200 -file [file join $pathEngine IMAGES background200.gif]



###########################################
# Icons used for the appearance of widget.


# ok grey
image create photo icon_void -data "R0lGODlhFAAUAOcAAAAAAAEBAQICAgMDAwQEBAUFBQYGBgcHBwgICAkJCQoKCgsLCwwMDA0NDQ4ODg8PD
xAQEBERERISEhMTExQUFBUVFRYWFhcXFxgYGBkZGRoaGhsbGxwcHB0dHR4eHh8fHyAgICEhISIiI
iMjIyQkJCUlJSYmJicnJygoKCkpKSoqKisrKywsLC0tLS4uLi8vLzAwMDExMTIyMjMzMzQ0NDU1N
TY2Njc3Nzg4ODk5OTo6Ojs7Ozw8PD09PT4+Pj8/P0BAQEFBQUJCQkNDQ0REREVFRUZGRkdHR0hIS
ElJSUpKSktLS0xMTE1NTU5OTk9PT1BQUFFRUVJSUlNTU1RUVFVVVVZWVldXV1hYWFlZWVpaWltbW
1xcXF1dXV5eXl9fX2BgYGFhYWJiYmNjY2RkZGVlZWZmZmdnZ2hoaGlpaWpqamtra2xsbG1tbW5ub
m9vb3BwcHFxcXJycnNzc3R0dHV1dXZ2dnd3d3h4eHl5eXp6ent7e3x8fH19fX5+fn9/f4CAgIGBg
YKCgoODg4SEhIWFhYaGhoeHh4iIiImJiYqKiouLi4yMjI2NjY6Ojo+Pj5CQkJGRkZKSkpOTk5SUl
JWVlZaWlpeXl5iYmJmZmZqampubm5ycnJ2dnZ6enp+fn6CgoKGhoaKioqOjo6SkpKWlpaampqenp
6ioqKmpqaqqqqurq6ysrK2tra6urq+vr7CwsLGxsbKysrOzs7S0tLW1tba2tre3t7i4uLm5ubq6u
ru7u7y8vL29vb6+vr+/v8DAwMHBwcLCwsPDw8TExMXFxcbGxsfHx8jIyMnJycrKysvLy8zMzM3Nz
c7Ozs/Pz9DQ0NHR0dLS0tPT09TU1NXV1dbW1tfX19jY2NnZ2dra2tvb29zc3N3d3d7e3t/f3+Dg4
OHh4eLi4uPj4+Tk5OXl5ebm5ufn5+jo6Onp6erq6uvr6+zs7O3t7e7u7u/v7/Dw8PHx8fLy8vPz8
/T09PX19fb29vf39/j4+Pn5+fr6+vv7+/z8/P39/f7+/v///yH+GkNyZWF0ZWQgd2l0aCBHSU1QI
G9uIGEgTWFjACH5BAEKAP8ALAAAAAAUABQAAAj+AP8JHDiwWDGCCBM+uwZu3Lhv154lJKgMmzhw3
zJ+AxfuGrKJy7qF8+ZNo7du38x9W4bwmLaRGUt+41auHr512Y4RnDZS5reT4+z1mwcOHLWBylCSX
NoN3Lx+9MJxm6lM4DOYJru144fPnExwzgRKK0oWnLdz+fSd4xauLbhoYt0W/Tau3j511Kpl83YRm
sBpF8e9Y/fN3T541qRRm1ZNGzeJ/66C21pPnb173RRXo7b4WjOBFbGVw3fvnlppjPNutlZVYDRq1
+rp0wfvWurNsOEizasuXz1v01Rzrlat9UBm1a592xac+OJq1phNZGYN22LOebVdYznxnypkerkUc
ctWDVmp7gQdoWrFypQi9PC7BwQAOw=="
# error red flag
image create photo icon_error  -data "R0lGODlhFAAUAOfsALAAALMAALQAALUAALYAALcAALgAALkAALoAALwAAL0AAL4AALsBAb8AAMIAAMA
BAb8CAr0EBIcUFJYREb8HB3kaGnsaGnkcHMEJCcIJCcsHB8wHB8sICJAZGbkODqIVFcUMDK8SEsI
NDbYREdAKCroQEMIODrsQEL8PD7kREbYSErwSEsAREb0SEr8SEsESErQWFrIXF7kYGM4TE84WFr0
bG88XF6cjI84ZGccgIMkgIMoiIq4xMc0pKckrK78xMdcrK9UsLMovL9YsLNMtLb01NdguLuUrK9Y
wMNovL80zM9UxMdYxMc8zM800NMI3N9kxMc40NM80NNMzM9cyMsw1NeQvL9czM+ouLuQwMMM5Ods
zM803N843N9Y1Ndo0NNE3N9o1Ndg2Ntc3N9Y4ONo3N+kzM9c4ONg4OOk0NMQ+Ptg5Odw4ONk5OeY
2Ntg6Otk6OtY7O9c7O+c3N79CQtk7O9c8PNg8PNg9PcZCQtk9Peg5Ob1FRds9Pdg+PsZDQ9k+Ptg
/P90+Pto/P8hERNdAQNhAQNlAQMhFRddBQdhBQdlBQeg9PdJDQ9ZCQtpBQchGRthCQsJISNlCQsh
HR9dDQ9hDQ9JFRdpDQ8lISNhERMpISNlERNpERMlJSeNCQsdKSsZLS9lGRtpGRspLS+1CQtlISMd
NTedFRcdOTt1JScdPT8hQUN9KSshRUclRUeBLS+NPT+hPT+FUVOFXV+JXV+FYWOFZWexZWetbW+x
paexra+h1deyBgeuCguiGhumIiPKGhuyKivCOjvCSkvKgoOejo+ukpO+lpfKnp/Krq/Orq+SwsPS
trfKwsPOysuPDw/PBwfjKyvXNzeTS0u3Q0PXR0enV1e3k5OLr6+js7Obw8ODy8vju7vHz8/fz8+r
4+Or7+97///n4+PL6+uT+/u78/O7///P///T///X///j////////////////////////////////
//////////////////////////////////////////////////yH+GkNyZWF0ZWQgd2l0aCBHSU1
QIG9uIGEgTWFjACH5BAEKAP8ALAAAAAAUABQAAAj+AP8JHMiH1apUp0KBojOwYcNXtQY9OgRIzxp
Ymxw2dGULUydMkyjiedMKkcZ/r25xOrRIFKdJiuRcGaMqj8NXtCxFSmMGkChNcbJgoQJmlJqBkmY
ZasOImbE9gRxZAQZNVhAplYoIJNUnUZhS1MQdm3NEGLdsu4AoaaJFoKc6gQqxwXWtHDJi27T5+hK
Fi5MnAjPBweOn0JZc1cKBw9YLSpcqQnz8EEgJjZw7dqa4eWbunDVUIERgoBBBhkBIYs6QWfIpmbd
v3chFK5PgAAIDIwQSYuIFSSxn6NYN09Ut3TRTCQwUUCHwjyAiRoIFL8bBAa9x6pplCGCA+T8el5J
g0BiibNmMBgoc/JLWaAABFCEG3uhBwoENHA0eQEBgQocAAizAoNEOGyiwQAMLJMAAAQAY8EIMJ3W
QgwYIJoDAAQQwwMIHJwkkQQ0utLDCCSV4kMIEHWp0QQUWpOhiigEBADs="
# error red octogon
image create photo icon_flag -data "R0lGODlhEAAQAMZpAIQAAIkAAIsAAIwAAI4AAI8AAJIAAJUDAZQEA5gEA5gFA5kFA5oFBJ4LB6EOCaYOC
qYPCqgPCqoUDakXD7MYELQYELQYEbYZEaodE6oiFLogFcAhF8EhF8AiF8EiF8UjGM0pHM0qHM4qH
M4rHc0sH9IsH9wuHdk0JPMvGts2Jtw2Jdw3JuY3HNw7Jv8yHvc0Iv8zHvs0Idw9Lf80IN09Lf81I
d0/MOJBJ+o/LOJEJ9VGO+lCL/9DLf9ELuNMO/9FL/9GMPlJMrxbUupQPv9LNvZOOMRbTv9NNf9QO
dNeVf9UQPdcR+xfT/9bRf9jUfdpVu5sXf9qVtZ1bO5zZe93avh1ZP51ZOl7bf97aPmBcdiLhP+Je
fqRg9agm/eZjP+Yi/6kl9Oyr9LBwNXQ0NjZ29/f39/m5+jz8/X29v///////////////////////
////////////////////////////////////////////////////////////////////yH+GkNyZ
WF0ZWQgd2l0aCBHSU1QIG9uIGEgTWFjACH5BAEKAH8ALAAAAAAQABAAAAeVgH9of4SFhodmQihWY
F5XRoeFZzo1Tl9cVCYkGhINB4ZJM0pbWVMpIRwVDwkBhVIwRFhVUDIgGxQPCq2EWi5AUU9MNra4C
wKFXS48TUtDNCIdFRALA4VhMT1IRTsrIx4W0wSFYi8/R0E4Kt7gCwWFYz4sOTctJyUfFxEMBgCEZ
JEZMExwgGDXnzKREipcyLChw4d/AgEAOw=="
# ok green
image create photo icon_ok -data "R0lGODlhFAAUAOfVAE11GU95GlyRG1uTGl2VGl+VHmCeGmGeGmyyKGy1IG21JXG3J3K4IXK4JHG5IXG5InS
3LnO5Ina5JXS6I3O6J3O6KHS6Kna6L3e7J3i8JXS/IXe+KHnAK3nBK3rBMHnDJ33BOH7BOnfGInn
GImjPA2rPBn7FL37FMGvQCWzQCX/GNoHGOYLGOXrLIHrLIW/QEW/QE2zSCW7REG/RD2/REGzTB3D
RE3DRFHHRE3HRFIHJNW/SEYHJN3LRFHLRFXLRFnDSE3rOHnPRFm/TD3LSFXnPH3PSFnPSF3zPInL
UFH7PI3LUFXrRHnPUFX7RInnTInbWEnfVGXvTJnnVG3vUJInNRXrVInrWIIDUI4jPPoHTK4rOR37
VKXzYHonSO4zQR4PYJIPYJYPXMIzTRIrWPoXZM4fZN4/WQ4nZO4jaOpHWSo/YRYvaPYvaP4zaQJD
YSY3bQY7bQo/bRZDbRZDbRpTZT5DbR5HbR5HbSJHbSZDcRpDcR5HcSJLcR5LcSZLcSpvYWpPcSpP
cS5faVJPcTJPcTZTcTZLdSZTcTpLdSpHeRZXcTpjaWZPdTJLeSZjbVpTdT5XdTZPeSpTeS5bdUJT
eTZXeS5fdUZfdUpbeTJbeTZTfSpTfS5XfTZfeUpjeVJneVJffUpnfU5veWZrfVpzfWJ7gWp7gW6D
fYKDfYqHfZajhcavgdazidqrkcLPghbHigbDjfrzjlb3jl7/jmrjnisLonMXoosfopsrorczqrdP
ovtboxNnsxtrsyOHu0+Lu1eTu2ent4+Xv2+nt5Ort5u/u7e/u7u/u7+7x6vLv8+/x7Pbv+vXy9vr
w//fy+v3y//7y//31//71///1///////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////yH+GkNyZWF0ZWQgd2l0aCBHSU1QIG9
uIGEgTWFjACH5BAEKAP8ALAAAAAAUABQAAAj+AP8JHDgQAQKCCBOG2PLolKlBVUAkJAjhiyhJevZ
odKQpy4KJF9R0kmOHj0k6bAi5AmQBoYIxm+jg4VOST5pVwJLV8pKAIA9Oduxo3GPHjaph1HxBUqR
jYIU1h+zgwXMHTxtMvaj9GoUm0ZkGAj1kyvOn7B85bnBNW9YqjqBCljgIVBEJkSE/gRbBidXsGSw
zl0B9mnRCIItQlAI1otQHVTBotHAQ4UKnVCUTAleQupRKl605uaLxelICB40jYsp8ENjBkqdb0oL
JKmbsDYkeRnzgeDFFg8AHWqywUobsmLNXJWbkNiJkh5MIA0XUkCLMmbNdVFAsF5IjygiCFIpODJn
FjFidFD6MMM+RBMkEhBuYXGFEZkaPI0J+wGjiJMNEDEFAEYMMN9gAxBJdKCHBRAIV4EALWIARBhY
uMCAAgwQFQMABBgwAAIYgMhgQADs="
# question mark orange
image create photo icon_question -data "R0lGODlhFAAUAOexAHhSHnxVIJViIJthHJ1jHJllIadnGahnGbJxG7p0Hbp1Hrp4I7t4I/NrAPNtA
PRtAMN6IcN6IsB7Jsl5IMF7JMB7J8p5IPRwAMN8I/RxAMN8JfVxAM96G/RyAMR9KPRzAMV9KPN0A
PN1APR1AMl+J/Z1APN2AMh/KPR2APV2APR3APV3AMmAKvN4APR4APR5APR5AfV5ANx+GfV5Afl4A
M+BJ9x/HM2CK/p5APt5ANWDKPV9COSBGfV+Cvt9A/l/CvWADuyCGNWHMO2DGemEHNWIMdWIM/WCE
vCDGPKDF9WJM+qFHfWDE/aDFP2CDdqJMf2DDfeFFvSGGPSGGfWGGtyMNfuIGfaJH/KKJOGPOeGQO
faNJvmNJfaOKPeOKfeQLOqTO+qTPPeRLfeSLveTMfeUMuyWP/eVNveWNveWN/eWOPCYP/CYQPeXO
PeYOveYO/eZPPeZPfiZPPmZO/mZPPeaPveaP/iaPviaP/maPfebQPebQfibQPibQfubPvecQvecQ
/icQ/qcQPucQfycP/ucQvidRPidRfudQvieR/qeRPqeRfueQ/2eQvmfSP6eRPyfRfegSv2fRP6fR
PinV/mnVvioWviqXvmrX/mxa/mzbvq4d/q4ePq7fvq/hvrAh/rDjvvLnfvSqvzSqfzXs/zZt/zew
Pzfw/zgxP3o0v3o0/3p1f3q2P3s2v3u3/7w4/727////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////yH+GkNyZWF0ZWQgd2l0aCBHS
U1QIG9uIGEgTWFjACH5BAEKAP8ALAAAAAAUABQAAAj+AP8JHDgQAQKCCBOyqMLGkSM2VVgkJMhAi
yI/dObMyTNoUZYFEyuYmSTnTBo1akzeeWRGAsIEYRqhuVQq1atVoyqp0dMIjAKCTyTZ+fIpllGjr
Ci1sSPpyUAKbATZIcNJFahNpIx2GmNH0BoKAm9AssOnTiI8Yq5o0kqGjx1INwQKKUSWT5w4ZTC5i
mXKDRxDfxAJEWhkkJ3Dfd5kghUL1RUUR8b8YaREYBFEZAMd8iIqVismD1CgiNGEiw6BNQgB6pOmy
44te7Z0UBHDhYoRPzgIhIAlCpUeKjJE8mQJhQoXMWCUSBJhoIwcIVTMaHAqFqwOI2K8UOHDBkENQ
2hNrIDxIBTfDyherKARBAPCE0hwmGixA0iPFy1E4EBCYiIIIk7gsIEDF6SAAxRLeDCRQAVYwMMUV
lghBQ8TCLAgQQEQcIABAwBw4YcLBgQAOw=="

# question blue
image create photo icon_info -data "R0lGODlhEgASAOZ/AOLl6hZCu8zb6hhWyOXy/Mzk93ip5JjN9ESV5TBIuPL2+zZkxzd71YOVyUes88nJy
erq66nT93m78y6M5TSb79Lk8ll5yDSB3rvI4arK6azE4+nr7meO1ws3t6XI6kej7iBq09rp+Rtfz
Tp0z4i97WeW3Tmc7evs8RNKwsPP7sHM4Sh+342v38XK3F2Y3CNVw9He6zGV687T3iR02WtwmSyF4
tnl8Sd63WSz8tbd55WVnAclrNjY2snJ1onL9XDB9YzC7U6x9Dyg8Bsjh////+bm59TU1LS0tAkTl
YaGleDg4FOP1x8moEmE2oqk1014ycje8CAnn3+66p2izW+47/Dv8NXn+Hqn39fo+Vqv8q/Q7ISu4
Yii3iNy2JOk3ezs7y1z1tno91htwTiT6GOq7AguslGa4bHW9n6NxRUxrhg3srfD3oah1jaP5rLL5
r7G4Im+8KCkztTV2dfY24K254235Gah4Obn62mv7fDz9tjf6Chiy+Ps+XO573qx6f///yH5BAEAA
H8ALAAAAAASABIAAAf/gH+CghAwWkBAWjAQg42CAn1BPz4+P0FUAo6CHg4HeQohIQp5Bw4ejhkOF
Vg4QhQUQjhWFaaDMB9QZyYUWQcHEhQmEVAfmX90UmG7EUQEBUQFMSZWJH53Agg2eDEfRCETE3BEE
jFkNghuGggbY+AICCsrfkQkE21VZnVbLhsTNSs3ZnQxoEDBhRU1TtgxwGLJnRoBQYAoQYRPExAzb
nxxcUUDAwAMuoAQMSAFkQUDRIAAU4QBixwjVJQYOQAFBy4oUKTkoGIEhj9OnujZUzOAyQ4BULzIY
YGNoDsLGmB4EaCDlxRlkK5psADAIBUB0MiwoGbHjjRiZDQI8MZRiwQJLqb04MGjRxy4LRpBKAJhD
o0hSJhEQTKEhpw/fAkpMfLgwREdSZLoONLYiBJGgQAAOw=="

# checked icon for selection
image create photo icon_checked -data "R0lGODlhDgAOAOcAAAAAAAEBAQICAgMDAwQEBAUFBQYGBgcHBwgICAkJCQoKCgsLCwwMDA0NDQ4ODg
8PDxAQEBERERISEhMTExQUFBUVFRYWFhcXFxgYGBkZGRoaGhsbGxwcHB0dHR4eHh8fHyAgICEhIS
IiIiMjIyQkJCUlJSYmJicnJygoKCkpKSoqKisrKywsLC0tLS4uLi8vLzAwMDExMTIyMjMzMzQ0ND
U1NTY2Njc3Nzg4ODk5OTo6Ojs7Ozw8PD09PT4+Pj8/P0BAQEFBQUJCQkNDQ0REREVFRUZGRkdHR0
hISElJSUpKSktLS0xMTE1NTU5OTk9PT1BQUFFRUVJSUlNTU1RUVFVVVVZWVldXV1hYWFlZWVpaWl
tbW1xcXF1dXV5eXl9fX2BgYGFhYWJiYmNjY2RkZGVlZWZmZmdnZ2hoaGlpaWpqamtra2xsbG1tbW
5ubm9vb3BwcHFxcXJycnNzc3R0dHV1dXZ2dnd3d3h4eHl5eXp6ent7e3x8fH19fX5+fn9/f4CAgI
GBgYKCgoODg4SEhIWFhYaGhoeHh4iIiImJiYqKiouLi4yMjI2NjY6Ojo+Pj5CQkJGRkZKSkpOTk5
SUlJWVlZaWlpeXl5iYmJmZmZqampubm5ycnJ2dnZ6enp+fn6CgoKGhoaKioqOjo6SkpKWlpaampq
enp6ioqKmpqaqqqqurq6ysrK2tra6urq+vr7CwsLGxsbKysrOzs7S0tLW1tba2tre3t7i4uLm5ub
q6uru7u7y8vL29vb6+vr+/v8DAwMHBwcLCwsPDw8TExMXFxcbGxsfHx8jIyMnJycrKysvLy8zMzM
3Nzc7Ozs/Pz9DQ0NHR0dLS0tPT09TU1NXV1dbW1tfX19jY2NnZ2dra2tvb29zc3N3d3d7e3t/f3+
Dg4OHh4eLi4uPj4+Tk5OXl5ebm5ufn5+jo6Onp6erq6uvr6+zs7O3t7e7u7u/v7/Dw8PHx8fLy8v
Pz8/T09PX19fb29vf39/j4+Pn5+fr6+vv7+/z8/P39/f7+/v///yH5BAEKAP8ALAAAAAAOAA4AAA
h1AHcJHEiw4L+DCBOW8SEwocMyOwSg2uUQoRkEIUo1rHjigYBb/3aJCRBgFcIaAgScOrirTAAHcQ
6WEHDAE8JdWgYIiPEvyIABhxLuyhNAAAIBAw7gcSiwjYAEDgrMqSjQWoAFAlRVDElRmQ9SW7mGFV
qw7MCAADs="
# unchecked icon for selection
image create photo icon_unchecked -data "R0lGODlhDgAOAOcAAAAAAAEBAQICAgMDAwQEBAUFBQYGBgcHBwgICAkJCQoKCgsLCwwMDA0NDQ4O
Dg8PDxAQEBERERISEhMTExQUFBUVFRYWFhcXFxgYGBkZGRoaGhsbGxwcHB0dHR4eHh8fHyAgICEh
ISIiIiMjIyQkJCUlJSYmJicnJygoKCkpKSoqKisrKywsLC0tLS4uLi8vLzAwMDExMTIyMjMzMzQ0
NDU1NTY2Njc3Nzg4ODk5OTo6Ojs7Ozw8PD09PT4+Pj8/P0BAQEFBQUJCQkNDQ0REREVFRUZGRkdH
R0hISElJSUpKSktLS0xMTE1NTU5OTk9PT1BQUFFRUVJSUlNTU1RUVFVVVVZWVldXV1hYWFlZWVpa
WltbW1xcXF1dXV5eXl9fX2BgYGFhYWJiYmNjY2RkZGVlZWZmZmdnZ2hoaGlpaWpqamtra2xsbG1t
bW5ubm9vb3BwcHFxcXJycnNzc3R0dHV1dXZ2dnd3d3h4eHl5eXp6ent7e3x8fH19fX5+fn9/f4CA
gIGBgYKCgoODg4SEhIWFhYaGhoeHh4iIiImJiYqKiouLi4yMjI2NjY6Ojo+Pj5CQkJGRkZKSkpOT
k5SUlJWVlZaWlpeXl5iYmJmZmZqampubm5ycnJ2dnZ6enp+fn6CgoKGhoaKioqOjo6SkpKWlpaam
pqenp6ioqKmpqaqqqqurq6ysrK2tra6urq+vr7CwsLGxsbKysrOzs7S0tLW1tba2tre3t7i4uLm5
ubq6uru7u7y8vL29vb6+vr+/v8DAwMHBwcLCwsPDw8TExMXFxcbGxsfHx8jIyMnJycrKysvLy8zM
zM3Nzc7Ozs/Pz9DQ0NHR0dLS0tPT09TU1NXV1dbW1tfX19jY2NnZ2dra2tvb29zc3N3d3d7e3t/f
3+Dg4OHh4eLi4uPj4+Tk5OXl5ebm5ufn5+jo6Onp6erq6uvr6+zs7O3t7e7u7u/v7/Dw8PHx8fLy
8vPz8/T09PX19fb29vf39/j4+Pn5+fr6+vv7+/z8/P39/f7+/v///yH+GkNyZWF0ZWQgd2l0aCBH
SU1QIG9uIGEgTWFjACH5BAEKAP8ALAAAAAAOAA4AAAgxAHcJHEiw4L+DCBMeFKiw4T+GDhNCjLhw
F0WEEylmjLjRYceGHxWGlGjx4sOCKAcGBAA7"

image create photo icon_checkedunchecked -data "R0lGODlhHgAOAOe8AAAAAAEBAQICAgMDAwQEBAUFBQYGBgcHBwgICAkJCQoKCgsLCwwMDA0NDQ4O
Dg8PDxAQEBERERISEhMTExQUFBUVFRYWFhcXFxgYGBkZGRoaGhsbGxwcHB0dHR4eHh8fHyAgICEh
ISIiIiMjIyQkJCUlJSYmJicnJygoKCkpKSoqKisrKywsLC0tLS4uLi8vLzAwMDExMTIyMjMzMzQ0
NDU1NTY2Njc3Nzg4ODk5OTo6Ojs7Ozw8PD09PT4+Pj8/P0BAQEFBQUJCQkNDQ0REREVFRUZGRkdH
R0hISElJSUpKSktLS0xMTE1NTU5OTk9PT1BQUFFRUVJSUlNTU1RUVFVVVVZWVldXV1hYWFlZWVpa
WltbW1xcXF1dXV5eXl9fX2BgYGFhYWJiYmNjY2RkZGVlZWZmZmdnZ2hoaGlpaWpqamtra2xsbG1t
bW5ubm9vb3BwcHFxcXJycnNzc3R0dHV1dXZ2dnd3d3h4eHl5eXp6ent7e3x8fH19fX5+fn9/f4CA
gIGBgYKCgoODg4SEhIWFhYaGhoeHh4iIiImJiYqKiouLi4yMjI2NjY6Ojo+Pj5CQkJGRkZKSkpOT
k5SUlJWVlZaWlpeXl5iYmJmZmZqampubm5ycnJ2dnZ6enp+fn6CgoKGhoaKioqOjo6SkpKWlpaam
pqenp6ioqKmpqaqqqqurq6ysrK2tra6urq+vr7CwsLGxsbKysrOzs7S0tLW1tba2tre3t7i4uLm5
ubq6uru7u///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////yH+GkNyZWF0ZWQgd2l0aCBH
SU1QIG9uIGEgTWFjACH5BAEKAP8ALAAAAAAeAA4AAAicAHcJHEiQ4L9/BRMaPMiw4cFdDx06LOND
oESJEBFePFhmhwBUGTdG1HjRDIIQpSyKHBmy4YkHAm4hbLkxo5gAAVYxrCFAwKmHNC9mLBPAQZyD
JQQc8MRQ5cqMWgYIiPEvyIABhxo6FZkxTwABCAQMOIDH4daaD9sISOCgwByMQTE+tBZggQBVQuOa
jajMB6maerWOfKpQIdDCBAMCADs="

# clipboard icon
image create photo icon_clipboard -data "R0lGODlhEgAVAOYAAOvr6peXimxEAm1EA1xcXMaHJ15eXu/v7unp59/Xz+jo5ubm42xFA+vr6cSH
Jurq6OLb1KampcvLyezt7Ozs62loZ6l+Oufn5ebm5KWlpOXl4qampuTk4W1GA+Li4W1FA6Sko4+P
g5CQhGtraufn5Lq6sO3t7H58e+7u7XRwZGhqZLGAMWlqZLuoj62uqnROEWJiYWNVO7a2sri4td3d
3bl+IriliXhQEre3tHBuYndSFXNvYru7snRNDXJvY9bX05CQiry8sqyagOzs6nVxZNvb2MW/uLR7
IqOjoaysqHNMDdvb2+rr6s3Nye7u7Lu7sWtFA8WHJ+Xl4+3t6+rq6bekiZBtNXp6c83OyndPEcHB
vo2NiLh+I+Tk4sO9trqDLMCDJHdQEXt7dcvLx6SkonZQErq6sejo5a+vrOLi37W1sdbW1MWIJmNj
YmNWPMWHJsWGJmZoZKenpm5GA+Pc1mxDAWhpZMSGJvDw7////wAAAAAAAAAAAAAAAAAAAAAAACH5
BAAAAAAALAAAAAASABUAAAf/gHSCgycEhgQng4oQCQlGFQQhIiIhBBVejRB0Nj0MDDFXAaKjYm4M
UEpVYWAWKTBbJSVmTzxBQG0pFlxZc2xEEzQjBsPEI0sTPg51cwUseXgHANLTB3h5dm/Lzc943d7d
B9dw2irPcufochsm4uTc3wcoUwDtzHbm6BEZIGQI9QX3vB1wQqHBAwRF6kUJuEEfCCQIFJDAcu3O
sjf3UJgYYlDBhQUaZlS8eK/gwTMYmmjBIWPkHIx5qEQksaALmjw48dix+PKeR5Ac0rh45mHNTpJ5
MEjh8GOMmiR5PEiIs8Jihzc7mODcmnNqVatvrOSIQ7Zs2S930ta5UcMBnLdwF9LKlevgSJkWOj58
GMBXgN+/fAe8EBIIADs="

image create photo icon_palm -data "R0lGODlhFAAUAOeLACBYATZTCi1nAD1jBjFtA3phDkF2C0V4Dkl/BUx+CU2CBVWEAF2CBFWFBId1
G1eHAFaHBF2KEoJ9KlqLFlyOCmKQCGGPIF6RFmSPGWWSCo6ASWWTCnOPGGeUE2eUImqXBGuUIG2Z
CW6XK4SRQ3CbJ3GbLHOfInahDHefMHWhK3mjGXikFHuiH3mlDXqhNXqjInqnDX6lJn6kMr+SPsuR
PoSrKIKpRIesI4awCoWuNYisRn+yMomvOIuxIoWxNYyyKtKaS4ywQo2zMJC2Lb2mTpG4HpK3LZS4
LpS1TZK5MZS5NJa4PZe6J5e1YJe5S5u9Md6qY5nBT+KqXKLCOaHENaHCUqTFMaTAaqHFSabLH6TH
TanJQeSzcNy4banOLqzId67OOa7MY7HLbrDOcK7Qf7bWPLjWR7rUYLfcL/HFkb/XgMDcUsDaZsPc
XvjJisTYkfPLi8LgTMjgYMrhYfjPlMrggN/fldDobtHllv/XntPjrM3uUtzkmdPpjNXpjNXuZNDq
pdrvc9nqr97xfub1j+v4mer8pOr9xvT/svv/w/r/4P//////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////yH+GkNyZWF0ZWQgd2l0aCBH
SU1QIG9uIGEgTWFjACH5BAEKAP8ALAAAAAAUABQAAAjSAP8JHEiwoEGCheRMOchwICIqNwQ2WdKw
oKEfKv6JUFOxzZMTHXRoGeICxRsbLLAYJDTnTJggFiasgOAhAoYXRsz8YdjHSQIBBBZkqOEn0b9B
B5U8OGAAwIANJhqAaMjkAwkZCAI4YODjSokKB8HgqCIoiQIJMzhcwKMHSQyDWdgo+gcjBRE4Xyi0
ECjG4B2BZYqM6UIHUAghFQXG8VJHiptDPY4kFoiGDxeBW6xM/rfHDhSBazYPBJLnXyDRAmmkQS1w
RAENrP/tyMEjCpnYFQMCADs="

# exit icon for close
image create photo icon_exit -data "
R0lGODlhMAAwAOcAAAAAAAEBAQICAgMDAwQEBAUFBQYGBgcHBwgICAkJCQoKCgsLCwwMDA0NDQ4O
Dg8PDxAQEBERERISEhMTExQUFBUVFRYWFhcXFxgYGBkZGRoaGhsbGxwcHB0dHR4eHh8fHyAgICEh
ISIiIiMjIyQkJCUlJSYmJicnJygoKCkpKSoqKisrKywsLC0tLS4uLi8vLzAwMDExMTIyMjMzMzQ0
NDU1NTY2Njc3Nzg4ODk5OTo6Ojs7Ozw8PD09PT4+Pj8/P0BAQEFBQUJCQkNDQ0REREVFRUZGRkdH
R0hISElJSUpKSktLS0xMTE1NTU5OTk9PT1BQUFFRUVJSUlNTU1RUVFVVVVZWVldXV1hYWFlZWVpa
WltbW1xcXF1dXV5eXl9fX2BgYGFhYWJiYmNjY2RkZGVlZWZmZmdnZ2hoaGlpaWpqamtra2xsbG1t
bW5ubm9vb3BwcHFxcXJycnNzc3R0dHV1dXZ2dnd3d3h4eHl5eXp6ent7e3x8fH19fX5+fn9/f4CA
gIGBgYKCgoODg4SEhIWFhYaGhoeHh4iIiImJiYqKiouLi4yMjI2NjY6Ojo+Pj5CQkJGRkZKSkpOT
k5SUlJWVlZaWlpeXl5iYmJmZmZqampubm5ycnJ2dnZ6enp+fn6CgoKGhoaKioqOjo6SkpKWlpaam
pqenp6ioqKmpqaqqqqurq6ysrK2tra6urq+vr7CwsLGxsbKysrOzs7S0tLW1tba2tre3t7i4uLm5
ubq6uru7u7y8vL29vb6+vr+/v8DAwMHBwcLCwsPDw8TExMXFxcbGxsfHx8jIyMnJycrKysvLy8zM
zM3Nzc7Ozs/Pz9DQ0NHR0dLS0tPT09TU1NXV1dbW1tfX19jY2NnZ2dra2tvb29zc3N3d3d7e3t/f
3+Dg4OHh4eLi4uPj4+Tk5OXl5ebm5ufn5+jo6Onp6erq6uvr6+zs7O3t7e7u7u/v7/Dw8PHx8fLy
8vPz8/T09PX19fb29vf39/j4+Pn5+fr6+vv7+/z8/P39/f7+/v///yH+GkNyZWF0ZWQgd2l0aCBH
SU1QIG9uIGEgTWFjACH5BAEKAP8ALAAAAAAwADAAAAj+AP8JHEiwoMGDCBMqLMjHkENCECNKnDho
kCBAcxYqLLQo0aFCFgUJCkSypEmSgFL+6cMHDx0zGgkmcrQIkaNPpVCxUpXqVClSokJ98sRpUyZM
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
Ow=="


# plus icon
image create photo icon_plus -data "
R0lGODlhEwATAJEAAJmZmczMzGZmZv///yH5BAAAAAAALAAAAAATABMAAAJClI+Zwe3PDASUQvnA
2OCKuQ3dgzka54Eo+VVV6FrSGdZ1N9s6mu823orBYoFSgzZyGBnIVAbmNEFZE1cUgi0qtooCADs="

# minus icon
image create photo icon_minus -data "
R0lGODlhEwATAJEAAJmZmczMzGZmZv///yH5BAAAAAAALAAAAAATABMAAAI1lI+Zwe3PDJxOUmov
zLqKvn3ASJZmIAHDyrYsgIruvMIpPduiyZMx+OAAhSBix6hBXhRMZgEAOw=="

# folder icon
image create photo icon_folder -data "
R0lGODlhEAAQAOZ+AGFhYfDw8HFxceDg4LCwsICAgKCgoJCQkPzsXfjbRv73cfztX//80P/5nv/7
xP/5mv/7wP/4hO/v7/70aVFRUfriT//4fP71a/72bv//+vjaRP/5p/rkU7+/v9/f3//4iP3zZ46I
Of/6vP/++P/5kf/94f/6s5+fn+/qpHBtNXFwZf/3dO/obb2zSOnRR/ngTf/+8evaVP/5jXBwXvvo
WP/83J+bWPrjUfvnVurTSf///r+7if/+9f/83f/4gf/5o//6r3FvZWBfTv/95a+vr//96d/e1f3x
ZP/5lX5xJ4t4Iv///GBeVP/4gG1gJ/3yZnBwcPzrW//7yHFuQvvpWb++sNnBP8mxN5+ccYCActDQ
0P/6uF5TGmFfUU9GILmfLIGBenBsWXFvWYGBgf3vYmBdSm9mJlBMOfvmVPzuYP/82P/7zP/5oo+P
j761TP/82VFRS7qjM//5q//3eIGAf//+7frjUFBQUIGAevzqW4B/bP/81cDAwP///wAAAAAAACH5
BAEAAH4ALAAAAAAQABAAAAfKgH6CggEHTIQGYU4Eg4IHXl9KBEFcVxoJZYMBZ3GXVkkuLxU3HGYB
ghQ5FXYcaDg0VFEICy2MAwAxebNpZEdPIBMXGAVCISFuwMIKK3MWPhEfYDMKLCkWTdAyJEgPDT8b
enw2JFPdbBtyQCZbIhAOWX5wKFg77Q5Sawx7bz0lKn4DxDDooqZGiSFF6sDgMSIDHUEFquAx0nCJ
jj4Y+3QAMIiCBwEZM3a4U6CRFgFjPGQkAsBAo0EHTkiQ0Kbly5cCoAgYcLOnT0GBAAA7"




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