#!/usr/bin/env python

from itertools import izip
from math import sqrt,atan2,pi,hypot,cos,sin

def grouped(iterable, n):
    "s -> (s0,s1,s2,...sn-1), (sn,sn+1,sn+2,...s2n-1), (s2n,s2n+1,s2n+2,...s3n-1), ..."
    return izip(*[iter(iterable)]*n)

def norm(u):
    return sqrt(u["x"]**2+u["y"]**2+u["z"]**2)


def rotate(y,z,rotate_deg):
    r = hypot(z,y)
    theta = atan2(z,y) + rotate_deg*pi/180
    y = r*cos(theta)
    z = r*sin(theta)
    return [y,z] 
    

def makevect(points,a,b):
    res = {}
    res["x"] = points[b,"x"] - points[a,"x"]
    res["y"] = points[b,"y"] - points[a,"y"]
    res["z"] = points[b,"z"] - points[a,"z"]
    return res

   
def pvect(u,v):
    res = {}
    res["x"] = u["y"]*v["z"]- u["z"]*v["y"]
    res["y"] = u["z"]*v["x"]- u["x"]*v["z"]
    res["z"] = u["x"]*v["y"]- u["y"]*v["x"]
    return res


def getparts(data,subpart=""):
    if subpart != "" :
        subpart = "."+subpart
    res = None
    i = data.index("root"+subpart+".children :")
    i+=1
    res = data[i].strip().split(",")    
    return res
    
def dumpparts(list_parts,subpart=""):
    if subpart != "" :
        subpart = "."+subpart
    res = "root"+subpart+".children :\n"+ ",".join(list_parts)+"\n\n"
    return res

def getfield(part,field,data,datatype="integer",subpart=""):
    if subpart != "" :
        subpart = "."+subpart
    
    i = data.index("root."+part+subpart+"."+field+" :")
    i +=1
    j = i
    while data[j] != "" :
        j += 1
    out = filter(None,",".join(data[i:j]).split(","))
    
    
    if field == "children" :
        datatype = "spacedstring"
    
    
    if datatype == "spacedstring":
        pass
    if datatype == "float":
        out  =[float(x) for x in out]
    if datatype == "integer":
        out  =[int(x) for x in out]        
    return out
    
def dumpfield(part,field,listdata,subpart="") :
    if subpart != "" :
        subpart = "."+subpart
        
    dump = "root."+part+subpart+"."+field+" :\n"
    maxcol = 10
    col = 1
    for item in listdata:
        dump += str(item) +","
        if col == maxcol :
            dump += "\n"
            col = 1
        col += 1
    
    dump += "\n\n"
    return dump