import os

def link2path(filename):
    return os.path.realpath(filename)
      
class PckgDefaultFile:
    def __init__(self,pckgname,mode,compression=None):
        self.pckgname=pckgname
        self.mode=mode
        self.compression=compression
        self.debug=0

    def close(self):
        try:    return self.pckg.close()
        except: return -1

class PckgZipFile(PckgDefaultFile):
    def __init__(self,pckgname,mode,compression=""):
        PckgDefaultFile.__init__(self,pckgname,mode,compression)
        import zipfile
        self.pckg=zipfile.ZipFile(self.pckgname,self.mode)
    
    def debug(self,state=1):
        self.debug=state
        
    def add(self,filename,arcname=None):
        try:    return self.pckg.write(filename,arcname)
        except: return -1

    def extract(self,filename,path=''):
        outfile = open(os.path.join(path,filename),"wb")
        outfile.write(self.pckg.read(filename))
        outfile.close()
        return 0
        
    def extractall(self,path=''):
        for name in self.pckg.namelist():
            if self.debug: print name
            if not name.endswith('/'):
                directory = os.path.normpath(os.path.join(path,os.path.dirname(name)))
                if not os.path.exists(directory): os.makedirs(directory)
                self.extract(name,path)
            elif not os.path.exists(name): 
                os.makedirs(os.path.join(path,name))
        return 0
  
    def read(self,filename):
        try:    return self.pckg.read(filename)
        except: return -1
    
    def check(self):
        try:    self.pckg.namelist()
        except: return -1            
        return 0
    def list(self):
        return self.pckg.namelist()
        
    def getUncompressedSize(self):
        totalSize=0
        try:
            for zipinfo in self.pckg.infolist():
                totalSize+=zipinfo.file_size
        except:
            return-1
        return totalSize
        
class PckgTarFile(PckgDefaultFile):
    def __init__(self,pckgname,mode,compression=""):
        PckgDefaultFile.__init__(self,pckgname,mode,compression)
        import tarfile
        try:
            self.pckg=tarfile.open(self.pckgname,self.mode+":"+self.compression)
        except Exception,e:
            print (self.pckgname,self.mode+":"+self.compression)
            raise e
        
    
    def debug(self,state=1):
        self.debug=state
        self.pckg.debug=state
           
    def add(self,filename,arcname=None):
        if arcname==None: arcname=filename
        filename=link2path(filename)
        
        try:    self.pckg.add(filename,arcname)
        except: return -1

    def extract(self,filename,path=''):            
        return self.pckg.extract(filename,path)

    def extractall(self,path=None):
        return self.pckg.extractall('')
        
    def read(self,filename):           
        return self.pckg.extractfile(filename).read()

    def check(self):
        try:    self.pckg.getnames()
        except: return -1
        return 0
    def list(self):
        return self.pckg.getnames()   
    def getUncompressedSize(self):
        totalSize=0
        try:
            for tarinfo in self.pckg:
                totalSize+=tarinfo.size
        except:
            return-1
        return totalSize
                
def validExt(filename,extlist):
    for ext in extlist:
        if filename.endswith(ext):
            return True
    return False

class PckgFile:
    def __init__(self,pckgname,mode):
        compression=''
        if validExt(pckgname,['zip']):
            classobj=PckgZipFile
        elif validExt(pckgname,['tar']):
            classobj=PckgTarFile
        elif validExt(pckgname,['tar.gz','tgz']):
            classobj=PckgTarFile
            compression="gz"
        elif validExt(pckgname,['tar.bz2']):
            classobj=PckgTarFile
            compression="bz2"
        
        self.write=self.add
        self.pckg=classobj(pckgname,mode,compression)
    
    def add(self,filename,arcname=None):
        return self.pckg.add(filename,arcname)
    
    def extract(self,filename,path=''):
        return self.pckg.extract(filename,path)

    def extractall(self,path=''):
        return self.pckg.extractall(path)
    
    def read(self,filename):
        return self.pckg.read(filename)
                 
    def close(self):
        return self.pckg.close()

    def check(self):
        return self.pckg.check()
    def list(self):
        return self.pckg.list()
        
    def getUncompressedSize(self):
        return self.pckg.getUncompressedSize()
            
                    
#if __name__=='__main__':
#    pckg=PckgFile("coucou2.tgz","w")
#    for x in ['aa','aa_z.tgz']:
#        pckg.add(x)
#    pckg.close()
#    
#        
#    #pckg=PckgFile("coucou.zip","r")
#    ##pckg.extract(x+".bak")
#    #pckg.extractall("extract")
#    #pckg.close()