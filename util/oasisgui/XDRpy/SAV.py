import os, stat, sys
import hashlib



class FileDB:

    fields = ["name","path","signature","md5"]
    md5 = True



    def __init__(self,obj):
        """
        Instanciation de la base de donnees a partir d'un repertoire ou d'un fichier de sauvegarde
        """
        if os.path.isdir(obj):
            self._mapDirectory(obj)
        elif os.path.isfile(obj):
            self._loadFromFile(obj)
        else :
            raise Exception(str(obj)+" is not a valid address")

    def __repr__(self):
        return "A file database instance"

    def __str__(self):
        """
        Affichage du contenu de l'instance
        """
        res = []
        for l in self._db:
            res.append(os.path.join(l["path"],l["name"])+" ==> "+l["signature"])

        return "\n".join(res)


    def _loadFromString(self,str):
        """
        Chargement ou rechargement du contenu de la base de donnee a partir d'une chaine de caractere au format CSV
        """
        self._db = [dict(zip(FileDB.fields,f.split(";"))) for f in str.split("\n")]        

    def _loadFromFile(self,file):
        """
        Chargement ou rechargement du contenu de la base de donnee a partir d'un fichier CSV
        """
        content = open(file,"r").read()
        self._loadFromString(content)

    def _mapDirectory(self,dir):
        """
        Parcours le repertoire dir et stocke son arborescence dans la memoire interne de l'instance
        La recusion est faite dans le os.walk
        """
        self._db = []
        for root, dirs, files in os.walk(dir):
            for name in files:
                fileNode = {}
                fileNode["name"] = name
                fileNode["path"] = os.path.abspath(root)
                fileNode["signature"] = str(FileDB._signature(os.path.join(root,name)))
                if FileDB.md5:
                    fileNode["md5"] = FileDB.md5file(os.path.join(os.path.abspath(root),name))
                else:
                    fileNode["md5"] = ""
                self._db.append(fileNode)

    def save(self,file):
        """
        Ecrit le contenu de la BDD au format CSV
        """
        content=[]
        for line in self._db:
            tempLine=[]
            for field in FileDB.fields:
                tempLine.append(line[field])
            content.append(";".join(tempLine))

        out = open(file,"w")
        out.write("\n".join(content))

    def isInDb(self,file):
        """
        Recherche si un fichier est present dans la base de donnees (nom et signature sont compares)
        Retourne la liste des fichiers correspondant, liste vide si aucun element
        """
        if not(os.path.isfile(file)):
            raise Exception(file + " not found")

        dirName, fileName = os.path.split(file)
        sig = FileDB._signature(file)
        results = [elm for elm in self._db if elm["name"] == fileName and elm["signature"]==str(sig)]
        
        if FileDB.md5:
            md5 = FileDB.md5file(file)
            results = [elm for elm in self._db if elm["name"] == fileName and elm["md5"]==str(md5)]
            
        return [os.path.join(elm["path"],elm["name"]) for elm in results]
        

    @staticmethod
    def _signature(file):
        """
        Cree une signature du fichier (mode, taille en octet, date de modification)
        """
        st = os.stat(file)
        return (stat.S_IFMT(st.st_mode),
                st.st_size,
                st.st_mtime)
    
    # commente car pb avec python 2.4 et 2.5
    # chunksize depend de la machine il faudra optimiser
    
    @staticmethod
    def md5file(fileName,chunksize=13):
        md5 = hashlib.md5()
        f = open(fileName,'rb') 
        for chunk in iter(lambda: f.read(2**chunksize), b''): 
            md5.update(chunk)
        f.close()
        return md5.hexdigest()


if __name__ == "__main__":
    db = FileDB('.')
    db.save('XDR-FileDB.csv')
