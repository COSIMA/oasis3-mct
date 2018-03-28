import os

#### FOR HTML OUTPUTS #####

class HtmlWriters(object):
    def __init__(self):
        pass


    def html_header(self,title):
        """ create the header for c3s HTML docs """
        header = """
<html>
<body>
<head>
<title> """+ title + """
</title>
<style>
html { min-height:100%; } /* to get the gradient to stetch to the bottom of the view port */
body {
        background: #c1c1c1;
        background: -moz-linear-gradient(left, #ffffff 0%, #c1c1c1 100%), url(img.png) no-repeat 50% 300px;
        background: -webkit-gradient(linear, 100% 0%,0% 0%, from(#ffffff), to(#c1c1c1)), url(img.png) no-repeat 50% 300px;
        background: -webkit-linear-gradient(left, #ffffff 0%,#c1c1c1 100%), url(img.png) no-repeat 50% 300px;
        background: -o-linear-gradient(left, #ffffff 0%,#c1c1c1 100%), url(img.png) no-repeat 50% 300px;
        background: -ms-linear-gradient(left, #ffffff 0%,#c1c1c1 100%), url(img.png) no-repeat 50% 300px;
        background: linear-gradient(left, #ffffff 0%,#c1c1c1 100%), url(img.png) no-repeat 50% 300px;
        margin-top:100px;
        margin-bottom:100px;
        margin-right:50px;
        margin-left:50px;
}
h1 {

}
h2 {
padding-left:10px;
}
h3 {
padding-left:20px;
}
h4 {
padding-left:40px;
}
h5 {
padding-left:50px;
}
p {
padding-left:50px;
}
</style>
</head>
"""
        return header
        
    def html_closing(self):
        """ append to and html file before closure"""
        closing = "</html></body>"    
        
        return closing
        
        
    def html_create_image(self,filename):
        img_declare = []
        img_declare.append('<div id="wrapper" style="width:100%; text-align:center">\n')
        img_declare.append('<img src="'+filename+'"; style="margin:auto;">\n')
        
        
        captionfile = filename.replace(".gif",".txt")
        if os.path.exists(captionfile):
            img_declare.append("<br>")
            print "Debug : a txt exists"
            tmpfile= open(captionfile, 'r')
            caption_text= tmpfile.readlines()
            tmpfile.close()
            for line in caption_text :
                 img_declare.append(line.replace("\n","<br>"))
            
        img_declare.append("</div>\n")
        print img_declare
    
        return img_declare
    
    def html_create_table(self,caption,list_header,content):
        """ create an html table, output is a list to write in a file"""
        
        linelist = []
        linelist.append("<table border='1'>\n")
        linelist.append("<caption>"+caption+"</caption>\n")
        
        linelist.append("    <tr>\n")
        for header in list_header:
            linelist.append("         <th>"+header+"</th>\n")
        linelist.append("    </tr>\n")
        
        for lines in content :
            
            linelist.append("    <tr>\n")
            for cell in lines :
                # intelligent line wrapper, for pathes and spaces.
                final_cell= ""
                maxlenght=35
                
                # the '#' symbol is used in comments to skip a line
                for sentence in cell.split("#"):
                    if len(sentence) < maxlenght :
                        final_cell+=sentence+"<br>"
                    else :
                        # cut sentences btw words
                        bit = ""
                        stage1 = ""
                        for word in sentence.split(" "):
                            bit+=" "+word
                            if len(bit) > maxlenght:
                                stage1+=bit+"<br> "
                                bit=""
                        
                        stage1+=bit+"<br> "
                        
                        final_cell = stage1
                        
                        
                        if len(final_cell.split("/")) > 1:
                            bit = ""
                            stage2 = ""
                            for word in final_cell.split("/"):
                                bit+="/"+word
                                if len(bit) > maxlenght:
                                    stage2+=bit+"<br> "
                                    bit=""
                                
                            stage2+=bit+"<br> "
                            final_cell=stage2
                    
                linelist.append("      <td>"+final_cell+"</td>\n")
            linelist.append("    </tr>\n")
        
        linelist.append("</table>\n")
    
        
        return linelist
        
        
    def html_add_toc (self,filename):
        """ add a table of content based on headers to an html file"""
        initfile =  open(filename,"r")
        lines = initfile.readlines()
        initfile.close()
        
        lines_changed=[]
        menu=[]
        
        ref_h1 = 0
        ref_h2 = 0
        ref_h3 = 0
        ref_h4 = 0
        
        h1tags=['','A.','B.','C.','D.','E.','F.','G.','H.','I.','J.']
        h2tags=['','I.','II.','III.','IV.','V.','VI.','VII.','VIII.','IX.','X.','XI.','XII.','XIII.','XIV.','XV.','XVI.','XVII.','XVIII.','XIX.','XX.' ]
        h3tags=['','1.','2.','3.','4.','5.','6.','7.','8.','9.','10.','11.','12.','13.','14.','15.','16.','17.','18.','19.','20.' ]
        h4tags=['','1.','2.','3.','4.','5.','6.','7.','8.','9.','10.','11.','12.','13.','14.','15.','16.','17.','18.','19.','20.' ]
        
        # parsing document
        for line in lines :
            newline=line
            
            for balise in ['h1', 'h2', 'h3', 'h4'] :
                if line.startswith('<'+balise+'>'):
                    title=line.split('</')[0]
                    title=title.split('>')[1]
                    
                    if balise == 'h1' :
                        ref_h1 +=1
                        ref_h2 = 0
                        ref_h3 = 0
                        ref_h4 = 0
                        
                        
                        
                    if balise == 'h2' :
                        ref_h2 +=1
                        ref_h3 = 0
                        ref_h4 = 0
                        
                    if balise == 'h3' :
                        ref_h3 +=1
                        ref_h4 = 0
                        
                        
                    if balise == 'h4' :
                        ref_h4 +=1
                        
                    h1tag=h1tags[ref_h1]
                    h2tag=h2tags[ref_h2]
                    h3tag=h3tags[ref_h3]
                    h4tag=h4tags[ref_h4]
                        
                    
                    tag = h1tag+h2tag+h3tag+h4tag
                    
                    
                    newline = '<'+balise+'>'+ tag + ' ' +title +' <a name=title_'+tag+'></a> </'+ balise +'>'
                    newmenu = '<'+balise+'> <a href="#title_'+tag+'">'+ tag +' '+title +'</a></'+ balise +'>'
                    menu.append(newmenu)
                    
            lines_changed.append(newline)
        
        # creating the final document by adding menu
        final_lines = []
        for line in lines_changed :
            final_lines.append(line)
            if line.strip()=="</head>" :
                final_lines.extend(menu)
            
        
        newhtml = open(filename, 'w')
        newhtml.writelines(final_lines)
        newhtml.close()
        
        return
        
    def get_html_content(self,filename):
        html_list=[]
        
        if os.path.exists(filename):
            tmpfile= open(filename, 'r')
            html_list= tmpfile.readlines()
            tmpfile.close()
        else :
            html_list.append("File "+filename+" is not created yet...\n" )
        
        return html_list

if __name__ == '__main__':
    pass # Do tests
