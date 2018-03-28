#!/usr/bin/env python
import os

class EnsPythonCut(object):
    def __init__(self, filename):
        self.filename = filename
        
        self.fileid = open(filename,"w")
        
        self.ppdir = os.path.dirname(filename)

    def writeHeader(self):
        """Header Of all ensight python scripts"""
        self.fileid.write("""

#image resolution and filename
XRESOLUTION=800
YRESOLUTION=600
COLORMIN=0
COLORMAX=1
OUTPATHFILE='./'  # trailing / needed ! 
CASEFILE='{0}'

ensight.part.select_default()
ensight.part.modify_begin()
ensight.part.elt_representation("3D_feature_2D_full")
ensight.part.modify_end()
ensight.data.binary_files_are("native")
ensight.data.format("Xdmf2")
ensight.data.shift_time(1.000000,0.000000,0.000000)
ensight.data.replace(CASEFILE)
""".format(os.path.join(self.ppdir,'c3sm_xdmfgroup.xmf')))
        self.fileid.flush()

    def writeBody(self,cuttype,direction,pos,var,list_ids): 
        """Make cuts and save images"""
    
        self.fileid.write("""
print "Defining functions ...",

def print_image(file):
  
  ensight.file.image_format("gif")
  ensight.file.image_format_options("Compression Default")
  ensight.anim_recorders.render_offscreen("ON")
  ensight.file.image_numpasses(4)
  ensight.file.image_stereo("current")
  ensight.file.image_screen_tiling(1,1)
  ensight.file.image_file(OUTPATHFILE+file)
  ensight.file.image_window_size("user_defined")
  ensight.file.image_window_xy(XRESOLUTION,YRESOLUTION)
  ensight.file.save_image()

  return

def cut(axis,value,part):
  ensight.part.select_begin(part)
  ensight.clip.begin()
  ensight.clip.mesh_plane(axis)
  ensight.clip.value(value)
  ensight.clip.tool("xyz")
  ensight.clip.end()
  ensight.clip.create()
  ensight.clip.domain("crinkly")
  ensight.part.select_end(part)

  return

def cylinder(Origin,axis,radius,part):
  ensight.part.select_begin(part)
  ensight.clip.begin()
  ensight.clip.extents("infinite")
  ensight.clip.tool("cylinder")
  ensight.clip.origin(Origin[0],Origin[1],Origin[2])
  ensight.clip.axis(axis[0],axis[1],axis[2])
  ensight.clip.radius(radius)
  ensight.clip.end()
  ensight.clip.create()
  ensight.part.select_end(part)

def color(variable,part):
  ensight.part.select_begin(part)
  ensight.part.modify_begin()
  ensight.part.colorby_palette(variable)
  ensight.function.palette("abspress")
  ensight.function.range(COLORMIN,COLORMAX)
  ensight.part.modify_end()
  ensight.part.select_end(part)

  return

def hide(part):
  ensight.part.select_begin(part)
  ensight.part.modify_begin()
  ensight.part.visible("OFF")
  ensight.part.modify_end()
  ensight.part.select_end(part)

def hide_legend(variable):
  ensight.legend.select_palette_begin(variable)
  ensight.legend.visible("OFF")


def legend(variable,VARTITLE,var_range_min,var_range_max):
  ensight.function.palette(variable)
  ensight.function.modify_begin()
  ensight.function.range(float(var_range_min),float(var_range_max))
  ensight.legend.select_palette_begin(variable)
  ensight.legend.title_name(VARTITLE)
  ensight.legend.format("%.2f")

print "Done"

#ensight.sendmesgoptions(exception=1,display=1)
ensight.viewport.select_begin(0)
ensight.viewport.background_type("constant")
ensight.viewport.select_end(0)


""")

        self.fileid.write(cuttype+"('"+direction+"','"+pos+"',1)\n")
        self.fileid.write("hide(1)\n")
        self.fileid.write("color('"+var+"',2)\n")
        self.fileid.write("legend('"+var+"','"+var+"',0,1)\n")
        
        self.fileid.write("ensight.part.select_begin(2)\n")
        self.fileid.write("ensight.view_transf.view_recall('+"+direction+"')\n")
        self.fileid.write("ensight.view_transf.fit(2)\n")
        self.fileid.write("ensight.part.select_end(2)\n")
 

        self.fileid.write("""
ensight.text.new_text('Simulation')
ensight.text.location_x(5.000000e-01)
ensight.text.location_y(1.50000e-01)
ensight.text.size(40)
""")
    
    
        for current_step in range(0,len(list_ids)):
            
            self.fileid.write("ensight.solution_time.current_step("+str(current_step)+")\n")
            self.fileid.write("ensight.solution_time.update_to_current()\n")
            self.fileid.write("ensight.text.select_begin(0)\n")
            self.fileid.write("ensight.text.change_text('"+list_ids[current_step].replace("#","<cr>")+"')\n")
            self.fileid.write("ensight.text.select_end(0)\n")
            #fileid.write('ensight.file.image_convert("ON")\n')
            self.fileid.write("print_image('./"+var+"_"+str(current_step)+"_"+list_ids[current_step].replace("#","_")+"')\n")
            #fileid.write('ensight.file.image_convert("OFF")\n')

        self.fileid.flush()

    def close(self):
        self.fileid.close()


if __name__ == '__main__':
    pass # Do tests
