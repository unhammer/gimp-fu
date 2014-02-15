 #!/usr/bin/env python

from gimpfu import *
import gtk
import os
import sys

export_width = 1600

subdirs = {
    "cbig": "farge",
    "cnet": "farge_til_nettbruk",
    "bwbig": "svarthvitt",
    "bwnet": "svarthvitt_til_nettbruk",
}

def python_export_bw_scaled_jpeg(img, base, origdir, subdir):
    subdir_full = os.path.join(origdir,subdir)
    if not os.path.isdir(subdir_full):
        os.mkdir(subdir_full)
    jpgname = os.path.join(subdir_full, base+".jpg")
    print >>sys.stderr, jpgname
    img.flatten()
    pdb.file_jpeg_save(img,
                       pdb.gimp_image_get_active_layer(img),
                       jpgname,
                       jpgname,
                       1,		# kvalitet
                       0,		# utjamning
                       1,		# optimaliser
                       0,		# progressiv
                       "Created with GIMP by ~T~",
                       3,		# subsmp, 3 is best quality (?)
                       1,		# force baseline
                       0,		# restart markers
                       0,		# dct slow
    )

def python_export_bw_scaled_overwrite_question(filename):
    global subdirs
    md = gtk.MessageDialog(flags = gtk.DIALOG_DESTROY_WITH_PARENT,
                           type = gtk.MESSAGE_WARNING,
                           buttons = gtk.BUTTONS_YES_NO,
                           message_format = "File %s already exists!\nOverwrite all with this file name in %s?" %(filename, ", ".join(subdirs.values())))
    answer = md.run()
    md.destroy()
    return answer == gtk.RESPONSE_YES

def python_export_bw_scaled_make_bw(img):
    img.flatten()
    drawable = pdb.gimp_image_get_active_layer(img)
    pdb.plug_in_colors_channel_mixer(
        img,
        drawable,
        True, # Monochrome
        # /usr/share/gimp/2.0/scripts/BW-Film-Simulation-1.1.scm 
        # Kodak Tri-X presets:
        0.25, 0.35, 0.40, 0.25, 0.35, 0.40, 0.25, 0.35, 0.40)

def python_export_bw_scaled(oimg, drawable) :
    global export_width, subdirs
    new_width = min(oimg.width, export_width)
    new_height = oimg.height * new_width / oimg.width

    origdir = os.path.dirname(oimg.filename)
    base, ext = os.path.splitext(os.path.basename(oimg.filename))

    if not any([l.name == 'vm' for l in oimg.layers]):
        md = gtk.MessageDialog(flags = gtk.DIALOG_DESTROY_WITH_PARENT,
                               type = gtk.MESSAGE_WARNING,
                               buttons = gtk.BUTTONS_CLOSE,
                               message_format = "No layer named 'vm'! Make one and try again :-)")
        md.run()
        md.destroy()
        raise gimp.error("No layer named 'vm'!")

    for jpgname in [os.path.join(origdir,subdir,base+".jpg")
                    for subdir in subdirs.values()]:
        if os.path.exists(jpgname):
            if python_export_bw_scaled_overwrite_question(jpgname):
                break
            else:
                return

    # farge, full storleik, utan vassmerke
    bigimg = oimg.duplicate()
    for l in bigimg.layers:
        if l.name == 'vm':
            l.visible = False
    python_export_bw_scaled_jpeg(bigimg, base, origdir, subdirs["cbig"])

    # svartkvitt, full storleik, utan vassmerke
    python_export_bw_scaled_make_bw(bigimg) # flattens!
    python_export_bw_scaled_jpeg(bigimg, base, origdir, subdirs["bwbig"])

    pdb.gimp_image_delete(bigimg)


    # farge, skalert ned, med vassmerke
    netimg = oimg.duplicate()
    pdb.gimp_image_scale(netimg, new_width, new_height)
    for l in netimg.layers:
        if l.name == 'vm':
            l.visible = True
    python_export_bw_scaled_jpeg(netimg, base, origdir, subdirs["cnet"])

    # svartkvitt, skalert ned, med vassmerke
    python_export_bw_scaled_make_bw(netimg) # flattens!
    python_export_bw_scaled_jpeg(netimg, base, origdir, subdirs["bwnet"])

    pdb.gimp_image_delete(netimg)





register(
    "python_fu_export_bw_scaled",
    "Export bw, scaled, with and without watermark",
    "Export bw, scaled, with and without watermark",
    "Kevin Brubeck Unhammer",
    "Kevin Brubeck Unhammer",
    "2014",
    "Export bw/scaled/watermarked",
    "*",
    [
        (PF_IMAGE, "image", "Input image", None),
        (PF_DRAWABLE, "drawable", "Input drawable", None),
    ],
    [],
    python_export_bw_scaled,
    menu = "<Image>/File/Save/"
)

main()
