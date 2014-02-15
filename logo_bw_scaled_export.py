 #!/usr/bin/env python

from gimpfu import *
import gtk
import os
import sys

export_width = 1600

subdirs = {
    "c": {
        "big": "farge",
        "net": "farge_til_nettbruk",
    },
    "bw": {
        "big": "svarthvitt",
        "net": "svarthvitt_til_nettbruk",
    },
}

def python_wmscaled_jpeg(img, base, origdir, subdir):
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
    pdb.gimp_image_delete(img)

def python_wmscaled_ask_overwrite(filename, subdirs):
    md = gtk.MessageDialog(flags = gtk.DIALOG_DESTROY_WITH_PARENT,
                           type = gtk.MESSAGE_WARNING,
                           buttons = gtk.BUTTONS_YES_NO,
                           message_format = "File %s already exists!\n\nOverwrite all with this file name in %s?" %(filename, ", ".join(subdirs.values())))
    answer = md.run()
    md.destroy()
    return answer == gtk.RESPONSE_YES

def python_wmscaled_make_bw(img, drawable):
    pdb.plug_in_colors_channel_mixer(
        img,
        drawable,
        True, # Monochrome
        # /usr/share/gimp/2.0/scripts/BW-Film-Simulation-1.1.scm 
        # Kodak Tri-X presets:
        0.25, 0.35, 0.40, 0.25, 0.35, 0.40, 0.25, 0.35, 0.40)

def python_wmscaled_bwdupe(oimg, drawable):
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
                    for subdir in subdirs["c"].values()]:
        if os.path.exists(jpgname):
            if python_wmscaled_ask_overwrite(jpgname, subdirs["c"]):
                break
            else:
                return

    # farge, skalert ned, med vassmerke
    netimg = oimg.duplicate()
    pdb.gimp_image_scale(netimg, new_width, new_height)
    for l in netimg.layers:
        if l.name == 'vm':
            l.visible = True
    python_wmscaled_jpeg(netimg, base, origdir, subdirs["c"]["net"])
    

    # farge, full storleik, utan vassmerke
    bigimg = oimg.duplicate()
    for l in bigimg.layers:
        if l.name == 'vm':
            l.visible = False
    python_wmscaled_jpeg(bigimg, base, origdir, subdirs["c"]["big"])

    # Lag og opna bw-duplikatet:
    bwimg = oimg.duplicate()
    bwimg.filename = oimg.filename
    for l in bwimg.layers:
        python_wmscaled_make_bw(bwimg, l)
    pdb.gimp_display_new(bwimg)
    gimp.displays_flush()
    # Don't delete bwimg, user gets to work see it instead


def python_wmscaled_thebw(oimg, drawable):
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
                    for subdir in subdirs["bw"].values()]:
        if os.path.exists(jpgname):
            if python_wmscaled_ask_overwrite(jpgname, subdirs["bw"]):
                break
            else:
                return

    # svartkvitt, skalert ned, med vassmerke
    netimg = oimg.duplicate()
    for l in netimg.layers:
        if l.name == 'vm':
            l.visible = True
    pdb.gimp_image_scale(netimg, new_width, new_height)
    python_wmscaled_jpeg(netimg, base, origdir, subdirs["bw"]["net"])

    # svartkvitt, full storleik, utan vassmerke
    bigimg = oimg.duplicate()
    for l in bigimg.layers:
        if l.name == 'vm':
            l.visible = False
    python_wmscaled_jpeg(bigimg, base, origdir, subdirs["bw"]["big"])



register(
    "python_fu_wmscaled_thebw",
    "Export the bw scaled, with and without watermark",
    "Export the bw scaled, with and without watermark",
    "Kevin Brubeck Unhammer",
    "Kevin Brubeck Unhammer",
    "2014",
    "Export the bw +scaled/watermarked",
    "*",
    [
        (PF_IMAGE, "image", "Input image", None),
        (PF_DRAWABLE, "drawable", "Input drawable", None),
    ],
    [],
    python_wmscaled_thebw,
    menu = "<Image>/File/Save/"
)

register(
    "python_fu_wmscaled_bwdupe",
    "Export scaled, with and without watermark, then dupe to bw",
    "Export scaled, with and without watermark, then dupe to bw",
    "Kevin Brubeck Unhammer",
    "Kevin Brubeck Unhammer",
    "2014",
    "Export +scaled/watermarked, then dupe to bw",
    "*",
    [
        (PF_IMAGE, "image", "Input image", None),
        (PF_DRAWABLE, "drawable", "Input drawable", None),
    ],
    [],
    python_wmscaled_bwdupe,
    menu = "<Image>/File/Save/"
)

main()
