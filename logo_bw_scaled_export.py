 #!/usr/bin/env python

from gimpfu import *
import gtk
import os
import sys

export_width = 1600

def python_export_bw_scaled_jpeg(img, base, origdir, subdir):
    subdir_full = os.path.join(origdir,subdir)
    if not os.path.isdir(subdir_full):
        os.mkdir(subdir_full)
    jpgname = os.path.join(subdir_full, base+".jpg")
    print >>sys.stderr, jpgname
    img.flatten()
    # File might exist already:
    dosave = True
    if os.path.exists(jpgname):
        dosave = python_export_bw_scaled_overwrite_question(jpgname)
    if dosave:
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

def python_export_bw_scaled_overwrite_question(filename):
    md = gtk.MessageDialog(flags = gtk.DIALOG_DESTROY_WITH_PARENT,
                           type = gtk.MESSAGE_WARNING,
                           buttons = gtk.BUTTONS_YES_NO,
                           message_format = "File %s already exists, overwrite?" %(filename,))
    answer = md.run()
    md.destroy()
    return answer == gtk.RESPONSE_YES

def python_export_bw_scaled(img, drawable) :
    global export_width
    new_width = min(img.width, export_width)
    new_height = img.height * new_width / img.width

    origdir = os.path.dirname(img.filename)
    base, ext = os.path.splitext(os.path.basename(img.filename))

    if not any([l.name == 'vm' for l in img.layers]):
        md = gtk.MessageDialog(flags = gtk.DIALOG_DESTROY_WITH_PARENT,
                               type = gtk.MESSAGE_WARNING,
                               buttons = gtk.BUTTONS_CLOSE,
                               message_format = "No layer named 'vm'! Make one and try again :-)")
        md.run()
        md.destroy()
        raise gimp.error("No layer named 'vm'!")

    # farge, full storleik, utan vassmerke
    tmp = img.duplicate()
    for l in tmp.layers:
        if l.name == 'vm':
            l.visible = False
    python_export_bw_scaled_jpeg(tmp, base, origdir, "farge")

    # farge, skalert ned, med vassmerke
    tmp = img.duplicate()
    pdb.gimp_image_scale(tmp, new_width, new_height)
    for l in tmp.layers:
        if l.name == 'vm':
            l.visible = True
    python_export_bw_scaled_jpeg(tmp, base, origdir, "farge_til_nettbruk")

    # svartkvitt, full storleik, utan vassmerke
    tmp = img.duplicate()
    tmp.flatten()
    pdb.gimp_desaturate_full(pdb.gimp_image_get_active_layer(tmp),
                             DESATURATE_LUMINOSITY)
    for l in tmp.layers:
        if l.name == 'vm':
            l.visible = False
    python_export_bw_scaled_jpeg(tmp, base, origdir, "svarthvitt")

    # svartkvitt, skalert ned, med vassmerke
    tmp = img.duplicate()
    tmp.flatten()
    pdb.gimp_desaturate_full(pdb.gimp_image_get_active_layer(tmp),
                             DESATURATE_LUMINOSITY)
    pdb.gimp_image_scale(tmp, new_width, new_height)
    for l in tmp.layers:
        if l.name == 'vm':
            l.visible = True
    python_export_bw_scaled_jpeg(tmp, base, origdir, "svarthvitt_til_nettbruk")




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
