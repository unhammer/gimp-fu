    #!/usr/bin/env python

from gimpfu import *
import os, sys

export_width = 1600

def python_export_bw_scaled_jpeg(img, base, origdir, subdir):
    subdir_full = os.path.join(origdir,subdir)
    if not os.path.isdir(subdir_full):
        os.mkdir(subdir_full)
    jpgname = os.path.join(subdir_full, base+".jpg")
    print >>sys.stderr, jpgname
    img.flatten()
    pdb.file_jpeg_save( # RUN-NONINTERACTIVE
        img,
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
    pdb.gimp_image_delete(tmp)

def python_export_bw_scaled(img, drawable) :
    global export_width
    export_height = img.height * export_width / img.width

    origdir = os.path.dirname(img.filename)
    base, ext = os.path.splitext(os.path.basename(img.filename))

    if not any([l.name == 'vm' for l in img.layers]):
        # Will this show to the user? TODO
        raise gimp.error("No layer named 'vm'!")

    tmp = img.duplicate()
    for l in tmp.layers:
        if l.name == 'vm':
            l.visible = False
    python_export_bw_scaled_jpeg(tmp, base, origdir, "farge")

    tmp = img.duplicate()
    pdb.gimp_image_scale(tmp, export_width, export_height)
    for l in tmp.layers:
        if l.name == 'vm':
            l.visible = False


    python_export_bw_scaled_jpeg(tmp, base, origdir, "farge_til_nettbruk")

    # TODO the other four
    # tmp = img.duplicate()
    # tmp.flatten()




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

