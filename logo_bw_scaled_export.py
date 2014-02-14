#!/usr/bin/env python

# Copyright 2012 by Akkana Peck, http://www.shallowsky.com/software/
# You may use and distribute this plug-in under the terms of the GPL v2
# or, at your option, any later GPL version.

from gimpfu import *
import os, sys

export_width = 1600

def python_export_bw_scaled(img, drawable) :
    global export_width
    export_height = img.height * export_width / img.width

    origdir = os.path.dirname(img.filename)
    base, ext = os.path.splitext(os.path.basename(img.filename))

    tmp = img.duplicate()
    tmp.flatten()

    subdir = "farge"
    subdir_full = os.path.join(origdir,subdir)
    if not os.path.isdir(subdir_full):
        os.mkdir(subdir_full)
    jpgname = os.path.join(subdir_full, base+".jpg")
    print >>sys.stderr, jpgname
    pdb.file_jpeg_save( # RUN-NONINTERACTIVE
        tmp,
        pdb.gimp_image_get_active_layer(tmp),
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

    pdb.gimp_image_scale(tmp, export_width, export_height)
    # TODO: make logo layer visible
    subdir = "farge_til_nettbruk"
    subdir_full = os.path.join(origdir,subdir)
    if not os.path.isdir(subdir_full):
        os.mkdir(subdir_full)
    jpgname = os.path.join(subdir_full, base+".jpg")
    print >>sys.stderr, jpgname
    pdb.file_jpeg_save( # RUN-NONINTERACTIVE
        tmp,
        pdb.gimp_image_get_active_layer(tmp),
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

