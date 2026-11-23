# -*- mode: CMake; cmake-tab-width: 4; -*-
#
# Points a translated manual at its own screenshots.
#
# The translated manuals (wxmaxima.<lang>.md, written by po4a) reference the
# same images as the English one: "![...](./Name.png){ id=img_Name }". For a
# language that has a screenshot set of its own, info/<lang>/Name.png shows
# the user interface in that language, and this rewrites each reference to an
# image that set contains into
#
#     ![...](<lang>/Name.png){ id=<lang>_img_Name }
#
# Images the set lacks (the ones still made by hand, a scene added after the
# set was made) keep pointing at the English image, so a partial set is
# fine.
#
# The id changes too because wxmaxima.css gives every "#img_Name" the size
# of the English image; a translated screenshot is usually a different size
# (longer menu names), and that rule would stretch it. Its own rule is
# "#<lang>_img_Name", see wxm_localised_image_css().
#
# Running it twice changes nothing: a rewritten reference no longer starts
# with "./".

# Rewrites the image references in MDFILE (in place) for the images found
# in IMAGEDIR, which are referenced as <LANG>/<file>.
function(wxm_localise_manual_images MDFILE LANG IMAGEDIR)
	if(NOT EXISTS "${MDFILE}" OR NOT IS_DIRECTORY "${IMAGEDIR}")
		return()
	endif()
	file(GLOB images RELATIVE "${IMAGEDIR}" "${IMAGEDIR}/*.png")
	if(NOT images)
		return()
	endif()
	file(READ "${MDFILE}" text)
	set(original "${text}")
	foreach(image ${images})
		# po4a may wrap a long line between "{" and "id=", hence the regex.
		# A reference without an id (the English manual gives every image
		# one) only gets its path changed.
		string(REPLACE "." "\\." pattern "${image}")
		string(REGEX REPLACE "\\(\\./${pattern}\\)\\{([ \t\r\n]*)id="
			"(${LANG}/${image}){\\1id=${LANG}_" text "${text}")
		string(REPLACE "(./${image})" "(${LANG}/${image})" text "${text}")
	endforeach()
	if(NOT text STREQUAL original)
		file(WRITE "${MDFILE}" "${text}")
	endif()
endfunction()

# Appends to OUTVAR the CSS size rules for the images in IMAGEDIR, as
# "#<LANG>_img_<name> {width: ...; height: ...;}", using ImageMagick's
# identify at IDENTIFY. Does nothing without it, as for the English images.
function(wxm_localised_image_css OUTVAR LANG IMAGEDIR IDENTIFY)
	set(css "${${OUTVAR}}")
	if(IDENTIFY AND IS_DIRECTORY "${IMAGEDIR}")
		file(GLOB images "${IMAGEDIR}/*.png")
		foreach(image ${images})
			get_filename_component(name ${image} NAME_WE)
			execute_process(COMMAND ${IDENTIFY}
				-format "#${LANG}_img_${name} {width: %wpx; height: %hpx;}" ${image}
				OUTPUT_VARIABLE rule)
			string(APPEND css "${rule}\n")
		endforeach()
	endif()
	set(${OUTVAR} "${css}" PARENT_SCOPE)
endfunction()
