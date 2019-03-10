###
###  Quick python script to convert maxima documentation TOC to index.hhk and contents.hhc
###  used for wxMaxima documentation
###
###  Copyrigth: 2006 Andrej Vodopivec
###  Licence: GPL
###

import re

TOC_FILE = "maxima_toc.html"
INDEX_FILES = ["maxima_44.html", "maxima_45.html", "maxima_46.html", "maxima_47.html",
  "maxima_48.html", "maxima_49.html", "maxima_50.html", "maxima_51.html", "maxima_52.html",
  "maxima_53.html", "maxima_54.html", "maxima_55.html"]


def convert_index(files):
  index = open("index.hhk", "w")
  fun = re.compile(".*?\"top\"><a\shref=\"(.*?)\"><code>(.*?)</code></a>*")
  var = re.compile(".*?\"top\"><a\shref=\"(.*?)\">(.*?)</a>*")
  for f in files:
    in_file = open(f, "r")
    for l in in_file.readlines():
      m = fun.search(l)
      if m:
        index.write("<li><object type=\"text/sitemap\">\n")
        index.write("   <param name=\"Local\" value=\"" + m.group(1) + "\">\n")
        index.write("   <param name=\"Name\" value=\"" + m.group(2) + "\"></object>\n")
      else:
        m = var.search(l)
        if m:
          index.write("<li><object type=\"text/sitemap\">\n")
          index.write("   <param name=\"Local\" value=\"" + m.group(1) + "\">\n")
          index.write("   <param name=\"Name\" value=\"" + m.group(2) + "\"></object>\n")
  index.close


def convert_toc(toc):
  with open(toc, "r") as f:
      lines = f.readlines()
  contents = open("contents.hhc", "w")
  li = re.compile(".*?<li><a\sname=\"(.*?\")\shref=\"(.*?)\">.*?\s(.*?)</a>")
  ul = re.compile(".*<ul class=\"toc\">")
  ul_end = re.compile(".*</ul>")
  for l in lines:
    m = ul.search(l)
    if m:
      contents.write("<ul>\n")
    m = ul_end.search(l)
    if m:
      contents.write("</ul>\n")
    m = li.search(l)
    if m:
      contents.write("<li><object type=\"text/sitemap\">\n")
      contents.write("   <param name=\"Local\" value=\"" + m.group(2) + "\">\n")
      contents.write("   <param name=\"Name\" value=\"" + m.group(3) + "\"></object>\n")
  contents.close()

if __name__ == "__main__":
  try:
    convert_toc(TOC_FILE)
  except:
    print("Error creating TOC file")
  try:
    convert_index(INDEX_FILES)
  except:
    print("Error creating INDEX file")
