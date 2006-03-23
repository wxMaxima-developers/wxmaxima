###
###  Quick python script to convert maxima documentation TOC to index.hhk and contents.hhc
###  used for wxMaxima documentation
###
###  Copyrigth: 2006 Andrej Vodopivec
###  Licence: GPL
###

import re

def convert(lines):
  li = re.compile(".*?<li><a\sname=\"(.*?\")\shref=\"(.*?)\">.*?\s(.*?)</a>")
  ul = re.compile(".*<ul class=\"toc\">")
  ul_end = re.compile(".*</ul>")
  index = open("index.hhk", "w")
  contents = open("contents.hhc", "w")
  TOC_depth = 0
  for l in lines:
    m = ul.search(l)
    if m:
      if TOC_depth==0:
        index.write("<ul>\n")
      contents.write("<ul>\n")
      TOC_depth = TOC_depth+1
    m = ul_end.search(l)
    if m:
      TOC_depth = TOC_depth-1
      if TOC_depth==0:
        index.write("</ul>\n")
      contents.write("</ul>\n")
    m = li.search(l)
    if m:
      index.write("<li><object type=\"text/sitemap\">\n")
      index.write("   <param name=\"Local\" value=\"" + m.group(2) + "\">\n")
      index.write("   <param name=\"Name\" value=\"" + m.group(3) + "\"></object>\n")
      contents.write("<li><object type=\"text/sitemap\">\n")
      contents.write("   <param name=\"Local\" value=\"" + m.group(2) + "\">\n")
      contents.write("   <param name=\"Name\" value=\"" + m.group(3) + "\"></object>\n")
  index.close()
  contents.close()

if __name__ == "__main__":
  try:
    toc = open("maxima_toc.html", "r")
    lines = toc.readlines()
    toc.close()
    convert(lines)
  except:
    print "Can't find `maxima_toc.html' file"
