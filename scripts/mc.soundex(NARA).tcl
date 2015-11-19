##
# Soundex(NARA) v1.0 ported to TCL by MC_8
##

##
# This script is written to be used for mc.respond.tcl.  Do not edit 
# this script, mc.respond.tcl instead.
#
# Implementation of soundex algorithm as described by Knuth in volume 3 
# of The Art of Computer Programming, with ideas stolen from Ian 
# Phillips <ian@pipex.net> and ported over to TCL by Carl M. Gregory 
# <mc8@purehype.net>.  Ported over from documentation via 
# http://www.jewishgen.org/infofiles/soundex.txt.
#
# Knuth's test cases are:
# 
# Euler, Ellery -> E460
# Gauss, Ghosh -> G200
# Hilbert, Heilbronn -> H416
# Knuth, Kant -> K530
# Lloyd, Ladd -> L300
# Lukasiewicz, Lissajous -> L222
##

##
# History
##
# v1.0 (03.23.02)
#  MC_8 - Initial port.
##

##
# Syntax:
#  soundex <string>
#   returns: soundex string, list format
#            if the string contains a non alpha character,
#            it will return the origional word (but continues to 
#            convert the rest)
##

proc soundex {arg} {
 for {set temp(i) 0} {$temp(i) < [llength [split $arg]]} {incr temp(i)} {
  set word [lindex [split $arg] $temp(i)]
  if {($word == "") || ([regexp -- {[^a-zA-Z]} $word])} {continue}

  set word [string toupper $word]

  foreach temp(foreach) [list A B C D E F G H I J K L M N O P Q R S T U V W X Y Z] {
   regsub -all -- ($temp(foreach))+ $word \\1 word
  }

  set temp(a) [string index $word 0]
  set temp(b) [string range $word 1 end]

  regsub -all -- R            $temp(b) 6  temp(b)
  regsub -all -- {[MN]}       $temp(b) 5  temp(b)
  regsub -all -- L            $temp(b) 4  temp(b)
  regsub -all -- {[DT]}       $temp(b) 3  temp(b)
  regsub -all -- {[CGJKQSXZ]} $temp(b) 2  temp(b)
  regsub -all -- {[BFPV]}     $temp(b) 1  temp(b)
  regsub -all -- {[AEIOUYWH]} $temp(b) "" temp(b)

  set arg [join [lreplace [split $arg] $temp(i) $temp(i) [string range $temp(a)${temp(b)}000 0 3]]]
 }; return [list $arg]
}