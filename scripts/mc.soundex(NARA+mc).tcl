##
# Soundex(NARA+mc) v2.1.1 ported to TCL by MC_8
##

##
# This script is written to be used for mc.respond.tcl.  Do not edit 
# this script, mc.respond.tcl instead.
#
# Implementation of soundex algorithm as described by Knuth in volume 3 
# of The Art of Computer Programming, with ideas stolen from Ian 
# Phillips <ian@pipex.net> and ported over to TCL by Carl M. Gregory 
# <mc8@purehype.net>.  Ported over from perl, perl coded by Mike Stok 
# <Mike.Stok@meiko.concord.ma.us>, 2 March 1994.
#
# Knuth's test cases are:
# 
# Euler, Ellery -> E460
# Gauss, Ghosh -> G200
# Hilbert, Heilbronn -> H416
# Knuth, Kant -> K530
# Lloyd, Ladd -> L300
# Lukasiewicz, Lissajous -> L222
#
# In this version, it's not returned as ANNN (A= Alpha, N= Numberic).  
# Rather returns in N*.  This is to fix the sensitivity of the first 
# character within a string and removes the predetermined length of 
# accuracly.  I have tested, and it looks to work just fine... but 
# since this idea (soundex) goes back to at least 1918, I wonder why it 
# wasn't implimented a long time ago... maby there is a bug in doing it
# this way, I don't know.
#
# Also, KN == K, GHT == T, GHN == N , TCH == CH.
##

##
# History
##
# v2.1.1 (03.23.02)
#  MC_8 - Changed the return to list format, to comply with new format 
#         proposed in mc.respond v1.1.0.24.
#
# v2.1 (03.20.02)
#  MC_8 - Cleaned up the coding.
#
# v1.0-2.0 (04.11.01)
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
 set return ""
 foreach word [split $arg] {
  if {[regexp -- {[^a-zA-Z0-9]} $word]} {lappend return $word; continue}
  set word [string toupper $word]
  regsub -all -- KN           $word  K word
  regsub -all -- GHT          $word  T word
  regsub -all -- GHN          $word  N word
  regsub -all -- TCH          $word CH word
  regsub -all -- {[AEHIOUWY]} $word  0 word
  regsub -all -- {[BFPV]}     $word  1 word
  regsub -all -- {[CGJKQSXZ]} $word  2 word
  regsub -all -- {[DT]}       $word  3 word
  regsub -all -- {[L]}        $word  4 word
  regsub -all -- {[MN]}       $word  5 word
  regsub -all -- {[R]}        $word  6 word
  while {[string match *00* $word]} {regsub -all -- 00 $word 0 word}
  while {[string match *11* $word]} {regsub -all -- 11 $word 1 word}
  while {[string match *22* $word]} {regsub -all -- 22 $word 2 word}
  while {[string match *33* $word]} {regsub -all -- 33 $word 3 word}
  while {[string match *44* $word]} {regsub -all -- 44 $word 4 word}
  while {[string match *55* $word]} {regsub -all -- 55 $word 5 word}
  while {[string match *66* $word]} {regsub -all -- 66 $word 6 word}
  lappend return $word
 }; return [list [join $return]]
}