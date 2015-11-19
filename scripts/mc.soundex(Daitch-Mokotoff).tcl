##
# Soundex(Daitch-Mokotoff) v1.0 ported to TCL by MC_8
##

##
# This script is written to be used for mc.respond.tcl.  Do not edit this script, mc.respond.tcl instead.
#
# Implementation of soundex algorithm as described by Knuth in volume
# 3 of The Art of Computer Programming, with ideas stolen from Ian
# Phillips <ian@pipex.net> and ported over to TCL by
# Carl M. Gregory <mc8@purehype.net>.  Ported over from Daitch-Mokotoff 
# coding chart documentation on http://www.jewishgen.org/infofiles/soundex.txt.
##

##
# History
##
# v1.0 (03.22.02)
#  MC_8 - Initial port.
##

##
# Syntax:
#  soundex <string>
#   returns: soundex string, in list format of every sound possibility.
#            if the string contains a non alpha character,
#            it will return the origional word (but continues to convert the rest)
##

proc soundex {arg} {
 set arg [list $arg]
 for {set temp(line:index) 0} {$temp(line:index) < [llength $arg]} {incr temp(line:index)} {
  set line [lindex $arg $temp(line:index)]
  for {set temp(word:index) 0} {$temp(word:index) < [llength [split $line]]} {incr temp(word:index)} {
   set word [lindex [split $line] $temp(word:index)]
   if {$word == ""} {continue}
   if {[regexp -- {[^a-zA-Z0-9^]} $word]} {continue}

   set word [string toupper $word]

  # 7
   regsub -all -- (SCHTSCH)+ $word 4 word
   regsub -all -- ^SCHTSCH   $word 2 word

  # 6
   regsub -all -- (SCHTSH|SCHTCH)+  $word 4 word
   regsub -all -- ^(SCHTSH|SCHTCH)  $word 2 word

  # 5
   regsub -all -- (SHTCH|SHTSH|STSCH|TTSCH|ZHDZH)+ $word 4 word
   regsub -all -- ^(SHTCH|SHTSH|STSCH|ZHDZH)       $word 2 word

  # 4
   regsub -all -- (SCHT|SCHD)+                                                   $word 43 word
   regsub -all -- (SHCH|STCH|STRZ|STRS|STSH|SZCZ|SZCS|TTCH|TSCH|TTSZ|ZDZH|ZSCH)+ $word 4  word
   regsub -all -- ^(SHCH|SCHT|SCHD|STCH|STRZ|STRS|STSH|SZCZ|SZCS|ZDZH)           $word 2  word

  # 3
   regsub -all -- (CHS)+                                                                     $word 54 word
   regsub -all -- (SZT|SHD|SZD|ZHD|SHT)+                                                     $word 43 word
   regsub -all -- (TRZ|TRS|TSH|DRZ|DRS|DSH|DSZ|DZH|DZS|TTS|TTZ|TZS|TSZ|ZSH|TCH|SCH|CSZ|CZS)+ $word 4  word
   regsub -all -- ^CHS                                                                       $word 5  word
   regsub -all -- ^(SZT|SHD|SZD|SHT|ZDZ|ZHD)                                                 $word 2  word

  # 2
   while {[regexp -- (.+)(AI|AJ|AY|AU|EI|EJ|EY|EU|IA|IE|IO|IU|OI|OJ|OY|UI|UJ|UY|UE) $word]} {
    regsub -all -- (.+)(AI|AJ|AY|AU|EI|EJ|EY|EU|IA|IE|IO|IU|OI|OJ|OY|UI|UJ|UY|UE) $word \\1^ word
   }
   if {[string match *CK* $word]} {
    regsub -all -- (CK)+ $word 45 temp(word_b)
    regsub -all -- (CK)+ $word 5  word
    lappend arg [join [lreplace [split [lindex $arg $temp(line:index)]] $temp(word:index) $temp(word:index) $temp(word_b)]]
   }
   if {[string match *CH* $word]} {
    regsub -all -- (CH)+ $word 4 temp(word_b)
    regsub -all -- (CH)+ $word 5 word
    lappend arg [join [lreplace [split [lindex $arg $temp(line:index)]] $temp(word:index) $temp(word:index) $temp(word_b)]]  
   }
   if {[regexp -- (RZ|RS) $word]} {
    regsub -all -- (RZ|RS)+ $word 4  temp(word_b)
    regsub -all -- (RZ|RS)+ $word 94 word
    lappend arg [join [lreplace [split [lindex $arg $temp(line:index)]] $temp(word:index) $temp(word:index) $temp(word_b)]]  
   }
   regsub -all -- (MN|NM)+                                     $word -66- word
   regsub -all -- (KS)+                                        $word 54   word
   regsub -all -- (SD|ZD|ST)+                                  $word 43   word
   regsub -all -- {(AU)([AEIOUY])}                             $word 7\\2 word
   regsub -all -- (PF|PH|FB)+                                  $word 7    word
   regsub -all -- ^KS                                          $word 5    word
   regsub -all -- (KH)+                                        $word 5    word
   regsub -all -- (CZ|CS|ZH|ZS|SZ|SC|DS|DZ|TS|TC|TZ|SH)+       $word 4    word
   regsub -all -- (TH|DT)+                                     $word 3    word
   regsub -all -- ^(SD|SC|ZD|ST)                               $word 2    word
   regsub -all -- ^(IA|IE|IO|IU|EU)                            $word 1    word
   regsub -all -- {(OI|OJ|OY|UI|UJ|UY|EI|EJ|EY|EU)([AEIOUY])}  $word 1\\2 word
   regsub -all -- ^(AI|AJ|AY|AU|UE|OI|OJ|OY|UI|UJ|UY|EI|EJ|EY) $word 0    word
   regsub -all -- {(AI|AJ|AY)([AEIOUY])}                       $word 0\\2 word

  # 1
   while {[regexp -- (.+)(A|E|H|I|O|U|Y) $word]} {
    regsub -all -- (.+)(A|E|H|I|O|U|Y) $word \\1^ word
   }
   if {[string match *C* $word]} {
    regsub -all -- (C)+ $word 4 temp(word_b)
    regsub -all -- (C)+ $word 5 word
    lappend arg [join [lreplace [split [lindex $arg $temp(line:index)]] $temp(word:index) $temp(word:index) $temp(word_b)]]
   }
   if {[string match *J* $word]} {
    regsub -all -- (J)+ $word 4 temp(word_b)
    regsub -all -- (J)+ $word 1 word
    lappend arg [join [lreplace [split [lindex $arg $temp(line:index)]] $temp(word:index) $temp(word:index) $temp(word_b)]]
   }
   regsub -all -- (X)+            $word 54   word
   regsub -all -- (R)+            $word 9    word
   regsub -all -- (L)+            $word 8    word
   regsub -all -- (F|P|B)+        $word 7    word
   regsub -all -- (V|W)+          $word 7    word
   regsub -all -- (M|N)+          $word -6-  word
   regsub -all -- ^(H|X)          $word 5    word
   regsub -all -- {(H)([AEIOUY])} $word 5\\2 word
   regsub -all -- (G|K|Q)+        $word 5    word
   regsub -all -- (Z|S)+          $word 4    word
   regsub -all -- (D|T)+          $word 3    word
   regsub -all -- ^Y              $word 1    word
   regsub -all -- ^(I|O|U|A|E)    $word 0    word

   foreach temp(number) [list 94 9 8 7 -66- -6- 54 5 45 43 4 3 2 1 0] {
    regsub -all -- $temp(number)$temp(number) $word $temp(number) word
   }
   regsub -all -- -66- $word 66 word; regsub -all -- -6- $word 6 word
   regsub -all -- {\^} $word "" word

   set line [join [lreplace [split $line] $temp(word:index) $temp(word:index) [string range ${word}000000 0 5]]]
  }; set arg [lreplace $arg $temp(line:index) $temp(line:index) $line]
 }; return $arg
}