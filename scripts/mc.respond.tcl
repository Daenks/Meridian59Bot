# Beta, to be 2.0
##
# Respond v1.1.0.24
#   Programmer: MC_8 - Carl M. Gregory <mc8@purehype.net>
#   Documenter: bassin247 - Michael Kennedy <bassin247@eggheads.org>
#  This script will only run on eggdrop 1.6.7 or greater.
#
# My SVS Website - http://mc.purehype.net:81/
# Have a bug?  http://mc.purehype.net:81/bugzilla/
# TCL Mailing list - http://mc.purehype.net:81/maillist/
##

##
# Someone says a word match in the channel, will respond to it.  Sounds 
# easy?  There is alot more to it than that -- but that's basically 
# what it does.  Please refer to the documenation for extended 
# information.
##

##
# History - too long, check out doc/mc.respond.history
##

##
# Commands - see the doc/ folder
##

##
# Settings
##

# This script will respond once per match every x seconds.  What do you 
# want to set as the value of x?  Set this to 0 to disable.
set mc_re(disable) 1

# What frequency, in min's, do you want the bot to perform an anti-idle 
# response?  Set this to 0 to disable.
set mc_re(aidle) 30

# What do you want as triggers for the public response interface, to 
# add/remove/search/etc..  Sepeate each trigger with a space.
#   Replacment veriables:
#     %botnick -> The bots IRC Nickname.
set mc_re(public:triggers) "%botnick %botnick, !!"

# [0=no/1=yes] Do you want to strip the public text of the mIRC 
# compliant bold/reverse/color/underline characters before evaluating 
# it?  Makes it a little easier to set a respond+to without virtually 
# invisable characters.
set mc_re(strip) 1

# How many characters per second (cps) do you want the bot to emulate?  
# This will make it look more realistic.  The more the bot is to 
# respond with, the long it takes it to display -- as a human.  Most 
# bots reply instantly, that is one of the "non realistic" things about 
# Artifical Intelligence.  Set this to 0 to disable.
set mc_re(cps) 0

# What flags should a user have in order to access the response 
# interface, to add/remove/search/etc..
#   Flag format -> <global>|<channel>
# Set this to "" to give everyone access.
# Set to "-|-" to give only valid users access.  Valid users means 
# users in the bots userfile.
set mc_re(access:interface:flags) "oS|oS"

# What flags should a user have in order to add and remove responses 
# that command a /quote or /loop?
#   Flag format -> <global>|<channel>
# Set this to "" to give everyone access.
# Set this to "-|-" to give only valid users access.  Valid users means 
# users in the bots userfile.
set mc_re(access:quotenloop:flags) "m|"

# Where do you want this script to store it's database information?
set mc_re(respond_db) "text/respond.db"

# Where is the language file kept at?
set mc_re(language_db) "language/mc.respond.english.lang"

# Where is the soundex script kept at?
# Soundex is the "sound" of words, or the phonetic sound to be exact.  
# This is a great help since it avoids most problems of misspelling or 
# alternative spellings.
#   For example:  Scherman, Schurman, Sherman, Shireman and Shurman are 
#                 considered the same.
#   More information:  http://www.jewishgen.org/infofiles/soundex.txt
# Set this to "" to disable.
set mc_re(soundex) "scripts/mc.soundex(Daitch-Mokotoff).tcl"


## MC SVS (Script Version Service) v2.0 ##
# [0=no/1=yes] Do you want the bot to connection to MC_8's SVS to see
# if there are newer versions of this script avilable?  It will only
# tell you -- It's up to you to get the newest version (I decided not
# to auto update the code, there may be new features in the newest
# script that may require your attention).
set mc_re(svs:enabled) 1

# Once a day the script will check with the SVS -- this specifies when
# that should happen.  The setting should be the hour (military).
# examples:
#  set mc_re(svs:check) 0   ;#This is at Midnight
#  set mc_re(svs:check) 5   ;#This is at 5am
#  set mc_re(svs:check) 13  ;#This is at 1pm
#  set mc_re(svs:check) 23  ;#This is at 11pm
set mc_re(svs:check) 3

# Upon finding a newer version of this script, who do you want this
# script to send a note to telling them of this newer version? If more
# than 1 user is to be notified, seperate each user with a ' ' (space).
set mc_re(svs:notify) "MC_8"

# Do you want even beta version notified to whomever you have set?  Or
# just stable releases of the script?
#  0 = Tell me about all version, even beta versions.
#  1 = Just tell me about new stable versions.
set mc_re(svs:stableonly) 0


##
# Done with configurations, do not edit past here unless you know TCL.
##

#Script:mc_re

set mc_re(script)      "Respond"
set mc_re(version)     "v1.1.0.24"
set mc_re(svs:script)  "respond"
set mc_re(svs:version) "001001000024"
set mc_re(svs:server)  "mc.purehype.net"
set mc_re(svs:port)    "81"
set mc_re(svs:get)     "/"
set mc_re(svs:query)   "svs=$mc_re(svs:script)&version=$mc_re(svs:version)"

if {$numversion < "1060700"} {
 putlog "$mc_re(script) $mc_re(version) by MC_8 will only work on eggdrop 1.6.7 or greater."
 putlog "$mc_re(script) $mc_re(version)  will not work with eggdrop $version."
 putlog "$mc_re(script) $mc_re(version)  not loaded."
 return 1
}

# Make sure all veriables are set
if {![info exists mc_re(disable)]} {set mc_re(disable) 0}
if {![info exists mc_re(aidle)]} {set mc_re(aidle) 0}
if {![info exists mc_re(public:triggers)]} {set mc_re(public:triggers) ""}
if {![info exists mc_re(access:interface:flags)]} {set mc_re(access:interface:flags) "n|"}
if {![info exists mc_re(respond_db)]} {set mc_re(respond_db) ".respond.db"}
if {![info exists mc_re(access:quotenloop:flags)]} {set mc_re(access:quotenloop:flags) "n|"}
if {![info exists mc_re(strip)]} {set mc_re(strip) 1}
if {![info exists mc_re(cps)]} {set mc_re(cps) 0}
if {![info exists mc_re(soundex)]} {set mc_re(soundex) ""}
if {![info exists mc_re(language_db)]} {set mc_re(language_db) ""}
if {![info exists mc_re([list :: lang 0x001])]} {set mc_re([list :: lang 0x001]) "Info already exists for %array."}
if {![info exists mc_re([list :: lang 0x002])]} {set mc_re([list :: lang 0x002]) "Ok %nick, set."}
if {![info exists mc_re([list :: lang 0x003])]} {set mc_re([list :: lang 0x003]) "No match found, %i entries searched."}
if {![info exists mc_re([list :: lang 0x004])]} {set mc_re([list :: lang 0x004]) "1 match (out of %i entries) found for %search\; %matches"}
if {![info exists mc_re([list :: lang 0x005])]} {set mc_re([list :: lang 0x005]) "%t matches (out of %i entries) found for %search\; %matches"}
if {![info exists mc_re([list :: lang 0x006])]} {set mc_re([list :: lang 0x006]) "%t matches (out of %i entries) found for %search (only 25 displayed)\; %matches"}
if {![info exists mc_re([list :: lang 0x007])]} {set mc_re([list :: lang 0x007]) "%nick: %line"}
if {![info exists mc_re([list :: lang 0x008])]} {set mc_re([list :: lang 0x008]) "Cannot add onto %array, it doesn't exist yet!"}
if {![info exists mc_re([list :: lang 0x009])]} {set mc_re([list :: lang 0x009]) "%array... it doesn't exist."}
if {![info exists mc_re([list :: lang 0x00A])]} {set mc_re([list :: lang 0x00A]) "%nick, Anti-Idle information does exist."}
if {![info exists mc_re([list :: lang 0x00B])]} {set mc_re([list :: lang 0x00B]) "%nick, response contains a %cmd command.  You do not have sufficent access to do that!"}
if {![info exists mc_re([list :: lang 0x00C])]} {set mc_re([list :: lang 0x00C]) "Invalid number %index.  %name only goes up to %tindex."}

proc mc:re:error {bindproc error {arg ""}} {
 global mc_re version lastbind
 putlog "Error in script $mc_re(script) $mc_re(version)."
 putlog "       Last bind: $lastbind"
 putlog "       Procedure: $bindproc"
 putlog "            Args: $arg"
 putlog " Eggdrop version: $version"
 putlog "     TCL version: [info tclversion]"
 putlog "*** Please tell MC_8 about this BUG!  Visit        "
 putlog "*** http://mc.purehype.net:81/bugzilla/ to do so.  "
 putlog "*** Please include the next line in the bug report."
 error $error
}

proc mc:re:errchk {command arg} {
 if {[catch {eval $command $arg} return]} {
  mc:re:error $command $return $arg; return 0
 }; return $return
}

proc mc:re:list {command {arg ""}} {
 global mc_re botnet-nick
 switch -- $command {
  load_db {
   if {![file exists $mc_re(respond_db)]} {close [open $mc_re(respond_db) w]; return 1}
   set io [open $mc_re(respond_db) r]; set temp(i) 0
   while {![eof $io]} {
    gets $io line; incr temp(i)
    if {$temp(i) == "1"} {
     if {[string range $line 0 2] == "#^v"} {
      set temp(version) [lindex [split [string range $line 2 end] :] 0]; continue
     } else {set temp(version) old}
    }
    switch -- $temp(version) {
     old {
      set temp(line) ""; set buffer(sline) [split $line]
      set buffer(sline:0) [lindex $buffer(sline) 0]
      set buffer(sline:1) [lindex $buffer(sline) 1]
      if {$buffer(sline:0) == "anti-idle"} {
       foreach temp(or) [split [mc:re:replace [join [lrange $buffer(sline) 1 end]] [list " -OR- " \007]] \007] {
        lappend temp(line) [split [mc:re:replace $temp(or) [list " -AND- " \007]] \007]
       }; set mc_re([list :: list anti-idle]) $temp(line)
      } else {
       foreach temp(or) [split [mc:re:replace [join [lrange $buffer(sline) 2 end]] [list " -OR- " \007]] \007] {
        if {[string match *|* [lindex [split $temp(or)] 0]]} {
         if {[validchan [lindex [split $temp(or)] 1]]} {
          set temp(access) [lrange [split $temp(or)] 0 1]
          set temp(or) [join [lrange [split $temp(or)] 2 end]]
         } else {
          set temp(access) [list [lindex [split $temp(or)] 0]]
          set temp(or) [join [lrange [split $temp(or)] 1 end]]
         }
        } else {set temp(access) ""}
        lappend temp(line) "[list $temp(access)] [split [mc:re:replace $temp(or) [list " -AND- " \007]] \007]"
       }; set mc_re([list :: list respond $buffer(sline:0) $buffer(sline:1)]) $temp(line)
      }
     }
     v1.1 {
      if {[lindex $line 0] == "anti-idle"} {set mc_re([list :: list anti-idle]) [lindex $line 1]} \
      elseif {[lindex $line 0] == "respond"} {
       set mc_re([list :: list respond [lindex $line 1] [lindex $line 2]]) [lindex $line 3]
      }
     }
     default {return -code error "$mc_re(script):  Invalid database format."}
    }
   }; close $io; return 1
  }
  save_db {
   if {$arg != "-quiet"} {putlog "Writing mc.respond file..."}
   set io [open $mc_re(respond_db) w]
   puts $io "#^v1.1:$mc_re(script):$mc_re(version):${botnet-nick}:[clock seconds]"
   foreach array_name [array name mc_re "[list :: list] *"] {
    if {[lindex $array_name 2] == "anti-idle"} {puts $io [list anti-idle $mc_re($array_name)]} \
    elseif {[lindex $array_name 2] == "respond"} {
     puts $io [list respond [lindex $array_name 3] [lindex $array_name 4] $mc_re($array_name)]
    }
   }; close $io; return 1
  }
  anti-idle:timer {
   while {[set index [lsearch -glob [timers] *mc:re:anti-idle*]] > "-1"} {
    killtimer [lindex [lindex [timers] $index] 2]
   }; if {$mc_re(aidle)} {timer $mc_re(aidle) mc:re:anti-idle}
  }
  load_lang {
   if {$mc_re(language_db) == ""} {return 0}
   if {![file exists $mc_re(language_db)]} {
    putlog "$mc_re(script):  Can not load language file, doesn't exist\; $mc_re(language_db)"
    return 0
   }
   set io [open $mc_re(language_db) r] 
   while {![eof $io]} {
    gets $io line
    if {[string match 0x* $line]} {
     set mc_re([list :: lang [lindex [split $line] 0]]) [join [lrange [split $line] 1 end]]
    }
   }; close $io
  }
  load_soundex {
   if {$mc_re(soundex) == ""} {return 0}
   if {![file exists $mc_re(soundex)]} {
    putlog "$mc_re(script):  Can not load soundex script, doesn't exist\; $mc_re(soundex)"
    return 0
   }
   if {[catch {source $mc_re(soundex)} error]} {
    putlog "$mc_re(script):  Can not load soundex script\;"
    foreach line [split $error \n] {putlog "    $line"}
    set mc_re(soundex) ""
   } elseif {[info procs soundex] == ""} {
    putlog "$mc_re(script):  Can not load soundex script\;"
    putlog "    $mc_re(soundex) does not support the soundex 'proc'edure that $mc_re(script) is looking for."
    set mc_re(soundex) ""

   } elseif {[catch {soundex "soundex procedure test"} error]} {
    putlog "$mc_re(script):  Can not load soundex script\;"
    putlog "    $mc_re(soundex) returned an error when performing initial test:"
    putlog "     set temp \[soundex \"soundex procedure test\"\]"
    foreach line [split $error \n] {putlog "     $line"}
    set mc_re(soundex) ""
   }
  }
 }
}

# v1.0
bind pubm - * mc:re:pubm
proc mc:re:pubm {nick uhost hand chan arg} {
 return [mc:re:errchk mc:re:pubm_ [list $nick $uhost $hand $chan $arg]]
}
proc mc:re:pubm_ {nick uhost hand chan arg} {
 global mc_re botnick
 if {$mc_re(strip)} {set arg [mc:re:strip:all $arg]}
 if {[mc:re:pubm:cmd $nick $uhost $hand $chan $arg]} {return 2}
 set buffer(larg) [string tolower $arg]
 set host [string range $uhost [expr [string last @ $uhost]+1] end]
 set buffer(lchan) [string tolower $chan]

 # Only reply to users once every x seconds.
 if {($mc_re(disable)) && ([info exists mc_re([list :: disable $host])])} {return 0}

 # Sort by search string length.
 set temp(array_names) ""
 foreach array_name [array name mc_re "[list :: list respond] *"] {
  lappend temp(array_names) [list [string length [lindex $array_name 4]] $array_name]
 }; set temp(old) [lsort -dictionary -index 0 -decreasing $temp(array_names)]; set temp(array_names) ""
 foreach array_name $temp(old) {lappend temp(array_names) [lindex $array_name 1]}

 foreach array_name $temp(array_names) {
  set temp(array:channels) [lindex $array_name 3]
  if {$temp(array:channels) == "all"} {set temp(array:channels) [string tolower [channels]]} \
  else {set temp(array:channels) [split $temp(array:channels) ,]}

  # Validate channel
  if {[lsearch -exact $temp(array:channels) $buffer(lchan)] == "-1"} {continue}

  set temp(search) [mc:re:replace [lindex $array_name 4] [list + " " %botnick $botnick]]
  set temp(output) $mc_re($array_name)

  # Perform soundex support.
  regsub -all -- {%[^ ]*} $temp(search) * temp(temp:search)
  while {[regexp -- {\*\*} $temp(temp:search)]} {regsub -all -- {\*\*} $temp(temp:search) * temp(temp:search)}
  if {$mc_re(soundex) != ""} {
   set temp(arg:soundex) [soundex $arg]
   set temp(search:soundex) [soundex $temp(temp:search)]
   set temp(:search:soundex) [soundex $temp(search)]
  } else {
   set temp(arg:soundex) [list $arg]
   set temp(search:soundex) [list $temp(temp:search)]
   set temp(:search:soundex) [list $temp(search)]
  }; set temp(break) 0
  for {set temp(a:i) 0} {$temp(a:i) < [llength $temp(search:soundex)]} {incr temp(a:i)} {
   set temp(a) [lindex $temp(search:soundex) $temp(a:i)]
   foreach temp(b) [string tolower $temp(arg:soundex)] {
    if {[string match $temp(a) $temp(b)]} {
     set temp(:search:soundex) [lindex $temp(:search:soundex) $temp(a:i)]
     set temp(break) 1
     break
    }
   }; if {$temp(break)} {break}
  }; if {!$temp(break)} {continue}

  # Filter to valid responses.
  set temp(temp) ""
  foreach temp(foreach) $temp(output) {
   set temp(access:flag) [lindex [lindex $temp(foreach) 0] 0]
   set temp(access:chan) [lindex [lindex $temp(foreach) 0] 1]
   set temp(foreach) [lrange $temp(foreach) 1 end]
   if {$temp(access:flag) != ""} {
    if {([validchan $temp(access:chan)]) && (![matchattr $hand $temp(access:flag) $temp(access:chan)])} {continue} \
    elseif {![matchattr $hand $temp(access:flag)]} {continue}
   }; lappend temp(temp) $temp(foreach)
  }; if {$temp(temp) == ""} {continue}; set temp(output) $temp(temp)

  # Generate random response (via 'or' list).
  set temp(length) [llength $temp(output)]
  if {$temp(length) == "1"} {set temp(index) 0} \
  else {
   set temp(list) [list :: random $array_name]; set temp(index) [rand $temp(length)]
   if {![info exists mc_re($temp(list))]} {set mc_re($temp(list)) $temp(index)}
   while {$temp(index) == $mc_re($temp(list))} {set temp(index) [rand $temp(length)]}
  }; set temp(output) [lindex $temp(output) $temp(index)]

  # Replace %veriables in search to the output.
  # Search       : hello %var
  # Arg          : hello MC_8
  # pre_Respond  : hello %var
  # post_Respond : hello MC_8
  set temp(i) -1; regsub -all -- {\*} $temp(:search:soundex) " " temp(temp:search); set temp(temp) ""
  foreach temp(foreach) [split $temp(temp:search)] {if {$temp(foreach) != ""} {lappend temp(temp) $temp(foreach)}}
  set temp(temp:search) [join $temp(temp)]
  foreach word [split $temp(arg:soundex)] {
   incr temp(i); set sword [lindex [split $temp(temp:search)] $temp(i)]
   if {[string tolower $word] != [string tolower $sword]} {
    if {[string index $sword 0] == "%"} {regsub -all -- $sword $temp(output) [lindex [split $arg] $temp(i)] temp(output)} \
    else {incr temp(i) -1}
   }
  }

  set temp(output) [mc:re:replace $temp(output) [list %nick $nick %uhost $uhost %hand $hand %chan $chan %botnick $botnick]]

  # Process %rnick and %rnick{token}
  if {[regexp -- %rnick $temp(output)]} {
   for {set temp(i) 0} {$temp(i) < [llength $temp(output)]} {incr temp(i)} {
    set temp(foreach) [lindex $temp(output) $temp(i)]
    if {[regexp -- {^%rnick(?:\{(.*)\})?$} $temp(foreach)]} {
     set temp(chanlist) [chanlist $chan]
     set temp(nick) [lindex $temp(chanlist) [rand [llength $temp(chanlist)]]]
     set temp(output) [mc:re:replace $temp(output) [list $temp(foreach) $temp(nick)]]
    }
   }
  }

  # Perform response
  set temp(last_utimer) 0
  foreach temp(foreach) $temp(output) {
   if {!$mc_re(cps)} {set temp(utimer) 1} else {
    set temp(utimer) [expr ([string length $temp(foreach)]/$mc_re(cps))+$temp(last_utimer)]
    set temp(last_utimer) $temp(utimer)
   }
   switch -- [string tolower [lindex [split $temp(foreach)] 0]] {
    /msg {
     utimer $temp(utimer) [list puthelp "PRIVMSG [lindex [split $temp(foreach)] 1] :[join [lrange [split $temp(foreach)] 2 end]]"]
    }
    /notice {
     utimer $temp(utimer) [list puthelp "NOTICE [lindex [split $temp(foreach)] 1] :[join [lrange [split $temp(foreach)] 2 end]]"]
    }
    /quote {
     utimer $temp(utimer) [list puthelp "[join [lrange [split $temp(foreach)] 1 end]]"]
    }
    /me {
     if {[validchan [lindex [split $temp(foreach)] 1]]} {
      utimer $temp(utimer) [list puthelp "PRIVMSG [lindex [split $temp(foreach)] 1] :\001ACTION [join [lrange [split $temp(foreach)] 2 end]]\001"]
     } else {
      utimer $temp(utimer) [list puthelp "PRIVMSG $chan :\001ACTION [join [lrange [split $temp(foreach)] 1 end]]\001"]
     }
    }
    /action {
     if {[validchan [lindex [split $temp(foreach)] 1]]} {
      utimer $temp(utimer) [list puthelp "PRIVMSG [lindex [split $temp(foreach)] 1] :\001ACTION [join [lrange [split $temp(foreach)] 2 end]]\001"]
     } else {
      utimer $temp(utimer) [list puthelp "PRIVMSG $chan :\001ACTION [join [lrange [split $temp(foreach)] 1 end]]\001"]
     }
    }
    /loop {mc:re:pubm $nick $uhost $hand $chan [join [lrange [split $temp(foreach)] 1 end]]}
    default {utimer $temp(utimer) [list puthelp "PRIVMSG $chan :$temp(foreach)"]}
   }
  }

  if {$mc_re(disable)} {
   set mc_re([list :: disable $host]) [clock seconds]
   utimer $mc_re(disable) [list catch "unset {mc_re([list :: disable $host])}"]
  }

  mc:re:list anti-idle reset; break
 }
}

# v1.0
bind ctcp - * mc:re:ctcp
proc mc:re:ctcp {nick uhost hand chan key arg} {
 return [mc:re:errchk mc:re:ctcp_ [list $nick $uhost $hand $chan $key $arg]]
}
proc mc:re:ctcp_ {nick uhost hand chan key arg} {
 if {![isbotnick $chan]} {mc:re:pubm $nick $uhost $hand $chan "\001$key $arg\001"}
 return 0
}

# v1.0
proc mc:re:pubm:cmd {nick uhost hand chan arg} {
 global mc_re botnick
 # Validate user by flags.
 if {(![matchattr $hand $mc_re(access:interface:flags) $chan]) && ($mc_re(access:interface:flags) != "")} {return 0}

 # See if the text is called by the proper triggers.
 set temp(return) 1
 foreach temp(trigger) [string tolower [split [mc:re:replace $mc_re(public:triggers) [list %botnick $botnick]]]] {
  if {![lsearch -exact [string tolower [split $arg]] $temp(trigger)]} {set temp(return) 0; break}
 }; if {$temp(return)} {return 0}
 
 set temp(command) [lindex [split $arg] 1]
 set temp(arg) [join [lrange [split $arg] 2 end]]
 switch -exact [string tolower $temp(command)] {

  search {
   if {[string tolower [lindex [split $temp(arg)] 0]] == "anti-idle"} {
    if {![info exists mc_re([list :: list anti-idle])]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x009]) [list %nick $nick %array anti-idle]]"
    } else {puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00A]) [list %nick $nick]]"}
    return 1
   }
   regsub -all -- " " $temp(arg) * temp(search); set temp(search) *[string tolower $temp(search)]*
   while {[regexp -- {\*\*} $temp(search)]} {regsub -all -- {\*\*} $temp(search) * temp(search)}
   set temp(found) ""
   foreach array_name [array names mc_re "[list :: list respond] *"] {
    if {[string match $temp(search) [string tolower [set temp(array_name) [lrange $array_name 3 4]]]]} {
     lappend temp(found) $temp(array_name)
    }
   }
   set temp(total) [llength [array names mc_re "[list :: list respond] *"]]
   if {$temp(found) == ""} {
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x003]) [list %total $temp(total)]]"
   } else {
    set temp(found:total) [llength $temp(found)]
    set temp(found) [join [lrange $temp(found) 0 9] ", "]
    if {$temp(found:total) == "1"} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x004]) [list %total   $temp(total) \
                                                                               %search  $temp(arg) \
                                                                               %matches $temp(found)]]"
    } elseif {$temp(found:total) > "10"} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x006]) [list %total   $temp(total) \
                                                                               %mtotal  $temp(found:total) \
                                                                               %search  $temp(arg) \
                                                                               %matches $temp(found)]]"
    } else {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x005]) [list %total   $temp(total) \
                                                                               %mtotal  $temp(found:total) \
                                                                               %search  $temp(arg) \
                                                                               %matches $temp(found)]]"
    }
   }; return 1
  }

  rsearch {
   regsub -all -- " " $temp(arg) * temp(search); set temp(search) *[string tolower $temp(search)]*
   while {[regexp -- {\*\*} $temp(search)]} {regsub -all -- {\*\*} $temp(search) * temp(search)}
   set temp(found) ""
   foreach array_name [array names mc_re "[list :: list respond] *"] {
    if {[string match $temp(search) [string tolower $mc_re($array_name)]]} {lappend temp(found) [lrange $array_name 2 3]}
   }
   set temp(total) [llength [array names mc_re "[list :: list respond] *"]]
   if {$temp(found) == ""} {
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x003]) [list %total $temp(total)]]"
   } else {
    set temp(found:length) [llength $temp(found)]
    set temp(found) [join [lrange $temp(found) 0 9] ", "]
    if {$temp(found:length) == "1"} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x004]) [list %total   $temp(total) \
                                                                               %search  $temp(arg) \
                                                                               %matches $temp(found)]]"
    } elseif {$temp(found:length) > "10"} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x006]) [list %total   $temp(total) \
                                                                               %mtotal  $temp(found:total) \
                                                                               %search  $temp(arg) \
                                                                               %matches $temp(found)]]"
    } else {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x005]) [list %total   $temp(total) \
                                                                               %mtotal  $temp(found:total) \
                                                                               %search  $temp(arg) \
                                                                               %matches $temp(found)]]"
    }
   }; return 1
  }

all hey is for horses -OR- hoo... hey... hooo -AND- /action goes back to his corner
anti-idle /msg #abc123 *yawn*
to
anti-idle {{1} {2}} {{1} {2}}

  info {
   if {[string tolower [lindex [split $temp(arg)] 0]] == "anti-idle"} {
    set array_name [list list :: list anti-idle]
    if {![info exists mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x009]) [list %nick $nick %array anti-idle]]"
    } else {
     set temp(or:i) 0
     foreach temp(or) $mc_re($array_name) {
      incr temp(or:i)
      puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x007]) [list %nick  $nick \
                                                                                %name  $temp(arg) \
                                                                                %value "Or #$temp(or:i)"
      set temp(and:i) 0
      foreach temp(and) $temp(or) {
       incr temp(and:i)
       puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x007]) [list %nick  $nick \
                                                                                 %name  $temp(arg) \
                                                                                 %value "  And #$temp(and:i): $temp(and)"]]"
      }
     }
    }; return 1
   }
   set temp(total) [llength [array names mc_re "[list :: list respond] *"]]
   set temp(chan) [lindex [split $temp(arg)] 0]
   set temp(trig) [lindex [split $temp(arg)] 1]
   if {($temp(chan) == "") || ($temp(trig) == "")} {return 0}
   set array_name [list :: list respond $temp(chan) $temp(trig)]
   if {![info exists mc_re($array_name)]} {
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x003]) [list %total $temp(total)]]"
   } else {
    set temp(or:i) 0
    foreach temp(or) $mc_re($array_name) {
     incr temp(or:i)
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x007]) [list %nick  $nick \
                                                                               %name  $temp(arg) \
                                                                               %value "Or #$temp(or:i)   ---   Flag requirment:  [lindex [lindex $temp(or) 0] 0] [lindex [lindex $temp(or) 0] 1]"]]"
     set temp(and:i) 0
     foreach temp(and) [lrange $temp(or) 1 end] {
      incr temp(and:i)
      puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x007]) [list %nick  $nick \
                                                                                %name  $temp(arg) \
                                                                                %value "  And #$temp(and:i): $temp(and)"]]"
     }
    }
   }; return 1
  }

  add {
   if {[string tolower [lindex [split $temp(arg)] 0]] == "anti-idle"} {
    if {[info exists mc_re([list :: list anti-idle])]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x001]) [list %nick  $nick \
                                                                               %array Anti-Idle]]"
     return 1
    }
    set temp(add) [join [lrange [split $temp(arg)] 1 end]]
    if {$temp(add) == ""} {return 0} 
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan]) && 
        ([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(add)] 0]] cmd])} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
     return 1
    }
    set mc_re([list :: list anti-idle]) [list [list $temp(add)]]
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   } else {
    set temp(chan) [lindex [split $temp(arg)] 0]
    set temp(trig) [lindex [split $temp(arg)] 1]
    if {[string match *|* [lindex [split $temp(arg)] 2]]} {
     if {[validchan [lindex [split $temp(arg)] 3]]} {
      set temp(flag) [list [lindex [split $temp(arg)] 2] [lindex [split $temp(arg)] 3]]
      set temp(add) [join [lrange [split $temp(arg)] 4 end]]
     } else {
      set temp(flag) [list [lindex [split $temp(arg)] 2]]
      set temp(add) [join [lrange [split $temp(arg)] 3 end]]
     }
    } else {
     set temp(flag) ""
     set temp(add) [join [lrange [split $temp(arg)] 2 end]]
    }
    if {($temp(chan) == "") || ($temp(trig) == "") || ($temp(add) == "")} {return 0}
    set array_name [list :: list respond $temp(chan) $temp(trig)]
    if {[info exists mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x001]) [list %nick  $nick \
                                                                               %array "$temp(chan) $temp(trig)"]]"
     return 1
    }
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan]) && 
        ([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(add)] 0]] cmd])} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
     return 1
    }
    set mc_re($array_name) [list [list $temp(flag) $temp(add)]]
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   }; mc:re:list save_db -quiet; return 1
  }

  addor {
   if {[string tolower [lindex [split $temp(arg)] 0]] == "anti-idle"} {
    if {![info exists mc_re([list :: list anti-idle])]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x008]) [list %nick  $nick \
                                                                               %array Anti-Idle]]"
     return 1
    }
    set temp(add) [join [lrange [split $temp(arg)] 1 end]]
    if {$temp(add) == ""} {return 0} 
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan]) && 
        ([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(add)] 0]] cmd])} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
     return 1
    }
    lappend mc_re([list :: list anti-idle]) $temp(add)
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   } else {
    set temp(chan) [lindex [split $temp(arg)] 0]
    set temp(trig) [lindex [split $temp(arg)] 1]
    if {[string match *|* [lindex [split $temp(arg)] 2]]} {
     if {[validchan [lindex [split $temp(arg)] 3]]} {
      set temp(flag) [list [lindex [split $temp(arg)] 2] [lindex [split $temp(arg)] 3]]
      set temp(add) [join [lrange [split $temp(arg)] 4 end]]
     } else {
      set temp(flag) [list [lindex [split $temp(arg)] 2]]
      set temp(add) [join [lrange [split $temp(arg)] 3 end]]
     }
    } else {
     set temp(flag) ""
     set temp(add) [join [lrange [split $temp(arg)] 2 end]]
    }
    if {($temp(chan) == "") || ($temp(trig) == "") || ($temp(add) == "")} {return 0}
    set array_name [list :: list respond $temp(chan) $temp(trig)]
    if {![info exists mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x008]) [list %nick  $nick \
                                                                               %array "$temp(chan) $temp(trig)"]]"
     return 1
    }
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan]) && 
        ([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(add)] 0]] cmd])} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
     return 1
    }
    lappend mc_re($array_name) [list $temp(flag) $temp(add)]
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   }; mc:re:list save_db -quiet; return 1
  }

  addand {
   if {[string tolower [lindex [split $temp(arg)] 0]] == "anti-idle"} {
    set array_name [list :: list anti-idle]
    if {![info exists mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x008]) [list %nick  $nick \
                                                                               %array Anti-Idle]]"
     return 1
    }
    set temp(index) [expr [lindex [split $temp(arg)] 1]-1]
    set temp(add) [join [lrange [split $temp(arg)] 2 end]]
    if {([regexp -- {[^0-9]} $temp(index)]) || ($temp(add) == "")} {return 0} 
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan]) && 
        ([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(add)] 0]] cmd])} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
     return 1
    }
    if {$temp(index) >= [llength $mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00C]) [list %nick   $nick \
                                                                               %index [expr $temp(index)+1] \
                                                                               %name   "Anti-Idle" \
                                                                               %tindex [llength $mc_re($array_name)]]]"
     return 1
    }
    set mc_ar($array_name) [lreplace $mc_re($array_name) $temp(index) $temp(index) "[lindex $mc_re($array_name) $temp(index)] [list $temp(add)]"]
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   } else {
    set temp(chan) [lindex [split $temp(arg)] 0]
    set temp(trig) [lindex [split $temp(arg)] 1]
    set temp(index) [expr [lindex [split $temp(arg)] 2]-1]
    set temp(add) [join [lrange [split $temp(arg)] 3 end]]
    set array_name [list :: list respond $temp(chan) $temp(trig)]
    if {($temp(chan) == "") || ($temp(trig) == "") || ($temp(add) == "") || ([regexp -- {[^0-9]} $temp(index)])} {return 0}
    if {![info exists mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x008]) [list %nick  $nick \
                                                                               %array "$temp(chan) $temp(trig)"]]"
     return 1
    }
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan]) && 
        ([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(add)] 0]] cmd])} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
     return 1
    }
    if {$temp(index) >= [llength $mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00C]) [list %nick   $nick \
                                                                               %index [expr $temp(index)+1] \
                                                                               %name   "$temp(chan) $temp(trig)" \
                                                                               %tindex [llength $mc_re($array_name)]]]"
     return 1
    }
    set mc_re($array_name) [lreplace $mc_re($array_name) $temp(index) $temp(index) "[lindex $mc_re($array_name) $temp(index)] [list $temp(add)]"]
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   }; mc:re:list save_db -quiet; return 1
  }

  del {
   if {[string tolower [lindex [split $temp(arg)] 0]] == "anti-idle"} {
    set array_name [list :: list anti-idle]
    if {![info exists mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x009]) [list %nick  $nick \
                                                                               %array Anti-Idle]]"
     return 1
    }
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan])} {
     foreach temp(or) $mc_re($array_name) {
      foreach temp(and) [lrange $temp(or) 1 end] {
       if {([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(and)] 0]] cmd])} {
        puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
        return 1
       }
      }
     }
    }
    unset mc_re($array_name)
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   } else {
    set temp(chan) [lindex [split $temp(arg)] 0]
    set temp(trig) [lindex [split $temp(arg)] 1]
    set array_name [list :: list respond $temp(chan) $temp(trig)]
    if {($temp(chan) == "") || ($temp(trig) == "")} {return 0}
    if {![info exists mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x009]) [list %nick  $nick \
                                                                               %array "$temp(chan) $temp(trig)"]]"
     return 1
    }
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan])} {
     foreach temp(or) $mc_re($array_name) {
      foreach temp(and) [lrange $temp(or) 1 end] {
       if {([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(and)] 0]] cmd])} {
        puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
        return 1
       }
      }
     }
    }
    unset mc_re($array_name)
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   }; mc:re:list save_db -quiet; return 1
  }

  delor {
   if {[string tolower [lindex [split $temp(arg)] 0]] == "anti-idle"} {
    set array_name [list :: list anti-idle]
    if {![info exists mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x009]) [list %nick  $nick \
                                                                               %array Anti-Idle]]"
     return 1
    }
    set temp(index) [expr [lindex [split $temp(arg)] 1]-1]
    if {([regexp -- {[^0-9]} $temp(index)]) || ($temp(index) == "")} {return 0} 
    if {$temp(index) >= [llength $mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00C]) [list %nick   $nick \
                                                                               %index [expr $temp(index)+1] \
                                                                               %name   "Anti-Idle" \
                                                                               %tindex [llength $mc_re($array_name)]]]"
     return 1
    }
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan])} {
     foreach temp(or) [lindex $mc_re($array_name) $temp(index)] {
      foreach temp(and) [lrange $temp(or) 1 end] {
       if {([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(and)] 0]] cmd])} {
        puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
        return 1
       }
      }
     }
    }
    set mc_re($array_name) [lreplace $mc_re($array_name) $temp(index) $temp(index)]
    if {$mc_re($array_name) == ""} {unset mc_re($array_name)}
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   } else {
    set temp(chan) [lindex [split $temp(arg)] 0]
    set temp(trig) [lindex [split $temp(arg)] 1]
    set temp(index) [expr [lindex [split $temp(arg)] 2]-1]
    if {(([regexp -- {[^0-9]} $temp(index)]) || ($temp(index) == "")) ||
        ($temp(chan) == "") || ($temp(trig) == "")} {return 0}
    set array_name [list :: list respond $temp(chan) $temp(trig)]
    if {![info exists mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x009]) [list %nick  $nick \
                                                                               %array "$temp(chan) $temp(trig)"]]"
     return 1
    }
    if {$temp(index) >= [llength $mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00C]) [list %nick   $nick \
                                                                               %index [expr $temp(index)+1] \
                                                                               %name   "$temp(chan) $temp(trig)" \
                                                                               %tindex [llength $mc_re($array_name)]]]"
     return 1
    }
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan])} {
     foreach temp(or) [lindex $mc_re($array_name) $temp(index)] {
      foreach temp(and) [lrange $temp(or) 1 end] {
       if {([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(and)] 0]] cmd])} {
        puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
        return 1
       }
      }
     }
    }
    set mc_re($array_name) [lreplace $mc_re($array_name) $temp(index) $temp(index)]
    if {$mc_re($array_name) == ""} {unset mc_re($array_name)}
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   }; mc:re:list save_db -quiet; return 1
  }

  deland {
   if {[string tolower [lindex [split $temp(arg)] 0]] == "anti-idle"} {
    set array_name [list :: list anti-idle]
    if {![info exists mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x009]) [list %nick  $nick \
                                                                               %array Anti-Idle]]"
     return 1
    }
    set temp(or:index) [expr [lindex [split $temp(arg)] 1]-1]
    set temp(and:index) [lindex [split $temp(arg)] 2]
    if {(([regexp -- {[^0-9]} $temp(or:index)]) || ($temp(or:index) == "")) ||
        (([regexp -- {[^0-9]} $temp(and:index)]) || ($temp(and:index) == ""))} {return 0} 
    if {$temp(or:index) >= [llength $mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00C]) [list %nick   $nick \
                                                                               %index [expr $temp(or:index)+1] \
                                                                               %name   "Anti-Idle" \
                                                                               %tindex [llength $mc_re($array_name)]]]"
     return 1
    }
    if {$temp(and:index) >= [llength [lindex $mc_re($array_name) $temp(or:index)]]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00C]) [list %nick   $nick \
                                                                               %index  $temp(and:index) \
                                                                               %name   "Anti-Idle" \
                                                                               %tindex [expr [llength [lindex $mc_re($array_name) $temp(or:index)]]-1]]]"
     return 1
    }
    set temp(temp) [lindex [lindex $mc_re($array_name) $temp(or:index)] [expr $temp(and:index)+1]]
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan]) &&
        ([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(temp)] 0]] cmd])} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
     return 1
    }
    set mc_re($array_name) "[lrange $mc_re($array_name) 0 [expr $temp(or:index)-1]] [list [lreplace [lindex $mc_re($array_name) $temp(or:index)] $temp(and:index) $temp(and:index)]] [lrange $mc_re($array_name) [expr $temp(or:index)+1] end]"
    if {([llength $mc_re($array_name)] == "1") && ([lindex [lindex $mc_re($array_name) 0] 1] == "")} {unset mc_re($array_name)}
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   } else {
    set temp(chan) [lindex [split $temp(arg)] 0]
    set temp(trig) [lindex [split $temp(arg)] 1]
    set temp(or:index) [expr [lindex [split $temp(arg)] 2]-1]
    set temp(and:index) [lindex [split $temp(arg)] 3]
    if {($temp(chan) == "") || ($temp(trig) == "") || 
        (([regexp -- {[^0-9]} $temp(or:index)]) || ($temp(or:index) == "")) ||
        (([regexp -- {[^0-9]} $temp(and:index)]) || ($temp(and:index) == ""))} {return 0} 
    set array_name [list :: list respond $temp(chan) $temp(trig)]
    if {![info exists mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x009]) [list %nick  $nick \
                                                                               %array "$temp(chan) $temp(trig)"]]"
     return 1
    }
    if {$temp(or:index) >= [llength $mc_re($array_name)]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00C]) [list %nick   $nick \
                                                                               %index [expr $temp(or:index)+1] \
                                                                               %name   "$temp(chan) $temp(trig)" \
                                                                               %tindex [llength $mc_re($array_name)]]]"
     return 1
    }
    if {$temp(and:index) >= [llength [lindex $mc_re($array_name) $temp(or:index)]]} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00C]) [list %nick   $nick \
                                                                               %index $temp(and:index) \
                                                                               %name   "$temp(chan) $temp(trig)" \
                                                                               %tindex [expr [llength [lindex $mc_re($array_name) $temp(or:index)]]-1]]]"
     return 1
    }
    set temp(temp) [lindex [lindex $mc_re($array_name) $temp(or:index)] [expr $temp(and:index)+1]]
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan]) &&
        ([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(temp)] 0]] cmd])} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
     return 1
    }
    set mc_re($array_name) "[lrange $mc_re($array_name) 0 [expr $temp(or:index)-1]] [list [lreplace [lindex $mc_re($array_name) $temp(or:index)] $temp(and:index) $temp(and:index)]] [lrange $mc_re($array_name) [expr $temp(or:index)+1] end]"
    if {([llength $mc_re($array_name)] == "1") && ([lindex [lindex $mc_re($array_name) 0] 1] == "")} {unset mc_re($array_name)}
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   }; mc:re:list save_db -quiet; return 1
  }

  replace {
   if {[string tolower [lindex [split $temp(arg)] 0]] == "anti-idle"} {
    set array_name [list :: list anti-idle]
    if {([info exists mc_re($array_name)]) && ($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan])} {
     foreach temp(or) $mc_re($array_name) {
      foreach temp(and) [lrange $temp(or) 1 end] {
       if {([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(and)] 0]] cmd])} {
        puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
        return 1
       }
      }
     }
    }
    set temp(add) [join [lrange [split $temp(arg)] 1 end]]
    if {$temp(add) == ""} {return 0} 
    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan]) && 
        ([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(add)] 0]] cmd])} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
     return 1
    }
    set mc_re($array_name) [list [list $temp(add)]]
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   } else {
    set temp(chan) [lindex [split $temp(arg)] 0]
    set temp(trig) [lindex [split $temp(arg)] 1]
    set array_name [list :: list respond $temp(chan) $temp(trig)]

    if {([info exists mc_re($array_name)]) && ($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan])} {
     foreach temp(or) $mc_re($array_name) {
      foreach temp(and) [lrange $temp(or) 1 end] {
       if {([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(and)] 0]] cmd])} {
        puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
        return 1
       }
      }
     }
    }
    if {[string match *|* [lindex [split $temp(arg)] 2]]} {
     if {[validchan [lindex [split $temp(arg)] 3]]} {
      set temp(flag) [list [lindex [split $temp(arg)] 2] [lindex [split $temp(arg)] 3]]
      set temp(add) [join [lrange [split $temp(arg)] 4 end]]
     } else {
      set temp(flag) [list [lindex [split $temp(arg)] 2]]
      set temp(add) [join [lrange [split $temp(arg)] 3 end]]
     }
    } else {
     set temp(flag) ""
     set temp(add) [join [lrange [split $temp(arg)] 2 end]]
    }
    if {($temp(chan) == "") || ($temp(trig) == "") || ($temp(add) == "")} {return 0}

    if {($mc_re(access:quotenloop:flags) != "") && (![matchattr $hand $mc_re(access:quotenloop:flags) $chan]) && 
        ([regexp -- {^(/quote|/loop)$} [string tolower [lindex [split $temp(add)] 0]] cmd])} {
     puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x00B]) [list %nick $nick %cmd $cmd]]"
     return 1
    }
    set mc_re($array_name) [list [list $temp(flag) $temp(add)]]
    puthelp "PRIVMSG $chan :[mc:re:replace $mc_re([list :: lang 0x002]) [list %nick $nick]]"
   }; mc:re:list save_db -quiet; return 1
  }
 }; return 0
}

# v1.0
proc mc:re:anti-idle {} {
 global mc_re
 set array_name [list :: list anti-idle]
 if {!$mc_re(aidle)} {return 0}
 mc:re:list anti-idle:timer
 if {![info exists mc_re($array_name)]} {putlog "$mc_re(script):  Can not Anti-Idle, information avilable." ; return 0}

 set temp(output) $mc_re($array_name)

 # Generate random response.
 set temp(length) [llength $temp(output)]
 if {$temp(length) == "1"} {set temp(index) 0} \
 else {
  set temp(list) [list :: random $array_name]; set temp(index) [rand $temp(length)]
  if {![info exists mc_re($temp(list))]} {set mc_re($temp(list)) $temp(index)}
  while {$temp(index) == $mc_re($temp(list))} {set temp(index) [rand $temp(length)]}
 }; set temp(output) [lindex $temp(output) $temp(index)]

 foreach temp(foreach) $temp(output) {
  switch -- [string tolower [lindex [split $temp(foreach)] 0]] {
   /msg {puthelp "PRIVMSG [lindex [split $temp(foreach)] 1] :[join [lrange [split $temp(foreach)] 2 end]]"}
   /notice {puthelp "NOTICE [lindex [split $temp(foreach)] 1] :[join [lrange [split $temp(foreach)] 2 end]]"}
   /quote {puthelp [join [lrange [split $temp(foreach)] 1 end]]}
   /me {puthelp "PRIVMSG [lindex [split $temp(foreach)] 1] :\001ACTION [join [lrange [split $temp(foreach)] 2 end]]\001"}
   /action {puthelp "PRIVMSG [lindex [split $temp(foreach)] 1] :\001ACTION [join [lindex [split $temp(foreach)] 2 end]]\001"}
   default {putlog "$mc_re(script):  Can not Anti-Idle, don't know what to do with\; $temp(foreach)"}
  }
 }
}

# v1.1
proc mc:re:replace {text subs} {
 if {![catch {string map -nocase $subs $text} output]} {return $output}
 for {set i 0} {[lindex $subs $i] != ""} {incr i 2} {
  regsub -all -- {([\\\*\+\?\{\}\,\(\)\:\[\]\.\^\$\=\!\|])} [lindex $subs $i] {\\\1} temp(temp)
  regsub -all -- $temp(temp) $text [lindex $subs [expr $i+1]] text
 }; return $text
}

# v1.0
proc mc:re:strip:all {text} {
 regsub -all -- {\002} $text "" text ;#bold
 regsub -all -- {\003([0-9][0-9]?(,[0-9][0-9]?)?)?} $text "" text ;#color
 regsub -all -- {\026} $text "" text ;#reverse
 regsub -all -- {\037} $text "" text ;#underline
 return $text
}

mc:re:list load_db
mc:re:list load_lang
mc:re:list load_soundex
mc:re:list anti-idle:timer


## SVS v2.0
if {(![info exists mc_re(svs:enabled)]) ||
    (![regexp -- {^(0|1)$} $mc_re(svs:enabled)])} {set mc_re(svs:enabled) 0}
if {(![info exists mc_re(svs:check)]) ||
    ([regexp -- {[^0-9]} $mc_re(svs:check)])} {set mc_re(svs:check) 0}
if {![info exists mc_re(svs:notify)]} {set mc_re(svs:notify) $owner}
if {(![info exists mc_re(svs:stableonly)]) ||
    (![regexp -- {^(0|1)$} $mc_re(svs:stableonly)])} {set mc_re(svs:stableonly) 0}
foreach bind [binds mc:re:svs:time] {
 unbind [lindex $bind 0] [lindex $bind 1] [lindex $bind 2] [lindex $bind 4]
}
foreach command [info commands mc:re:svs:time] {rename $command ""}
if {$mc_re(svs:enabled)} {
 if {($mc_re(svs:check) > "23") || ($mc_re(svs:check) < "0")} {set mc_re(svs:check) 00}
 if {[string length $mc_re(svs:check)] == "1"} {set mc_re(svs:check) 0$mc_re(svs:check)}
 bind time - "00 $mc_re(svs:check) *" mc:re:svs:time
 proc mc:re:svs:time {{args ""}} {
  global mc_re
  if {[catch {socket -async $mc_re(svs:server) $mc_re(svs:port)} sid]} {
   putlog "SVS Error\[$mc_re(svs:script)/$mc_re(svs:version)\]:  $sid"
   return 1
  }
  fconfigure $sid -blocking 0 -buffering line
  set tout [after 60000 "mc:re:svs:interface $sid timeout"]
  fileevent $sid readable "mc:re:svs:interface $sid $tout"
  puts $sid "GET $mc_re(svs:get)?$mc_re(svs:query) HTTP/1.0\
           \nHost: $mc_re(svs:server)\n"
  flush $sid
 }
}
proc mc:re:svs:interface {sid tout} {
 global mc_re
 if {$tout == "timeout"} {
  putlog "SVS Warning\[$mc_re(svs:script)/$mc_re(svs:version)\]:  SVS Server timed out."
  close $sid
  return 0
 }; after cancel $tout
 set temp 0
 while {![eof $sid]} {
  gets $sid get
  if {$temp < "2"} {
   if {$get == ""} {incr temp; if {$temp == "1"} {set temp 2}}
   continue
  }; if {$get == ""} {continue}
  switch -- [lindex $get 0] {
   003 {
    set reply [lrange $get 1 end]
    if {[lindex $reply 0] != $mc_re(svs:script)} {
     putlog "SVS Error\[$mc_re(svs:script)/$mc_re(svs:version)\]:  SVS Server sent wrong info, got [lindex $reply 0]"
     break   
    }
    if {$mc_re(svs:stableonly)} {
     set temp [string range $mc_re(svs:version) 0 8]999
    } else {set temp [string range $mc_re(svs:version) 0 11]}
    if {[lindex $reply 1] > $temp} {
     set note [lindex $reply 5]
     regsub -- %0 $note [lindex $reply 0] note
     regsub -- %1 $note [lindex $reply 1] note
     regsub -- %2 $note [lindex $reply 2] note
     regsub -- %3 $note [lindex $reply 3] note
     regsub -- %4 $note [lindex $reply 4] note
     regsub -- %version $note $mc_re(version) note
     foreach to [split $mc_re(svs:notify) ",; "] {
      if {$to == ""} {continue}
      regsub -- %nick $note $to outnote
      if {[string match *$outnote* [set unotes [notes $to -[notes $to]]]]} {
       set temp 0
       foreach unote $unotes {
        if {$outnote == [lindex $unote 2]} {set temp 1; break}
       }; if {$temp} {continue}
      }
      switch -- [sendnote SVS $to $outnote] {
       0 {
        if {![validuser $notify]} {set x "invalid user"} else {set x "unknown error"}
        putlog "SVS Error\[$mc_re(svs:script)/$mc_re(svs:version)\]:  Trying to send note to $to, $x."
       }
       3 {putlog "SVS Error\[$mc_re(svs:script)/$mc_re(svs:version)\]:  Trying to send note to $to, notebox too full!"}
      }
     }
    }
   }
  }
 }; close $sid
}
## ^

putlog "$mc_re(script) $mc_re(version) by MC_8 loaded."

##
# MC_8's scratch pad of code.  (keeping so I don't forget how it was done)
##
#[23:09] <MC_8> .tcl set search "*hey*dude*%what* %subject" ; set words "hey dude man, sup?"
#[23:09] <Mute> Tcl: hey dude man, sup?
#[23:09] <MC_8> .tcl set i -1 ; set search [split [join $search "*"] "*"] ; set src $search ; set search "" ; foreach srch $src {if {$srch != ""} {set search "$search $srch"}} ; foreach word $words {incr i 1 ; putlog "\[$i\][lindex $search $i] ? $word" ; if {$word != [lindex $search $i]} {if {[string index [lindex $search $i] 0] == "%"} {putlog "[lindex $search $i] == $word"} {incr i -1}}}
#[23:09] <Mute> [21:11] [0]hey ? hey
#[23:09] <Mute> [21:11] [1]dude ? dude
#[23:09] <Mute> [21:11] [2]%what ? man,
#[23:09] <Mute> [21:11] %what == man,
#[23:09] <Mute> [21:11] [3]%subject ? sup?
#[23:09] <Mute> [21:11] %subject == sup?
#
#proc test {search words} {
# set i -1;
# set search [split [join $search "*"] "*"];
# set src $search;
# set search "";
# foreach srch $src {
#  if {$srch != ""} {set search "$search $srch"}
# }
# foreach word $words {
#  incr i 1
#  puts "\[$i\][lindex $search $i] ? $word"
#  if {$word != [lindex $search $i]} {
#   if {[string index [lindex $search $i] 0] == "%"} {
#    puts "[lindex $search $i] == $word"
#   } else {incr i -1}
#  }
# }
#}
#
#proc test {search words} {
# regsub -all -- {\*} $search " " temp(temp)
# set search ""
# foreach temp(foreach) [split $temp(temp)] {if {$temp(foreach) != ""} {append search " $temp(foreach)"}}
# foreach sword $search word $words {
#  puts "$sword ? $word"
#  if {[string match %* $sword]} {puts "-- $sword == $word"}
# }
#}
#
#
#proc test {search words} {
# set temp(i) -1; regsub -all -- {\*} $search " " temp(temp); set search ""
# foreach temp(foreach) [split $temp(temp)] {if {$temp(foreach) != ""} {lappend search $temp(foreach)}}
# set search [join $search]
# foreach word $words {
#  incr temp(i); set sword [lindex [split $search] $temp(i)]
#  puts "\[$temp(i)\]$sword ? $word"
#  if {[string tolower $word] != [string tolower $sword]} {
#   if {[string index $sword 0] == "%"} {puts "$sword == $word"} \
#   else {incr temp(i) -1}
#  }
# }
#}
##
