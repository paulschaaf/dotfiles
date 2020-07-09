#!/usr/bin/env kscript

import kotlin.system.exitProcess

val AllEffects = mapOf(
        "-clear"     to 0,
        "-bold"      to 1,
        "-dim"       to 3,
        "-underline" to 4,
        "-blink"     to 5,
        "-inverse"   to 7)

val AllColors = mapOf(
        "-black"     to 30,
        "-red"       to 31,
        "-green"     to 32,
        "-yellow"    to 33,
        "-blue"      to 34,
        "-magenta"   to 35,
        "-cyan"      to 36,
        "-white"     to 37
)

val defaultArgs = arrayOf("-fg", "-bold", "-yellow")

fun showUsage(errorMessage: String? = null) {
    if (errorMessage != null) println("Error: $errorMessage")
    println("""
Usage: lite.kts [-fg|bg]? [[<COLOR>]* [<EFFECT>]* [regex]+]+

ANSI colorizes text matching a regex pattern. A group of colors can be listed together. If 
combining the colors causes a conflict, the last one wins. Regex's may overlap and are applied 
in the order specified. The arguments default to "${defaultArgs.joinToString(" ")}".

  -bg    The subsequent colors apply to the character background.
  -fg    The subsequent colors apply to the character foreground.
  -h     Show this help screen.
 
  COLOR is any of these
       ${AllColors.keys.joinToString(", ")}

  EFFECT is any of these
       ${AllEffects.keys.joinToString(", ")}

Example:
  /bin/ls /etc | egrep 'su|sh|rc' | lite.kts '^su.*' -red sh -green rc
  
  List the files in /etc using the following colors:
     -the default highlighting:     any line that begins with 'su'
     -red bold:                     the letters 'sh'
     -green background:             the letters 'so'
  
Report bugs to <paul.schaaf@gmail.com>.
""")
    exitProcess(if (errorMessage == null) 0 else 1)
}

var color = 0
val esc = 27.toChar()
val effects = mutableListOf<Int>()

fun Iterable<Any>.ansi() = this.joinToString(";").ansi()
fun Any.ansi() = "$esc[${this}m"

fun highlight(result : MatchResult): String {
    val ansiCodes = effects.plus(color + ground).ansi()
    return "${ansiCodes}${result.groupValues.first()}$clear"
}

val clear = 0.ansi()

var ground = 0
val matchers = mutableListOf<(String) -> String>()

defaultArgs.plus(args).forEach { arg ->
    when {
        arg == "-bg"                -> ground = 10
        arg == "-fg"                -> ground = 0
        arg == "-h"                 -> showUsage()
        AllEffects.containsKey(arg) -> effects.add(AllEffects[arg]!!)
        AllColors.containsKey(arg)  -> color = AllColors[arg]!!
        else                        -> {
            val regex = Regex(arg)
            val ansiCodes = effects.plus(color + ground).ansi()
//            matchers.add { str -> regex.replace(str, ::highlight) }
            matchers.add { regex.replace(it) { result -> "${ansiCodes}${result.groupValues.first()}$esc[0m" } }
        }
    }
}

generateSequence { readLine() }
        .fold("", { acc: String, line: String ->
            matchers.foldRight(line, { matcher, str -> matcher(str) })
                    .apply(::println)
        })

//generateSequence { readLine() }.forEach { line ->
//    matchers.foldRight(line, { matcher, str -> matcher(str) })
//            .apply(::println)
//}
