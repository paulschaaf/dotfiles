#!/usr/bin/env kscript

import kotlin.system.exitProcess

val AllEffects = mapOf(
        "-clear" to 0,
        "-bold" to 1,
        "-dim" to 2,
        "-italic" to 3,
        "-underline" to 4,
        "-blink" to 5,
        "-fast-blink" to 6,
        "-inverse" to 7,
        "-concealed" to 8,
        "-strikeout" to 9
)

val AllColors = mapOf(
        "-black" to 30,
        "-red" to 31,
        "-green" to 32,
        "-yellow" to 33,
        "-blue" to 34,
        "-magenta" to 35,
        "-cyan" to 36,
        "-white" to 37
)

val defaultArgs = arrayOf("-fg", "-bold", "-yellow")

fun showUsage(errorMessage: String? = null) {
    if (errorMessage != null) println("Error: $errorMessage")
    println("""
Usage: lite.kts [-fg|bg]? [[<COLOR>]* [<EFFECT>]* [regex]+]+

ANSI colorizes text matching a regex pattern. A group of colors can be listed together. If 
combining the colors causes a conflict, the last one wins. Regex's may overlap and are applied 
in the order specified. The default highlighting is "${defaultArgs.joinToString(" ")}".

  -bg    The subsequent colors apply to the character background.
  -fg    The subsequent colors apply to the character foreground.
  -h     Show this help screen.
 
  COLOR is any of these
       ${AllColors.keys.joinToString(", ")}

  EFFECT is any of these
       ${AllEffects.keys.joinToString(", ")}

Example:
  /bin/ls /etc | grep -E 'su|sh|rc' | lite.kts '^su.*' -red sh -green do
  
  List some of the files in /etc using the following colors:
     -the default highlighting:     any line that begins with 'su'
     -red bold:                     the letters 'sh'
     -green background:             the letters 'do'
  
Report bugs to <paul.schaaf@gmail.com>.
""")
    exitProcess(if (errorMessage == null) 0 else 1)
}

val esc = 27.toChar()

fun Any.ansi() = "$esc[${this}m"
fun Iterable<Any>.ansi() = this.joinToString(";").ansi()

fun highlight(string: String, colors: List<Int>): String = "${colors.ansi()}${string}${0.ansi()}"

fun highlighter(regex: Regex, colors: List<Int>): (String) -> String =
        { regex.replace(it) { matchResult -> highlight(matchResult.groupValues.first(), colors) } }

val transformers = mutableListOf<(String) -> String>()

var color = 0
var ground = 0
val effects = mutableListOf<Int>()

defaultArgs
        .plus(args)
        .forEach {
            when {
                it == "-bg"                -> ground = 10
                it == "-fg"                -> ground = 0
                it == "-h"                 -> showUsage()
                it == "--help"             -> showUsage()
                AllEffects.containsKey(it) -> effects += AllEffects[it]!!
                AllColors.containsKey(it)  -> color = AllColors[it]!!
                else                       -> transformers += highlighter(Regex(it), effects.plus(ground + color))
            }
        }

generateSequence(::readLine).forEach { line ->
    transformers
        .fold(line) { str, transform -> transform(str) }
        .apply(::println)
}
