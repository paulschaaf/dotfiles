/*** BEGIN Logging Code *********************************************************/
final var _logDateFormat = new java.text.SimpleDateFormat("HH:mm:ss |  ")
print("\n##########  " + java.util.Date.CurrentDate)  // show log header for this run

function log(str:       String ) { log({str}) }
function log(strs: List<String>) {
  var currentDateStr: String
  for (str in strs) {
    currentDateStr = _logDateFormat.format(java.util.Date.CurrentDate)
    print(currentDateStr + str)
  }
}

function timeBlock(repeat: int, action: (i: int)): int {
  return timeBlock(repeat, "block", action)
}
function timeBlock(repeat: int, name: String, action: (i: int)): int {
  log("Beginning " + repeat + " iterations of " + name + ".")
  var start = java.util.Date.Now
  for (iteration in 1..repeat) action(iteration)
  var elapsed = start.SecondsSince
  log(repeat + " iterations of " + name + " performed in " + elapsed + " seconds.")
  return elapsed
}
/*** END Logging Code ***********************************************************/
