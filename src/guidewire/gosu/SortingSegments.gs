uses java.util.ArrayList

var segments = typekey.ClaimSegment.getTypeKeys( false ).copy()
segments.addAll( segments )
segments.shuffle()
print(segments + "\n")

var cl : Claim

uses java.util.Set

var uniqueSegments : Set<ClaimSegment>
cl.Exposures.each( \ exposure -> uniqueSegments.add( exposure.Segment ) )

var segmentsHighestToLowest : typekey.ClaimSegment[] = { "auto_high", "auto_mid", "auto_low"  }
var max = segmentsHighestToLowest.findFirst( \ seg -> uniqueSegments.contains( seg ) )
print( "The highest found is '" +  max + "'\n")

var validations = typekey.ValidationLevel.getTypeKeys( false ).copy()
validations.addAll(validations)
validations.shuffle()

print(validations + "\n")

//max = validations.max()
print( "The highest found is '" +  validations.min() + "'\n")

var seg : typekey.ClaimSegment
//print((seg <= orderedSegs.first()) ? true : false)

var exps = new ArrayList<Exposure>().toTypedArray()
seg = exps.first().Segment
//seg = exps.maxBy( \ exp -> exp.Segment ).Segment
print(seg)

/*var lesser =  key1 < key2 ? key1 : key2
var greater = key1 > key2 ? key1 : key2

if(key1 == key2) {
  print(key1 + " equals " + key2)
}
else {
  print("'" + lesser + "' is less than '" + greater + "'")
}
print(segments)
print(sortedSegs)

var highestSegment = segments.first()
for(seg in segments) {
  if(seg == "auto_high") return seg
  if(seg != highestSegment) {
    if(seg == 
  }
}
*/
