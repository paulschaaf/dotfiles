uses java.util.ArrayList
//uses java.util.HashMap

//var strings = new ArrayList<String>() {"one", "two", "three"}

//var nums = new ArrayList() {1, 2, 3}
//var sum = nums.reduce( 0, \ sum, item : Number -> sum + item )
//print("The sum is " + sum)
/*var warning () : String
warning = \ -> {
  warning = \ -> return null
  return "hello"
}
*/
/*print(typeof warning())
print(typeof warning())
print("warning is null: " + (warning()==null))
var bl = \ a : Number -> 4 + a
*/
//var numbersToNames = new HashMap<int,String>(){1->"one",2->"two",3->"three"}
//var numberNames = new ArrayList<String>(){"First", "Second", "Third", "Fourth"}
//
//var n_times = \ multiplier : Number -> (\ num : Number -> num * multiplier)
//
//var upperCaseOf : Block(String) : String
//upperCaseOf = \ str : String -> str.toUpperCase()
//
////var summation = \ coll : ArrayList<Number> -> coll.reduce(0, \ q : Number, d -> q + d)
////print(summation(nums))
//
//
//sum = 0
//for (item in nums) {
//  sum = sum + item
//}
//
//
//
//var longest = strings.reduce( "", \ longest : String, s -> longest.length > s.length ? longest : s )
//
//print("The longest is: '" + longest + "'")
//print(nums.reduce( 0.0, \ q : Number, d : Number -> q + d)  )

/*
var getFirstFrom = \ aList : List<?> -> aList[0]

print(getFirstFrom(strings))
print(getFirstFrom(nums))

// Declare it
print( upperCaseOf("This is a test") )

//////////////////////////////////////////*/
//function multiplyArgumentBy(aNumber : Number) : Block(Number) : Number {
//  return \ argument : Number -> aNumber * argument
//}
//
//var triple = multiplyArgumentBy(3)
//var quadruple = multiplyArgumentBy(4)
//var inchesToCm = multiplyArgumentBy(2.54)
//
//print(triple(10))
//print(quadruple(10))
//print(inchesToCm(10))

/*var multiplier = 5
var useMuliplier = \ numParam : Number -> multiplier * numParam 
print(useMuliplier(10))
multiplier = 6
print(useMuliplier(10))
*/




/*
numberNames[4] = "four"
numberNames.eachKeyAndValue( \ i, s -> print("Key: "+i+", value: "+s) )
*/
//numbers.each( \ s -> print(s) )
////numbers.eachWithIndex( \ s, i -> print("Item #" + i + " is " + s) )
//numbers.sortBy( \ s -> s )
//numbers.each( \ s -> print(s) )
//flatMap( \ s -> s.length )

/*
var list = new ArrayList()
list.add("1")
list.add(new Address() )
(list.get(1) as Address).AddressLine1 = "foo bar"
*/
/*
var add = \ num1 : Number, num2 : Number -> num1 + num2
var addBlock = \ num1 : Number, num2 : Number -> num1 + num2
function addFunction(num1 : Number, num2 : Number) : Number {
  return num1 + num2
}
*/
var strs = new String[5]
strs[0] = "hi"
strs[1] = "there"
strs[2] = "how"
strs[3] = "are"
strs[4] = "you"

strs.each( \ s -> print(s) )
print("")
strs.sortBy( \ s -> s )
strs.each( \ s -> print(s) )

var foo : ABContact
if(foo.UpdateTime <= gw.api.util.DateUtil.currentDate()) print("yes")
