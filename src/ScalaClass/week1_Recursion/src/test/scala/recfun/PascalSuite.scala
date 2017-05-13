package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("col=0,row=-1") { assert(pascal(0,-1) === 0) }
  test("col=0,row=0")  { assert(pascal(0,0)  === 1) }
  test("col=0,row=1")  { assert(pascal(0,1)  === 1) }
  test("col=0,row=7")  { assert(pascal(0,2)  === 1) }
  test("col=1,row=2")  { assert(pascal(1,2)  === 2) }
  test("col=1,row=3")  { assert(pascal(1,3)  === 3) }
  test("col=4,row=3")  { assert(pascal(4,3)  === 0) }
}
