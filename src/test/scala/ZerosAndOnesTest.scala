import org.scalatest._

class ZerosAndOnesTest extends FunSpec {
  describe("ZerosAndOnes") {
    val zerosAndOnes = new ZerosAndOnes()

    it("should transform string with one 'x' to a List of two strings") {
      val testValue = "0x1"
      val expectedValue = List("001", "011")

      assert(zerosAndOnes.expand(testValue) == expectedValue)
    }

    it("should remain string the same") {
      val testValue = "0110101"
      val expectedValue = List("0110101")

      assert(zerosAndOnes.expand(testValue) == expectedValue)
    }

    it("should transform string with 'x' in the beginning and in the end to a List of four strings") {
      val testValue = "x000x"
      val expectedValue = List("00000", "00001", "10000", "10001")

      assert(zerosAndOnes.expand(testValue) == expectedValue)
    }

    it("should return empty string if the input string is empty") {
      val testValue = ""
      val expectedValue = List("")

      assert(zerosAndOnes.expand(testValue) == expectedValue)
    }

    it("should throw IllegalArgumentException if the input string contains illegal characters") {
      val testValue = "01x0112x0"
      intercept[IllegalArgumentException] {
        zerosAndOnes.expand(testValue)
      }
    }

    it("should generate list of length 2^20 when we need to change 20 characters in the input string") {
      val testValue = "x" * 21
      val expectedValue = BigInt(2).pow(21).toInt

      assert(zerosAndOnes.expand(testValue).length == expectedValue)
    }
  }
}