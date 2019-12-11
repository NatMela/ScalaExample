import scala.annotation.tailrec

class ZerosAndOnes {
  val characterToChange = 'x'
  val legalCharacter1 = "0"
  val legalCharacter2 = "1"

  def generateAllCombinations(input: String, listOfBaseLists: List[List[String]]): List[String] = {
    @tailrec
    def generateInOneRec(sPairs: List[(List[String], String)], accum: List[String]): List[String] = {
      sPairs match {
        case Nil => accum
        case head :: tail => {
          val constPart: String = head._2
          val digits: List[String] = head._1

          val newAccum: List[String] = accum.flatMap(prevString => {
            val preparedString = prevString + constPart
            digits.map(preparedString + _)
          })
          generateInOneRec(tail, newAccum)
        }
      }
    }

    val inputSplitted: Array[String] = ("_" + input + "_").split(characterToChange)
    val digits: List[List[String]] = (listOfBaseLists :+ List(""))
    val sPairs = digits zip inputSplitted

    generateInOneRec(sPairs, List(""))
  }

  def expand(input: String): List[String] = {
    if (input.filter(x => x != legalCharacter1.head).filter(_ != legalCharacter2.head).filter(_ != characterToChange) != "") {
      throw new IllegalArgumentException("The input string contains illegal characters")
    }
    val numberOfXCharactersInString = input.count(_ == characterToChange)
    val baseList = List(legalCharacter1, legalCharacter2)
    val listOfBaseLists = List.fill(numberOfXCharactersInString)(baseList)
    val result = generateAllCombinations(input, listOfBaseLists)
    result.map(_.drop(1).dropRight(1))
  }
}

object ZerosAndOnes extends App {
  val zerosAndOnes = new ZerosAndOnes()
  if (args.length == 0) {
    println(" At least one parameter is needed")
    System.exit(1)
  }
  val input = args(0)
  try {
    val result = zerosAndOnes.expand(input)
    result.foreach(println(_))
  } catch {
    case e: IllegalArgumentException => println(e.getMessage);
  }
}