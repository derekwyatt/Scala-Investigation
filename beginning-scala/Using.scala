
def using[A <: { def close(): Unit }, B](param: A)(f: A => B): B = {
  try {
    f(param)
  } finally {
    param.close()
  }
}

import scala.collection.mutable.ListBuffer

def bmap[T](test: => Boolean)(block: => T): List[T] = {
  val ret = new ListBuffer[T]
  while (test) ret += block
  ret.toList
}

// -----------------------------------------------------------------
// Fake DB support code

// This class is /really/ silly.  There are many better functional ways we could
// do this but I wanted to keep List and Map constructs out of this in order to
// make it /very/ clear that the bmap() code is really doing something
class FakeDBResultSet {
  var count = 0
  var result = ("", 0, false)
  def next: Boolean = {
    count match {
      case 0 => result = ("John Hanemaayer", 50, true)
      case 1 => result = ("David Raeside", 40, false)
      case 2 => result = ("Timothy Lofft", 40, true)
      case 3 => result = ("Curt Beattie", 30, true)
      case 4 => result = ("Tom Duffney", 50, false)
      case _ => return false
    }
    println("ResultSet set to result " + count)
    count += 1
    return true
  }

  def close(): Unit = println("Closing DB ResultSet")
}

class FakeDBStatement {
  def close(): Unit = println("Closing DB Statement")
  def executeQuery = new FakeDBResultSet
}

class FakeDBConnection {
  def close(): Unit = println("Closing DB Connection")
  def createStatement = new FakeDBStatement
}

// -----------------------------------------------------------------

case class Person(name: String, age: Int, valid: Boolean)

object Person {
  def findPeople(conn: FakeDBConnection): List[Person] = {
    using(conn.createStatement) {
      statement => {
        using(statement.executeQuery) { resultSet =>
          bmap(resultSet.next) {
            resultSet.result match {
              case (n: String, a: Int, v: Boolean) => new Person(n, a, v)
            }
          }
        }
      }
    }
  } 
}

Person.findPeople(new FakeDBConnection).foreach(println(_))
