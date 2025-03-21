package u03

import org.junit.Test
import u03.Lab03.*
import Person.*
import Stream.*
import org.junit.Assert.*

class Lab03Test :
  import u03.Lab03.*
  import Sequence.*

  @Test def testGetCourses() =
    val teacher1 = Teacher("Diego", "Math")
    val student = Student("Daniel", 2001)
    val teacher2 = Teacher("Ivan", "History")
    val seq = Cons(teacher1, Cons(student, Cons(teacher2, Nil())))
    val expected = Cons("Math", Cons("History", Nil()))
    assertEquals(expected, getCourses(seq))

  @Test def testFoldLeft() =
    val s = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(s)(0)(_ - _))
    assertEquals(16, foldLeft(s)(0)(_ + _))

  @Test def testGetCoursesNumber() =
    val teacher1 = Teacher("Diego", "Math")
    val student = Student("Daniel", 2001)
    val teacher2 = Teacher("Ivan", "History")
    val seq = Cons(teacher1, Cons(student, Cons(teacher2, Nil())))
    assertEquals(2, getCoursesNumber(seq))

  @Test def testTakeWhile() =
    val stream = Stream.iterate(0)(_ + 1)
    val expected = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil())))))
    assertEquals(expected, Stream.toList(Stream.takeWhile(stream)(_ < 5)))

  @Test def testFill() =
    val expected = Cons("a", Cons("a", Cons("a", Cons("a", Nil()))))
    assertEquals(expected, Stream.toList(Stream.fill(4)("a")))

  @Test def testFibonacci() =
    val expected = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil())))))
    assertEquals(expected, Stream.toList(Stream.take(fibonacci)(5)))