package u03

import u03.Optionals.Optional.*

object Lab03 extends App :

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(fullname: String, course: String)

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])


  // Tasks - Part 1 (lists)

  object Sequence :

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper)) // ritorna una lista con head mappata e ricorsione su tail
      case Nil() => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()


    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(h, t) if n > 0 => skip(t)(n - 1) // finchè n > 0, skippa elementi scalando n
      case _ => s // stampa ciò che rimane dopo aver skippato n elementi

    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2)) // ritorna seq con head = (h1,h2) e tail = (t1,t2)
      case _ => Nil()

    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
      case Cons(h1, t1) => Cons(h1, concat(t1, s2)) // ritorna seq con head = h1 e tail = concat del resto
      case Nil() => s2 // caso base = s1 nil

    def reverse[A](s: Sequence[A]): Sequence[A] =
      def help(remaining: Sequence[A], reverse: Sequence[A]): Sequence[A] = remaining match
        case Nil() => reverse
        case Cons(h, t) =>
          val accumulator = Cons(h, reverse)
          help(t, accumulator)
      help(s, Nil())

    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper)) // ritorna concat tra head mappata e ricorsione con tail mappata
      case Nil() => Nil()


  // Tasks - Part 2 (more on lists)

  object Person :

    import Sequence.*
    import Person.*

    /*
    se Teacher, "course" è head della nuova sequenza
    se Student, head viene ignorato e avviene ricorsione su tail
     */
    def getCourses(p: Sequence[Person]): Sequence[String] = p match
      case Cons(Teacher(_, course), t) => Cons(course, getCourses(t))
      case Cons(Student(_, _), t) => getCourses(t)
      case Nil() => Nil()

    /*
     value = valore iniziale, diventa il risultato in maniera ricorsiva
     operator = funzione che ritorna il risultato tra value e head
     */
    def foldLeft[A, B](s: Sequence[A])(value: B)(operator: (B, A) => B): B = s match
      case Cons(h, t) => foldLeft(t)(operator(value, h))(operator)
      case Nil() => value

    /*
      coursesToNumber = nuova sequenza in cui ogni elemento è un corso mappato ad 1
    */
    def getCoursesNumber(p: Sequence[Person]): Int =
      val coursesToNumber = flatMap(getCourses(p))(course => Cons(1, Nil()))
      foldLeft(coursesToNumber)(0)(_ + _)


  // Tasks - Part 3 (streams)

  object Stream :


    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))


    /*
    se head rispetta il predicato, restituisce ricorsivamente uno stream applicando takeWhile su tail
     */
    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
      case _ => Empty()


    /*
    se n non si è ancora azzerato, restituisce ricorsivamente uno stream con head = k ed n--
    */
    def fill[A](n: Int)(k: A): Stream[A] = k match
      case k if n > 0 => cons(k, fill(n-1)(k))
      case _ => Empty()

    /*
    lo stream iniziale di default sarebbe Cons(0, Cons(1, Empty())),
    ma invece di farla terminare, viene chiamata ricorsivamente calcNext che calcola il resto dello stream
    sommando i precedenti due elementi
     */
    val fibonacci: Stream[Int] =
      Cons(() => 0, () => Cons(() => 1, () => {
        def calcNext(a: Int, b: Int): Stream[Int] =
          Cons(() => a + b, () => calcNext(b, a + b))
        calcNext(0, 1) }))
