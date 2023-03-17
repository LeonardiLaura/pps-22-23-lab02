package u02

import scala.Tuple.Filter

object task extends App:

  // task 1

  println("hello scala")

  // task 3a

  println("TASK 3 A")

  val positive: (Int) => String = (x: Int) => x match
    case x if x > 0 => "positive"
    case x if x < 0 => "negative"
    case _ => "zero"
  println(positive(0))
  def positive2(x: Int): String = x match
    case x if x > 0 =>"positive"
    case x if x < 0 => "negative"
    case _ => "zero"
  println(positive2(1))

  // task 3b

  println("TASK 3 B")

  val neg: ((String) => Boolean) => (String) => Boolean = (f1: (String) => Boolean) => !f1(_)

  val x : String => Boolean = (y: String) => y match
    case "pippo" => true
    case _ => false

  /*
  println(x("pippo"))
  val notx = neg(x)
  println(notx("pippo"))
  println(x("pippo") && !notx("pippo"))
  */

  val empty: String => Boolean = _ == "" // predicate on strings
  val notEmpty = neg(empty) // which type of notEmpty?
  println(notEmpty("foo"))  // true
  println(notEmpty(""))     // false
  println(notEmpty("foo") && !notEmpty("")) // true.. a comprehensive test

  def neg2(f: (String) => Boolean): (String) => Boolean = !f(_)


  val notEmpty2 = neg2(empty) // which type of notEmpty?
  println(notEmpty("foo"))  // true
  println(notEmpty(""))     // false
  println(notEmpty("foo") && !notEmpty("")) // true.. a comprehensive test

  // task 3c

  def neg3[X](f: X=> Boolean): X => Boolean = !f(_)

  val zero: Int => Boolean = _ == 0 // predicate on strings

  val notZero = neg3(zero)
  println("TASK 3 C")
  println(notZero(1))  // true
  println(notZero(0))     // false
  println(notZero(1) && !notZero(0)) // true

  // task 4

  println("TASK 4")

  val p1: Int => Int => Int => Boolean =
    x => y => z => x <= y && y == z
  val p2: (Int, Int, Int) => Boolean = (x: Int, y: Int,z: Int) => x <= y && y == z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z
  def p4(x: Int, y: Int,z: Int): Boolean = x <= y && y == z

  // true
  print(p1(2)(3)(3))
  print(" "+p2(2,3,3))
  print(" "+p3(2)(3)(3))
  println(" "+p4(2,3,3))

  // false
  print(p1(4)(3)(3))
  print(" "+p2(2,3,4))
  print(" "+p3(2)(1)(3))
  println(" "+p4(2,1,1))

  // task 5

  println("TASK 5")

  val compose: (Int => Int, Int => Int) => (Int) => Int = (f: (Int) => Int, g: (Int) => Int) =>
    i=>f(g(i))

  println(compose(_ - 1, _ * 2)(5)) // 9


  def compose2[X,Y,Z](f: X => Y, g: Z => X): Z => Y =
    i=>f(g(i))

  val prova: (Int , Int ) => Int = (x , y) => x - y
  val prova2: (Int , Int ) => Int = (x , y) => x * y
  println(compose2(prova(_,1), prova2(_,2))(5)) // 9

  // task 6

  println("TASK 6")

  val gcd: (Int, Int) => Int = (a: Int, b: Int) => a match
    case a if a%b == 0 => b
    case a if a > b => gcd(b, a%b )
    case _ => gcd(a, b%a )

  println(gcd(12, 8)) // 4
  println(gcd(14, 7)) // 7

  // task 7

  println("TASK 7")

  enum Shape:
    case Square(side: Double, pointDownSx: (Double, Double))
    case Rectangle(height: Double, width: Double, pointDownSx: (Double, Double))
    case Circle(radius: Double, center: (Double, Double))

  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Square(side,_) => side*4
      case Rectangle(height,width,_) => (height + width)*2
      case Circle(radius,_) => radius*2*3.14

    def contains(shape: Shape, point: (Double, Double)): Boolean = shape match
      case Square(side, pointDownSx) =>
        point._1 >= pointDownSx._1 && point._1 <= pointDownSx._1+side &&
          point._2 >= pointDownSx._2 && point._2 <= pointDownSx._2+side
      case Rectangle(height, width, pointDownSx) =>
        point._1 >= pointDownSx._1 && point._1 <= pointDownSx._1+width &&
          point._2 >= pointDownSx._2 && point._2 <= pointDownSx._2+height
      case Circle(radius, center) =>
        point._1 >= center._1-radius && point._1 <= center._1+radius &&
          point._2 >= center._2-radius && point._2 <= center._2+radius


  println(Shape.perimeter(Shape.Square(2,(0,0)))) //8
  println(Shape.perimeter(Shape.Rectangle(2,5,(0,0)))) //14
  println(Shape.contains(Shape.Square(2,(0,0)),(1,1)))  //true
  println(Shape.contains(Shape.Square(2,(0,0)),(-1,1)))  //false


  // task 8

  println("TASK 8")

  object Optionals extends App:

    enum Option[A]:
      case Some(a: A)
      case None() // here parens are needed because of genericity

    object Option:
      def isEmpty[A](opt: Option[A]): Boolean = opt match
        case None() => true
        case _ => false

      def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
        case Some(a) => a
        case _ => orElse

      def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
        case Some(a) => f(a)
        case _ => None()

      def filter[A](x: Option[A])(cond: A => Boolean): Option[A] = x match
        case Some(a) if cond(a) => x
        case _ => None()

      def map[A](x: Option[A])(cond: A => Boolean): Option[Boolean] = x match
        case None() => None()
        case Some(a) if cond(a) => Some(true)
        case _ => Some(false)

      def fold[A](x: Option[A])(default: A)(cond: A => A): A = x match
        case None() => default
        case Some(a) => cond(a)

  import Optionals.Option.*

  println("TEST FILTER")
  println(filter(Some(5))(_ > 2))  // Some(5)
  println(filter(Some(5))(_ > 8)) // None
  println(filter(Optionals.Option.None[Int]())(_ > 2)) // None
  println("TEST MAP")
  println(map(Some(5))(_ > 2)) // Some(true)
  println(map(Some(5))(_ > 8)) // Some(false)
  println(map(None[Int]())(_ > 2)) // None
  println("TEST FOLD")
  println(fold(Some(5))(1)(_ + 1)) // 6
  println(fold(None[Int]())(1)(_ + 1)) // 1


