package net.janvsmachine.fpinscala

import Chapter2._
import Chapter3._

object fp {

  val lte = (x: Int, y: Int) ⇒ x >= y             //> lte  : (Int, Int) => Boolean = <function2>
  Array(1, 2, 3).sortWith(lte)                    //> res0: Array[Int] = Array(3, 2, 1)
  isSorted(Array(1, 1), lte)                      //> res1: Boolean = true

  val l = List(1, 2, 3, 4, 5, 6)                  //> l  : net.janvsmachine.fpinscala.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons
                                                  //| (5,Cons(6,Nil))))))

  // Exercise 3.2
  tail(l)                                         //> res2: net.janvsmachine.fpinscala.List[Int] = Cons(2,Cons(3,Cons(4,Cons(5,Con
                                                  //| s(6,Nil)))))

  // Exercise 3.3
  setHead(l, 5)                                   //> res3: net.janvsmachine.fpinscala.List[Int] = Cons(5,Cons(2,Cons(3,Cons(4,Con
                                                  //| s(5,Cons(6,Nil))))))

  // Exercise 3.4
  drop(l, 0)                                      //> res4: net.janvsmachine.fpinscala.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Con
                                                  //| s(5,Cons(6,Nil))))))
  drop(l, 1)                                      //> res5: net.janvsmachine.fpinscala.List[Int] = Cons(2,Cons(3,Cons(4,Cons(5,Con
                                                  //| s(6,Nil)))))
  drop(l, 10)                                     //> res6: net.janvsmachine.fpinscala.List[Int] = Nil

  // Exercise 3.5
  dropWhile(l, (x: Int) ⇒ x < 4)                  //> res7: net.janvsmachine.fpinscala.List[Int] = Cons(4,Cons(5,Cons(6,Nil)))

  // Exercise 3.6
  dropLast(l)                                     //> res8: net.janvsmachine.fpinscala.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Con
                                                  //| s(5,Nil)))))

  // Exercise 3.7
  foldRight(l, 0)(_ + _)                          //> res9: Int = 21

  // Exercise 3.9
  foldRight(l, 0)((n: Int, acc: Int) ⇒ acc + 1)   //> res10: Int = 6
  
  // Exercise 3.10
  foldLeft(l, 0)(_ + _)                           //> res11: Int = 21
 
  val strings = List("a", "b", "c")               //> strings  : net.janvsmachine.fpinscala.List[String] = Cons(a,Cons(b,Cons(c,Ni
                                                  //| l)))
  foldLeft(strings, "x")(_ + _)                   //> res12: String = xabc
  foldRight(strings, "x")(_ + _)                  //> res13: String = abcx
 
  // Exercise 3.12:
  foldLeft(l, Nil.asInstanceOf[List[Int]])((xs, x) => Cons(x, xs))
                                                  //> res14: net.janvsmachine.fpinscala.List[Int] = Cons(6,Cons(5,Cons(4,Cons(3,Co
                                                  //| ns(2,Cons(1,Nil))))))
}