package net.janvsmachine.fpinscala

import List._

object States {

  type State[S, +A] = S ⇒ (A, S)

  def unit[A, S](a: A): State[S, A] = s ⇒ (a, s)

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    s ⇒
      foldLeft(fs, (List[A](), s))((acc: (List[A], S), r: State[S, A]) ⇒ {
        val (nextVal, s2) = r(s)
        (Cons(nextVal, acc._1), s2)
      })

  def flatMap[S, A, B](st: State[S, A])(g: A ⇒ State[S, B]): State[S, B] =
    s ⇒ {
      val (a, s2) = st(s)
      val st2 = g(a)
      st2(s)
    }

  def map[S, A, B](s: State[S, A])(f: A ⇒ B): State[S, B] = flatMap(s)(a ⇒ unit(f(a)))

  def map2[S, A, B, C](stateA: State[S, A], stateB: State[S, B])(f: (A, B) ⇒ C): State[S, C] =
    flatMap(stateA)(a ⇒ flatMap(stateB)(b ⇒ unit(f(a, b))))

}
