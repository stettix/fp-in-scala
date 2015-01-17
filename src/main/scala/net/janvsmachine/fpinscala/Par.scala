package net.janvsmachine.fpinscala

import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import List._
import Option._

case class UnitFuture[A](a: A) extends Future[A] {
  def get(timeout: Long, unit: TimeUnit): A = a
  def get(): A = a
  def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  def isCancelled(): Boolean = false
  def isDone(): Boolean = true
}

object Par {

  type Par[A] = ExecutorService ⇒ Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) ⇒ UnitFuture(a)

  def lazyUnit[A](a: ⇒ A): Par[A] = fork(unit(a))

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) ⇒ C): Par[C] =
    (es: ExecutorService) ⇒ {
      val fa = pa(es)
      val fb = pb(es)
      UnitFuture(f(fa.get, fb.get))
    }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    foldRight(ps, unit(List[A]()))((acc, p) ⇒ map2(acc, p)((x, xs) ⇒ Cons(x, xs)))

  def parMap[A, B](ps: List[A])(f: A ⇒ B): Par[List[B]] = fork {
    val fbs = List.map(ps)(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](ps: List[A])(f: A ⇒ Boolean): Par[List[A]] = fork {
    val fas = List.map(ps)(asyncF(a ⇒ if (f(a)) Some(a) else None))
    val asf = sequence(fas)
    map(asf)(l ⇒ Option.sequence(l).getOrElse(Nil))
  }

  def asyncF[A, B](f: A ⇒ B): A ⇒ Par[B] =
    a ⇒ lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A ⇒ B): Par[B] =
    map2(pa, unit(()))((a, _) ⇒ f(a))

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) ⇒ D): Par[D] =
    (es: ExecutorService) ⇒ {
      val fa = pa(es)
      val fb = pb(es)
      val fc = pc(es)
      UnitFuture(f(fa.get, fb.get, fc.get))
    }

  // Could this be written in terms of map2?
  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) ⇒ E): Par[E] =
    (es: ExecutorService) ⇒ {
      val fa = pa(es)
      val fb = pb(es)
      val fc = pc(es)
      val fd = pd(es)
      UnitFuture(f(fa.get, fb.get, fc.get, fd.get))
    }

  def fork[A](a: ⇒ Par[A]): Par[A] =
    es ⇒ es.submit(new Callable[A] {
      def call = a(es).get
    })

  def merge[A](as: IndexedSeq[A], z: A)(f: (A, A) ⇒ A): Par[A] =
    if (as.length <= 1)
      unit(as.headOption.getOrElse(z))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      val pl = fork(merge(l, z)(f))
      val pr = fork(merge(r, z)(f))
      map2(pl, pr)(f)
    }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

}
