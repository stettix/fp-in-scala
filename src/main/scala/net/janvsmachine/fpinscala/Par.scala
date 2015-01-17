package net.janvsmachine.fpinscala

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit

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

  def fork[A](a: ⇒ Par[A]): Par[A] = ???

  def run[A](s: ExecutorService)(a: Par[A]): A = ???

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) ⇒ C) =
    (es: ExecutorService) ⇒ {
      val fa = a(es)
      val fb = b(es)
      UnitFuture(f(fa.get, fb.get))
    }

  def asyncF[A, B](f: A ⇒ B): A ⇒ Par[B] = a ⇒ lazyUnit(f(a))

}
