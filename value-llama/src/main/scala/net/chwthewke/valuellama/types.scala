package net.chwthewke.valuellama

import monocle._, Monocle._
import monocle.macros._
// TODO move to build
import scala.language.higherKinds
import scalaz.{ Lens => _, _ }, Scalaz._

case class GenState( seed : Seed, failures : Int )

object GenState {
  val failures : Lens[GenState, Int] = GenLens[GenState]( _.failures )
  val seed : Lens[GenState, Seed] = GenLens[GenState]( _.seed )
}

case class GenParams( size : Int, maxFailures : Int )

object GenParams {
  val size : Lens[GenParams, Int] = GenLens[GenParams]( _.size )
  val maxFailures : Lens[GenParams, Int] = GenLens[GenParams]( _.maxFailures )
}

class Result[+A]( val value : Option[A] ) extends AnyVal

object Result {
  def apply[A]( a : Option[A] ) : Result[A] = new Result( a )

  def unapply[A]( r : Result[A] ) : Option[Option[A]] = Some( r.value )

  def option[A] : Iso[Result[A], Option[A]] = Iso.apply[Result[A], Option[A]]( _.value )( new Result( _ ) )

  def value[A] : Prism[Result[A], A] = option.composePrism( monocle.std.option.some )

  implicit val ResultInstance : MonadPlus[Result] with Traverse[Result] with BindRec[Result] with IsEmpty[Result] =
    new MonadPlus[Result] with Traverse[Result] with BindRec[Result] with IsEmpty[Result] {
      override def traverseImpl[G[_], A, B]( fa : Result[A] )( f : A => G[B] )( implicit G : Applicative[G] ) : G[Result[B]] =
        fa.value.traverse( f ).map( new Result( _ ) )

      override def tailrecM[A, B]( f : A => Result[A \/ B] )( a : A ) : Result[B] =
        new Result( BindRec[Option].tailrecM( ( x : A ) => f( x ).value )( a ) )

      override def point[A]( a : => A ) : Result[A] = new Result( a.point[Option] )

      override def isEmpty[A]( fa : Result[A] ) : Boolean = fa.value.isEmpty

      override def empty[A] : Result[A] = new Result( None )

      override def plus[A]( a : Result[A], b : => Result[A] ) : Result[A] =
        new Result( a.value <+> b.value )

      override def bind[A, B]( fa : Result[A] )( f : A => Result[B] ) : Result[B] =
        new Result( fa.value.flatMap( ( x : A ) => f( x ).value ) )
    }
}
