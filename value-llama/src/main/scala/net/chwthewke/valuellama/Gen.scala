package net.chwthewke.valuellama

import monocle._, Monocle._
import scala.collection.generic.CanBuildFrom
import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.{ Lens => _, _ }, Scalaz._
import scalaz.syntax.isEmpty.ToIsEmptyOps

import MonadStateSyntax._

class Gen[S, +A]( val runGen : ( GenParams, S ) => ( S, Result[A] ) ) extends AnyVal

trait GenFunctions0 {

  def constResult[S, A]( ra : => Result[A] ) : Gen[S, A] =
    Gen( ( _, s ) => ( s, ra ) )

}

trait GenInstances extends GenFunctions0 {

  implicit def GenInstance[S] : MonadState[Gen[S, ?], S] with MonadPlus[Gen[S, ?]] with BindRec[Gen[S, ?]] =
    new MonadState[Gen[S, ?], S] with MonadPlus[Gen[S, ?]] with BindRec[Gen[S, ?]] {
      override def init : Gen[S, S] = Gen( ( p, s ) => ( s, s.point[Result] ) )

      override def get : Gen[S, S] = init

      override def put( s : S ) : Gen[S, Unit] = Gen( ( p, _ ) => ( s, ().point[Result] ) )

      override def modify( f : S => S ) : Gen[S, Unit] = Gen( ( p, s ) => ( f( s ), ().point[Result] ) )

      // TODO stack-safety
      override def bind[A, B]( fa : Gen[S, A] )( f : A => Gen[S, B] ) : Gen[S, B] =
        Gen( ( p, s ) => {
          val ( s1, r1 ) = fa.runGen( p, s )
          r1.value.cata( a => f( a ).runGen( p, s1 ), ( s1, mempty[Result, B] ) )
        } )

      override def empty[A] : Gen[S, A] = constResult( mempty[Result, A] )

      override def plus[A]( a : Gen[S, A], b : => Gen[S, A] ) : Gen[S, A] =
        Gen( ( p, s ) => {
          val ( s1, r1 ) = a.runGen( p, s )
          if ( r1.isEmpty ) b.runGen( p, s1 ) else ( s1, r1 )
        } )

      override def point[A]( a : => A ) : Gen[S, A] = constResult( a.point[Result] )

      override def tailrecM[A, B]( f : A => Gen[S, A \/ B] )( a : A ) : Gen[S, B] = Gen { ( p, s ) =>
        @tailrec
        def go( s : S, a : A ) : ( S, Result[B] ) =
          f( a ).runGen( p, s ) match {
            case ( s, Result( None ) )             => ( s, Result( None ) )
            case ( s, Result( Some( \/-( b ) ) ) ) => ( s, Result( Some( b ) ) )
            case ( s, Result( Some( -\/( a ) ) ) ) => go( s, a )
          }

        go( s, a )
      }
    }

}

trait GenFunctions {

  def long : Gen[GenState, Long] = zoom( GenState.seed )( stateGen( _.long ) )
  def long( a : Long, b : Long ) : Gen[GenState, Long] = zoom( GenState.seed )( stateGen( _.long( a, b ) ) )
  def double : Gen[GenState, Double] = zoom( GenState.seed )( stateGen( _.double ) )

  // TODO unapply?
  def stateGen[S, A]( st : S => ( S, A ) ) : Gen[S, A] = stateM[Gen[S, ?], S, A]( st )

  def withState[S, A]( g : S => Gen[S, A] ) : Gen[S, A] = MonadState[Gen[S, ?], S].get.flatMap( g )

  def xmapState[S, T, A]( g : Gen[S, A] )( b : T => S, f : S => T ) : Gen[T, A] =
    Gen { ( p, t ) =>
      val ( s, ra ) = g.runGen( p, b( t ) )
      ( f( s ), ra )
    }

  def zoom[S, T, A]( l : Lens[T, S] )( g : Gen[S, A] ) : Gen[T, A] =
    withState( t => xmapState( g )( l.get, l.set( _ )( t ) ) )

  def retry[A]( g : Gen[GenState, A] ) : Gen[GenState, A] = ???

  trait Buildable[E, T] {
    def builder : scala.collection.mutable.Builder[E, T]
  }

  object Buildable {
    implicit def buildableCanBuildFrom[F, E, T]( implicit cbf : CanBuildFrom[F, E, T] ) : Buildable[E, T] =
      new Buildable[E, T] { override def builder = cbf.apply }
  }

  // TODO not convinced this is stack-safe
  private def container[C[_], A]( size : Int, g : Gen[GenState, A] )(
    implicit b : Buildable[A, C[A]] ) : Gen[GenState, C[A]] = {

    import scala.collection.mutable.Builder

    def stepC( rem : Int, b : Builder[A, C[A]] ) : Gen[GenState, ( Int, Builder[A, C[A]] ) \/ C[A]] = {
      if ( rem == 0 ) \/-( b.result ).pure[Gen[GenState, ?]]
      else g.map( a => -\/( ( rem - 1, b += a ) ) )
    }

    BindRec[Gen[GenState, ?]].tailrecM( ( stepC _ ).tupled )( ( size, b.builder ) )
  }
}

object Gen extends GenInstances with GenFunctions {
  implicit class GenOps[A]( val self : Gen[GenState, A] ) {
    def eval( p : GenParams ) : Result[A] =
      self.runGen( p, GenState( Seed.random, 0 ) )._2
  }

  def apply[S, A]( r : ( GenParams, S ) => ( S, Result[A] ) ) = new Gen( r )
}
