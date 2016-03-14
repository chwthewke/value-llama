package net.chwthewke.valuellama

import monocle._, Monocle._
import scala.collection.generic.CanBuildFrom
import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.{ Lens => _, _ }, Scalaz._

class Gen[S, +A]( val runGen : ( GenParams, S ) => ( S, GenFailure \/ A ) ) extends AnyVal

trait GenInstances {

  implicit def GenInstance[S] : Monad[Gen[S, ?]] with BindRec[Gen[S, ?]] =
    new Monad[Gen[S, ?]] with BindRec[Gen[S, ?]] {
      // TODO stack-safety?
      override def bind[A, B]( fa : Gen[S, A] )( f : A => Gen[S, B] ) : Gen[S, B] =
        Gen( ( p, s ) => {
          val ( s1, r1 ) = fa.runGen( p, s )
          r1.fold( e => ( s1, -\/( e ) ), a => f( a ).runGen( p, s1 ) )
        } )

      override def point[A]( a : => A ) : Gen[S, A] = Gen( ( p, s ) => ( s, \/-( a ) ) )

      override def tailrecM[A, B]( f : A => Gen[S, A \/ B] )( a : A ) : Gen[S, B] = Gen { ( p, s ) =>
        @tailrec
        def go( s : S, a : A ) : ( S, GenFailure \/ B ) =
          f( a ).runGen( p, s ) match {
            case ( s1, -\/( err ) )       => ( s1, -\/( err ) )
            case ( s1, \/-( \/-( b ) ) )  => ( s1, \/-( b ) )
            case ( s1, \/-( -\/( a1 ) ) ) => go( s1, a1 )
          }

        go( s, a )
      }
    }

}

trait GenFunctions {

  def long : Gen[GenState, Long] = zoom( GenState.seed )( stateGen( _.long ) )

  def long( a : Long, b : Long ) : Gen[GenState, Long] = zoom( GenState.seed )( stateGen( _.long( a, b ) ) )

  def int : Gen[GenState, Int] = long.map( _.toInt )

  def int( a : Int, b : Int ) : Gen[GenState, Int] = long( a, b ).map( _.toInt )

  def double : Gen[GenState, Double] = zoom( GenState.seed )( stateGen( _.double ) )

  private def withParams[S, A]( g : GenParams => Gen[S, A] ) : Gen[S, A] =
    Gen[S, GenParams]( ( p, s ) => ( s, \/-( p ) ) ).flatMap( g )

  def sized[S, A]( g : Int => Gen[S, A] ) : Gen[S, A] =
    withParams( g compose ( _.size ) )

  private def modifyParams[S, A]( f : GenParams => GenParams, g : Gen[S, A] ) : Gen[S, A] =
    Gen( ( p, s ) => g.runGen( f( p ), s ) )

  def resized[S, A]( s : Int, g : Gen[S, A] ) : Gen[S, A] = modifyParams( GenParams.size.set( s ), g )

  def retried[S, A]( n : Int, g : Gen[S, A] ) : Gen[S, A] = modifyParams( GenParams.maxFailures.set( n ), g )

  def stateGen[S, A]( st : S => ( S, A ) ) : Gen[S, A] = Gen( ( p, s ) => st( s ).map( \/-( _ ) ) )

  def withState[S, A]( g : S => Gen[S, A] ) : Gen[S, A] = Gen[S, S]( ( p, s ) => ( s, \/-( s ) ) ).flatMap( g )

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

  // NOTE actually stack-safe \o/
  def containerN[C[_], A]( size : Int, g : Gen[GenState, A] )(
    implicit b : Buildable[A, C[A]] ) : Gen[GenState, C[A]] = {

    import scala.collection.mutable.Builder

    def stepC( rem : Int, b : Builder[A, C[A]] ) : Gen[GenState, ( Int, Builder[A, C[A]] ) \/ C[A]] = {
      if ( rem == 0 ) \/-( b.result ).pure[Gen[GenState, ?]]
      else g.map( a => -\/( ( rem - 1, b += a ) ) )
    }

    BindRec[Gen[GenState, ?]].tailrecM( ( stepC _ ).tupled )( ( size, b.builder ) )
  }

  def container[C[_], A]( g : Gen[GenState, A] )(
    implicit b : Buildable[A, C[A]] ) : Gen[GenState, C[A]] =
    sized( n => int( 0, n ).flatMap( containerN[C, A]( _, g ) ) )

  def container1[C[_], A]( g : Gen[GenState, A] )(
    implicit b : Buildable[A, C[A]] ) : Gen[GenState, C[A]] =
    sized( n => int( 1, n max 1 ).flatMap( containerN[C, A]( _, g ) ) )
}

object Gen extends GenInstances with GenFunctions {

  implicit class GenOps[A]( val self : Gen[GenState, A] ) {
    def eval( implicit p : GenParams ) : GenFailure \/ A =
      self.runGen( p, GenState( Seed.random, 0 ) )._2
  }

  def apply[S, A]( r : ( GenParams, S ) => ( S, GenFailure \/ A ) ) = new Gen( r )
}
