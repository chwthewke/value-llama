package net.chwthewke.valuellama

import monocle._, Monocle._
import scalaz.{ Lens => _, _ }, Scalaz._
import scalaz.syntax.isEmpty.ToIsEmptyOps

import MonadStateSyntax._

class Gen[S, A]( val runGen : ( GenParams, S ) => ( S, Result[A] ) ) extends AnyVal

trait GenFunctions0 {

  def constResult[S, A]( ra : => Result[A] ) : Gen[S, A] =
    Gen( ( _, s ) => ( s, ra ) )

}

trait GenInstances extends GenFunctions0 {

  implicit def GenInstance[S] : MonadState[Gen[S, ?], S] with MonadPlus[Gen[S, ?]] =
    new MonadState[Gen[S, ?], S] with MonadPlus[Gen[S, ?]] {
      override def init : Gen[S, S] = Gen( ( p, s ) => ( s, s.point[Result] ) )

      override def get : Gen[S, S] = init

      override def put( s : S ) : Gen[S, Unit] = Gen( ( p, _ ) => ( s, ().point[Result] ) )

      override def modify( f : S => S ) : Gen[S, Unit] = Gen( ( p, s ) => ( f( s ), ().point[Result] ) )

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
    }

}

trait GenFunctions {
  // TODO unapply?
  def stateGen[S, A]( st : S => ( S, A ) ) : Gen[S, A] = stateM[Gen[S, ?], S, A]( st )

  def zoom[S, T, A]( l : Lens[S, T] )( g : Gen[T, A] ) : Gen[S, A] =
    Gen { ( p, s ) =>
      val ( t1, a ) = g.runGen( p, l.get( s ) )
      ( l.set( t1 )( s ), a )
    }

  def long : Gen[GenState, Long] = zoom( GenState.seed )( stateGen( _.long ) )
  def double : Gen[GenState, Double] = zoom( GenState.seed )( stateGen( _.double ) )

}

object Gen extends GenInstances with GenFunctions {
  def apply[S, A]( r : ( GenParams, S ) => ( S, Result[A] ) ) = new Gen( r )
}
