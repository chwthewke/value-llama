package net.chwthewke.valuellama

class GenOps[A]( val self : Gen[GenState, A] ) {
  def eval( p : GenParams ) : Result[A] =
    self.runGen( p, GenState( Seed.random, 0 ) )._2
}

