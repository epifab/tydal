package io.epifab.yadl.domain.typesafe

abstract class DbFunction(val name: String)

abstract class DbFunction1[+T, +U](name: String) extends DbFunction(name)
abstract class DbFunction2[+T1, +T2, +U](name: String) extends DbFunction(name)
abstract class DbFunction3[+T1, +T2, +T3, +U](name: String) extends DbFunction(name)
