
package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RaizNewton extends AnyFunSuite{

  test("testRaizNewton1") {
    val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
    val res1 = Newton.raizNewton(e1, Atomo('x'), 2.0, Newton.buenaAprox)
    assert(scala.math.abs(res1 - 2.0) < 0.001 || scala.math.abs(res1 + 2.0) < 0.001)
  }


  test("testRaizNewton2") {
    val e2 = Resta(Prod(Prod(Atomo('x'), Atomo('x')), Atomo('x')), Numero(8.0))
    val res2 = Newton.raizNewton(e2, Atomo('x'), 2.0, Newton.buenaAprox)
    assert(scala.math.abs(res2 - 2.0) < 0.001)
  }


  test("testRaizNewton3") {
    val e3 = Suma(Atomo('x'), Numero(-5.0))
    val res3 = Newton.raizNewton(e3, Atomo('x'), 0.0, Newton.buenaAprox)
    assert(scala.math.abs(res3 - 5.0) < 0.001)
  }


  test("testRaizNewton4") {
    val e4 = Resta(Logaritmo(Atomo('x')), Numero(1.0))
    val res4 = Newton.raizNewton(e4, Atomo('x'), 3.0, Newton.buenaAprox)
    assert(scala.math.abs(res4 - scala.math.exp(1.0)) < 0.001)
  }



  test("testRaizNewton5") {
    val e5 = Resta(Expo(Numero(scala.math.E), Atomo('x')), Numero(1.0))
    val res5 = Newton.raizNewton(e5, Atomo('x'), 0.0, Newton.buenaAprox)
    assert(scala.math.abs(res5 - 0.0) < 0.001)
  }


  test("testRaizNewtonPar1") {
    val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
    val res1 = NewtonPar.raizNewton(e1, Atomo('x'), 2.0, NewtonPar.buenaAproxi)
    assert(scala.math.abs(res1 - 2.0) < 0.001 || scala.math.abs(res1 + 2.0) < 0.001)
  }


  test("testRaizNewtonPar2") {
    val e2 = Resta(Prod(Prod(Atomo('x'), Atomo('x')), Atomo('x')), Numero(8.0))
    val res2 = NewtonPar.raizNewton(e2, Atomo('x'), 2.0, NewtonPar.buenaAproxi)
    assert(scala.math.abs(res2 - 2.0) < 0.001)
  }


  test("testRaizNewtonPar3") {
    val e3 = Suma(Atomo('x'), Numero(-5.0))
    val res3 = NewtonPar.raizNewton(e3, Atomo('x'), 0.0, NewtonPar.buenaAproxi)
    assert(scala.math.abs(res3 - 5.0) < 0.001)
  }


  test("testRaizNewtonPar4") {
    val e4 = Resta(Logaritmo(Atomo('x')), Numero(1.0))
    val res4 = NewtonPar.raizNewton(e4, Atomo('x'), 3.0, NewtonPar.buenaAproxi)
    assert(scala.math.abs(res4 - scala.math.exp(1.0)) < 0.001)
  }


  test("testRaizNewtonPar5") {
    val e5 = Resta(Expo(Numero(scala.math.E), Atomo('x')), Numero(1.0))
    val res5 = NewtonPar.raizNewton(e5, Atomo('x'), 0.0, NewtonPar.buenaAproxi)
    assert(scala.math.abs(res5 - 0.0) < 0.001)
  }







}
