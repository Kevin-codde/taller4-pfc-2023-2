package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Evaluacion extends AnyFunSuite {

    test("testTaller4") {
        assert("Taller 4" == Taller4.saludo())
    }

     // Tests for evaluacion function
    test("evaluacion caso 1") {
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        assert(35.0 == Newton.evaluar(expresion, Atomo('x'), 5.0))
    }

    test("evaluacion caso 2") {
        val expresion = Resta(Numero(20.0), Div(Atomo('y'), Numero(4.0)))
        assert(15.0 == Newton.evaluar(expresion, Atomo('y'), 20.0))
    }

    test("evaluacion caso 3") {
        val expresion = Prod(Atomo('a'), Resta(Numero(15.0), Atomo('b')))
        assert(15.0 == Newton.evaluar(expresion, Atomo('a'), 1.0))
    }

    test("evaluacion caso 4") {
        val expresion = Div(Atomo('x'), Suma(Numero(7.0), Atomo('y')))
        assert(1 == Newton.evaluar(expresion, Atomo('x'), 7.0))
    }

    test("evaluacion caso 5") {
        val expresion = Expo(Atomo('x'), Numero(2.0))
        assert(25.0 == Newton.evaluar(expresion, Atomo('x'), 5.0))
    }

    test("evaluacionPar caso 1") {
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        assert(35.0 == NewtonPar.evaluar(expresion, Atomo('x'), 5.0))
    }

    test("evaluacionPar caso 2") {
        val expresion = Resta(Numero(20.0), Div(Atomo('y'), Numero(4.0)))
        assert(15.0 == NewtonPar.evaluar(expresion, Atomo('y'), 20.0))
    }

    test("evaluacionPar caso 3") {
        val expresion = Prod(Atomo('a'), Resta(Numero(15.0), Atomo('b')))
        assert(15.0 == NewtonPar.evaluar(expresion, Atomo('a'), 1.0))
    }

    test("evaluacionPar caso 4") {
        val expresion = Div(Atomo('x'), Suma(Numero(7.0), Atomo('y')))
        assert(1 == NewtonPar.evaluar(expresion, Atomo('x'), 7.0))
    }

    test("evaluacionPar caso 5") {
        val expresion = Expo(Atomo('x'), Numero(2.0))
        assert(25.0 == NewtonPar.evaluar(expresion, Atomo('x'), 5.0))
    }
}
