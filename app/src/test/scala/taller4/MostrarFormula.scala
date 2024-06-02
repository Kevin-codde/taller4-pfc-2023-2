package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MostrarFormula extends AnyFunSuite {

    test("testTaller4") {
        assert("Taller 4" == Taller4.saludo())
    }

    // Tests for mostrarFormula function
    test("mostrarFormula caso 1") {
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        assert("(10.0 + (x * 5.0))" == Newton.mostrarFormula(expresion))
    }

    test("mostrarFormula caso 2") {
        val expresion = Resta(Numero(20.0), Div(Atomo('y'), Numero(4.0)))
        assert("(20.0 - (y / 4.0))" == Newton.mostrarFormula(expresion))
    }

    test("mostrarFormula caso 3") {
        val expresion = Prod(Atomo('a'), Resta(Numero(15.0), Atomo('b')))
        assert("(a * (15.0 - b))" == Newton.mostrarFormula(expresion))
    }

    test("mostrarFormula caso 4") {
        val expresion = Div(Atomo('x'), Suma(Numero(7.0), Atomo('y')))
        assert("(x / (7.0 + y))" == Newton.mostrarFormula(expresion))
    }

    test("mostrarFormula caso 5") {
        val expresion = Expo(Atomo('x'), Numero(2.0))
        assert("(x ^ 2.0)" == Newton.mostrarFormula(expresion))
    }

    test("mostrarFormulaPar caso 1") {
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        assert("(10.0 + (x * 5.0))" == NewtonPar.mostrarFormula(expresion))
    }

    test("mostrarFormulaPar caso 2") {
        val expresion = Resta(Numero(20.0), Div(Atomo('y'), Numero(4.0)))
        assert("(20.0 - (y / 4.0))" == NewtonPar.mostrarFormula(expresion))
    }

    test("mostrarFormulaPar caso 3") {
        val expresion = Prod(Atomo('a'), Resta(Numero(15.0), Atomo('b')))
        assert("(a * (15.0 - b))" == NewtonPar.mostrarFormula(expresion))
    }

    test("mostrarFormulaPar caso 4") {
        val expresion = Div(Atomo('x'), Suma(Numero(7.0), Atomo('y')))
        assert("(x / (7.0 + y))" == NewtonPar.mostrarFormula(expresion))
    }

    test("mostrarFormulaPar caso 5") {
        val expresion = Expo(Atomo('x'), Numero(2.0))
        assert("(x ^ 2.0)" == NewtonPar.mostrarFormula(expresion))
    }
}
