package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Limpieza extends AnyFunSuite {

    test("testTaller4") {
        assert("Taller 4" == Taller4.saludo())
    }

    // Tests for limpieza function
    test("limpieza caso 1") {
        val expresion = Suma(Numero(0.0),Suma(Prod(Numero(1.0),Numero(5.0)),Prod(Atomo('x'),Numero(0.0))))
        val resultado = Numero(5.0)
        assert(resultado == Newton.limpiar(expresion))
    }

    test("limpieza caso 2") {
        val expresion = Resta(Numero(20.0), Div(Atomo('y'), Numero(4.0)))
        val resultado = Resta(Numero(20.0), Div(Atomo('y'), Numero(4.0)))
        assert(resultado == Newton.limpiar(expresion))
    }

    test("limpieza caso 3") {
        val expresion = Prod(Atomo('a'), Resta(Numero(15.0), Atomo('b')))
        val resultado = Prod(Atomo('a'), Resta(Numero(15.0), Atomo('b')))
        assert(resultado == Newton.limpiar(expresion))
    }

    test("limpieza caso 4") {
        val expresion = Div(Atomo('x'), Suma(Numero(7.0), Atomo('y')))
        val resultado = Div(Atomo('x'), Suma(Numero(7.0), Atomo('y')))
        assert(resultado == Newton.limpiar(expresion))
    }

    test("limpieza caso 5") {
        val expresion = Expo(Atomo('x'), Numero(2.0))
        val resultado = Expo(Atomo('x'), Numero(2.0))
        assert(resultado == Newton.limpiar(expresion))
    }

    test("limpiezaPar caso 1") {
        val expresion = Suma(Numero(0.0),Suma(Prod(Numero(1.0),Numero(5.0)),Prod(Atomo('x'),Numero(0.0))))
        val resultado = Numero(5.0)
        assert(resultado == NewtonPar.limpiar(expresion))
    }

    test("limpiezaPar caso 2") {
        val expresion = Resta(Numero(20.0), Div(Atomo('y'), Numero(4.0)))
        val resultado = Resta(Numero(20.0), Div(Atomo('y'), Numero(4.0)))
        assert(resultado == NewtonPar.limpiar(expresion))
    }

    test("limpiezaPar caso 3") {
        val expresion = Prod(Atomo('a'), Resta(Numero(15.0), Atomo('b')))
        val resultado = Prod(Atomo('a'), Resta(Numero(15.0), Atomo('b')))
        assert(resultado == NewtonPar.limpiar(expresion))
    }

    test("limpiezaPar caso 4") {
        val expresion = Div(Atomo('x'), Suma(Numero(7.0), Atomo('y')))
        val resultado = Div(Atomo('x'), Suma(Numero(7.0), Atomo('y')))
        assert(resultado == NewtonPar.limpiar(expresion))
    }

    test("limpiezaPar caso 5") {
        val expresion = Expo(Atomo('x'), Numero(2.0))
        val resultado = Expo(Atomo('x'), Numero(2.0))
        assert(resultado == NewtonPar.limpiar(expresion))
    }
}
