package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Derivadas extends AnyFunSuite {

    test("testTaller4") {
        assert("Taller 4" == Taller4.saludo())
    }

    test("derivada caso 1") {
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        val resultado = Suma(Numero(0.0),Suma(Prod(Numero(1.0),Numero(5.0)),Prod(Atomo('x'),Numero(0.0))))
        assert(resultado == Newton.derivar(expresion, Atomo('x')))
    }

    test("derivada caso 2") {
        val expresion = Resta(Prod(Numero(4.0), Atomo('x')), Numero(7.0))
        val resultado = Numero(4.0)
        assert(resultado == Newton.limpiar(Newton.derivar(expresion, Atomo('x'))))
    }

    test("derivada caso 3") {
        val expresion = Div(Atomo('y'), Numero(3.0))
        val resultado = Div(Numero(3.0), Expo(Numero(3.0), Numero(2.0)))
        assert(resultado == Newton.limpiar(Newton.derivar(expresion, Atomo('y'))))
    }

    test("derivada caso 4") {
        val expresion = Expo(Atomo('x'), Numero(3.0))
        val resultado = Prod(Atomo('x'), Div(Numero(3.0), Atomo('x')))
        assert(resultado == Newton.limpiar(Newton.derivar(expresion, Atomo('x'))))
    }

    test("derivada caso 5") {
        val expresion = Suma(Prod(Numero(5.0), Atomo('x')), Resta(Numero(3.0), Atomo('y')))
        val resultado = Numero(5.0)
        assert(resultado == Newton.limpiar(Newton.derivar(expresion, Atomo('x'))))
    }

    test("derivadaPar caso 1") {
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        val resultado = Suma(Numero(0.0),Suma(Prod(Numero(1.0),Numero(5.0)),Prod(Atomo('x'),Numero(0.0))))
        assert(resultado == NewtonPar.derivar(expresion, Atomo('x')))
    }

    test("derivadaPar caso 2") {
        val expresion = Resta(Prod(Numero(4.0), Atomo('x')), Numero(7.0))
        val resultado = Numero(4.0)
        assert(resultado == NewtonPar.limpiar(NewtonPar.derivar(expresion, Atomo('x'))))
    }

    test("derivadaPar caso 3") {
        val expresion = Div(Atomo('y'), Numero(3.0))
        val resultado = Div(Numero(3.0), Expo(Numero(3.0), Numero(2.0)))
        assert(resultado == NewtonPar.limpiar(NewtonPar.derivar(expresion, Atomo('y'))))
    }

    test("derivadaPar caso 4") {
        val expresion = Expo(Atomo('x'), Numero(3.0))
        val resultado = Prod(Atomo('x'), Div(Numero(3.0), Atomo('x')))
        assert(resultado == NewtonPar.limpiar(NewtonPar.derivar(expresion, Atomo('x'))))
    }

    test("derivadaPar caso 5") {
        val expresion = Suma(Prod(Numero(5.0), Atomo('x')), Resta(Numero(3.0), Atomo('y')))
        val resultado = Numero(5.0)
        assert(resultado == NewtonPar.limpiar(NewtonPar.derivar(expresion, Atomo('x'))))
    }
}