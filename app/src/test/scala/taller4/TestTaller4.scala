package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTaller4 extends AnyFunSuite {



    // Comparación con margen de error
    def aproximadamenteIgual(x: Double, y: Double, epsilon: Double = 0.000001): Boolean = {
        Math.abs(x - y) < epsilon
    }

    test("testSaludo") {
        assert("Taller 4" == Taller4.saludo())
    }

    //Creame tests para cada metodo de la clase NewtonPar
    test("testMostrarFormula") {
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        val formula = NewtonPar.mostrarFormula(expresion)
        assert("(10.0 + (x * 5.0))" == formula)
    }

    test("testDerivar") {
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        val derivada = NewtonPar.derivar(expresion,Atomo('x'))
        assert("(0.0 + ((1.0 * 5.0) + (x * 0.0)))" == NewtonPar.mostrarFormula(derivada))
    }

    test("testEvaluar") {
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        val evaluar = NewtonPar.evaluar(expresion,Atomo('x'),5.0)
        assert(35.0 == evaluar)
    }

    test("testLimpiar") {
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        val derivada = NewtonPar.derivar(expresion,Atomo('x'))
        val linpio = NewtonPar.limpiar(derivada)
        assert("5.0" == NewtonPar.mostrarFormula(linpio))
    }


    test("testRaizNewton2") {
        val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
        val res2 = NewtonPar.raizNewton(e2, Atomo('x'), 2.0, NewtonPar.buenaAproxi)
        assert(2.0 == res2)
    }

}
