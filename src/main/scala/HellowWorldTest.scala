/*
 * Test.scala 
 * A simple Figaro test program.
 * 
 * Created By:      Michael Reposa (mreposa@cra.com)
 * Creation Date:   Aug 6, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.library.atomic.continuous.Normal

object HelloWorldTest {
	def main(args: Array[String]) {
		val test = Constant("Test")

		val algorithm = VariableElimination(test)
		algorithm.start()
		println(algorithm.probability(test, "Hello World"))
        println(algorithm.probability(test, "Test"))

		println(algorithm.probability(test, "Test"))

        val sunny = Binomial(7,0.2)
        println(VariableElimination.probability(sunny,1))
        val temp = Normal (40, 100)
        def greater (d: Double) = d > 30
        print(Importance.probability(temp, greater _))
				val avg = Flip(0.3)
				println(VariableElimination.probability(avg,true))

		println(util.Properties.versionString)




	}
}
