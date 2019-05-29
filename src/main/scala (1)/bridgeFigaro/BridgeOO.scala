package bridgeFigaro

import java.nio.file.{Files, Paths}
import java.io.File

import com.cra.figaro.algorithm.factored.VariableElimination

import scala.io.Source
import com.cra.figaro.library.atomic.continuous.{AtomicUniform, Exponential, Normal, Uniform}
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._

import scala.io.Source


object BridgeOO {

  class HyperParameter (hyperShapeLow: Double, hyperShapeUpp: Double, hyperScaleLow: Double, hyperScaleUpp: Double) {
    val hyperShape = Uniform (hyperShapeLow, hyperShapeUpp)
    val hyperScale = Uniform (hyperScaleLow, hyperScaleUpp)
  }

  class Parameter (hyperParameter: HyperParameter, shapeVariance: Double, scaleVariance: Double) {
    val shape = Normal (hyperParameter.hyperShape, shapeVariance)
    shape.setCondition((i: Double) => i > 1)
    shape.setCondition((i: Double) => i < 3)

    val scale = Normal (hyperParameter.hyperScale, scaleVariance)
    scale.setCondition((i: Double) => i > 10)
    scale.setCondition((i: Double) => i < 30)
  }

  class Data (val distribution: Parameter, GT: Double, LT: Double){
    val historicalTransition = Weibull (distribution.shape, distribution.scale)
    historicalTransition.setCondition((i: Double) => i > GT)
    if (LT > 0) historicalTransition.setCondition((i: Double) => i < LT)
  }

  class Transition (val distribution: Parameter){
    val predictTransition = Weibull (distribution.shape, distribution.scale)
  }

  class Condition (val transition1: Transition, val transition2: Transition, val query: Query){
    val conditionDistribution =
      Apply(transition1.predictTransition, transition2.predictTransition,
      (i: Double, j: Double) =>
        if (query.queryTime < i) "good";
        else if (query.queryTime < j) "fair";
        else "poor")
  }

  class Query (time: Double) {
    val queryTime = time
  }





  val hyperParameterTransition1 = new HyperParameter (1, 3, 15, 30)
  val transitionGroup11 = new Parameter (hyperParameterTransition1, 0.2, 10)
  val transitionGroup12 = new Parameter (hyperParameterTransition1, 0.3, 5)

  val hyperParameterTransition2 = new HyperParameter (1.5, 3, 20, 30)
  val transitionGroup21 = new Parameter (hyperParameterTransition2, 0.2, 5)


  def readData(directoryName: String) {
    val allLines = Source.fromFile(directoryName).getLines.drop(1) //get rid of the header

    for (line <- allLines) {
      val parts = line.split(',').toList
      if (parts.length ==4){
        val dataTemp = new Data (transitionGroup11, parts(2).toDouble, parts(3).toDouble)
      } else
      {val dataTemp = new Data (transitionGroup11, parts(2).toDouble, -1)} //right censored
    }
  }

  readData("/Users/Haoyuan/GoogleDrive/Zhang Haoyuan/NationalBridgeInventory/CleanData_AK/Concrete5.txt")


  val bridgeTD1 = new Transition (transitionGroup11)
  val bridgeTD2 = new Transition (transitionGroup21)
  val bridgeQuery = new Query(30)
  val bridgeCondition = new Condition(bridgeTD1, bridgeTD2,bridgeQuery)

  println("Number of elements: " + Universe.universe.activeElements.length)


  /*
  //val imp = Importance(transitionGroup11.shape, transitionGroup11.scale, bridgeTD1.predictTransition, bridgeCondition.conditionDistribution)
  val imp = MetropolisHastings(ProposalScheme.default, transitionGroup11.shape, transitionGroup11.scale, bridgeTD1.predictTransition, bridgeCondition.conditionDistribution)
  //val imp = VariableElimination(transitionGroup11.shape, transitionGroup11.scale, bridgeTD1.predictTransition, bridgeCondition.conditionDistribution)
  imp.start
  Thread.sleep(10000)
  //val currentTime = 15
  //val result = imp.probability(bridgeTransition.predictTransition, (i: Double) => i < currentTime)
  //val add = Select (result -> "good", 1- result ->"poor")
  //println("Probability result = " + add)

    imp.stop
    println(imp.mean(transitionGroup11.shape))
    println(imp.distribution(transitionGroup11.scale))
    //val a = imp.distribution(bridgeTransition.predictTransition)
    //val b = imp.mean(bridgeTransition.predictTransition)

   println(imp.distribution(bridgeCondition.conditionDistribution))
    imp.kill()
*/





  val imp = Importance(20, bridgeCondition.conditionDistribution)
  //val imp = MetropolisHastings(20,ProposalScheme.default, bridgeCondition.conditionDistribution)
  //val imp = Importance(1000, bridgeCondition.conditionDistribution)
  //val imp = Importance(1000, bridgeTD1.predictTransition)


  imp.start
  //Thread.sleep(100000)
  imp.stop
  println(imp.distribution(bridgeCondition.conditionDistribution))
  imp.kill()




  def main(args: Array[String]) {
     //val trainingDirectoryName = args(0)
     //val emails = readData(trainingDirectoryName)
   }
 }

/*
class Condition (val transition: Transition){
  val currentTime = 1.5
  def getCondition(i: Double): String =
    if (i > transition.predictTransition.value) "good"; else "poor"
  println(getCondition(15))

  val goodMood = Dist(0.2 -> Normal(2, 6), 0.8 -> Normal(0.2,3))

}
*/