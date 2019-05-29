package bridgeFigaro

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.{AtomicUniform, Exponential, Normal, Uniform}
import com.cra.figaro.util

import scala.io.Source

object BridgePRM2 {

  val fileDirectory = "/Users/Haoyuan/GoogleDrive/Zhang Haoyuan/NationalBridgeInventory/CleanData_AK/"

  class ParameterPrior (parameterMean: Double, parameterVariance: Double) extends ElementCollection {
    val parameterPrior: Element[Double] = Normal(parameterMean, parameterVariance)("parameterPrior", this)
  }

  class ParameterIfHasHyperParameter (val HyperParameter: ParameterPrior, variance: Double, LowB: Double, UppB: Double) {
    val parameter: Element[Double] = Normal (HyperParameter.parameterPrior, variance)
    parameter.setCondition((i: Double) => i > LowB)
    parameter.setCondition((i: Double) => i < UppB)
  }

  class Distribution (val dataInput: Iterator[String], val weibullShape: Element[Double], val weibullScale: Element[Double], val exponentialRate: Element[Double]) {
    val predictedTransition = Dist(
      0.4 -> Weibull(weibullShape, weibullScale),
      0.6 -> Exponential(exponentialRate))

    class Data (GT: Double, LT: Double) { //nested Class
      val historicalTransition = Dist(
        0.4 -> Weibull(weibullShape, weibullScale),
        0.6 -> Exponential(exponentialRate))

      historicalTransition.setCondition((i: Double) => i > GT)
      if (LT > 0) historicalTransition.setCondition((i: Double) => i < LT)
    }

    for (line <- dataInput) {
      val parts = line.split(',').toList
      if (parts.length ==4){
        val dataTemp = new Data (parts(2).toDouble, parts(3).toDouble)}
      else {
        val dataTemp = new Data (parts(2).toDouble, -1)} //right censored
    }
  }



  def loadData (fileName: String) = {
    val dataInput = Source.fromFile(fileDirectory+ fileName +".txt").getLines.drop(1) //get rid of the header
    (dataInput)
  }

  object bridgeInfo {
    val hyperBridgeWeibullShape = new ParameterPrior (0.5, 3)
    val hyperBridgeWeibullScale = new ParameterPrior (10, 50)
    val hyperBridgeExponentialRate = new ParameterPrior (0.5, 10)
  }

  object concreteBridgeInfo {
    val concreteBridgeWeibullShape = new ParameterPrior(1, 1)
    val concreteBridgeWeibullScale = new ParameterPrior(30, 30)
    val concreteBridgeExponentialRate = new ParameterPrior(1, 5)
  }

  object hyperConcreteBridgeInfo {
    val ifHyperConcreteBridgeWeibullShape = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullShape, 0.5, 0, 3)
    val ifHyperConcreteBridgeWeibullScale = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullScale, 5, 20, 40)
    val ifHyperConcreteBridgeExponentialRate = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeExponentialRate, 0.5, 1, 5)
  }

  object metalBridgeInfo {
    val metalBridgeWeibullShape = new ParameterPrior(1, 2)
    val metalBridgeWeibullScale = new ParameterPrior(20, 30)
    val metalBridgeExponentialRate = new ParameterPrior(0.5, 5)
  }

  object hyperMetalBridgeInfo {
    val ifHyperMetalBridgeWeibullShape = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullShape, 1, 0, 5)
    val ifHyperMetalBridgeWeibullScale = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullScale, 5, 10, 43)
    val ifHyperMetalBridgeExponentialRate = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeExponentialRate, 0.5, 0, 3)
  }

  def main(args: Array[String]) {

    val targetDataSize = loadData ("RT1").size

    if (targetDataSize < 100 ){
      val concreteBridgeDistribution = new Distribution (loadData ("RT1"), hyperConcreteBridgeInfo.ifHyperConcreteBridgeWeibullShape.parameter, hyperConcreteBridgeInfo.ifHyperConcreteBridgeWeibullScale.parameter, hyperConcreteBridgeInfo.ifHyperConcreteBridgeExponentialRate.parameter)
      val metalBridgeDistribution = new Distribution (loadData ("RT1"), hyperMetalBridgeInfo.ifHyperMetalBridgeWeibullShape.parameter, hyperMetalBridgeInfo.ifHyperMetalBridgeWeibullScale.parameter, hyperMetalBridgeInfo.ifHyperMetalBridgeExponentialRate.parameter)

      println("Number of elements: " + Universe.universe.activeElements.length)
/*
      val alg = Importance(1000, concreteBridgeDistribution.predictedTransition)
      alg.start()
      alg.stop()
      println(alg.distribution(concreteBridgeDistribution.predictedTransition))
      alg.kill()
      */
    }
    else {
      val concreteBridgeDistribution = new Distribution (loadData ("RT1"), concreteBridgeInfo.concreteBridgeWeibullShape.parameterPrior, concreteBridgeInfo.concreteBridgeWeibullScale.parameterPrior, concreteBridgeInfo.concreteBridgeExponentialRate.parameterPrior)

      println("Number of elements: " + Universe.universe.activeElements.length)

      val alg = Importance(1000, concreteBridgeDistribution.predictedTransition, concreteBridgeDistribution.weibullShape,concreteBridgeDistribution.weibullScale, concreteBridgeDistribution.exponentialRate)
      /*
      alg.start()
      alg.stop()
      println(alg.distribution(concreteBridgeDistribution.predictedTransition))
      println(alg.distribution(concreteBridgeDistribution.weibullShape))
      println(alg.distribution(concreteBridgeDistribution.weibullScale))
      println(alg.distribution(concreteBridgeDistribution.exponentialRate))
      alg.kill()
      */
    }
    println("Number of elements: " + Universe.universe.activeElements.length)

  }

}
