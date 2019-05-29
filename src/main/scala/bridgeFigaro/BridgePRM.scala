package bridgeFigaro

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Normal

import scala.io.Source

object BridgePRM {

  val fileDirectory = "/Users/Haoyuan/GoogleDrive/Zhang Haoyuan/NationalBridgeInventory/CleanData_AK/"
  val gradingScale : Int = 3


  class ParameterPrior (parameterMean: Double, parameterVariance: Double) extends ElementCollection {
    val parameterPrior: Element[Double] = Normal(parameterMean, parameterVariance)("parameterPrior", this)
  }

  class ParameterIfHasHyperParameter (val HyperParameter: ParameterPrior, variance: Double, LowB: Double, UppB: Double) {
    val parameter: Element[Double] = Normal (HyperParameter.parameterPrior, variance)
    parameter.setCondition((i: Double) => i > LowB)
    parameter.setCondition((i: Double) => i < UppB)
  }

  class Distribution (val dataInput: Iterator[String], val weibullShape: Element[Double], val weibullScale: Element[Double]) {
    val predictedTransition = Weibull(weibullShape, weibullScale)

    class Data (GT: Double, LT: Double) { //nested Class
      val historicalTransition =Weibull(weibullShape, weibullScale)
      historicalTransition.setCondition((i: Double) => i > GT)
      if (LT > 0) historicalTransition.setCondition((i: Double) => i < LT)
    }

    var i = 0
    for (line <- dataInput) {
      i = i+1
      val parts = line.split(',').toList
      if (parts.length ==4){
        val dataTemp = new Data (parts(2).toDouble, parts(3).toDouble)}
      else {
        val dataTemp = new Data (parts(2).toDouble, -1)} //right censored
    }
    println("number of data input is " + i)
  }

  class Condition (val T1: Distribution, val T2: Distribution, val T3: Distribution, val query: Query){
    val conditionDistribution =
      Apply(T1.predictedTransition, T2.predictedTransition,T3.predictedTransition,
        (i: Double, j: Double, k: Double) =>
          if (query.queryTime  < k) "C4"
          else if (query.queryTime < j) "C3"
          else if (query.queryTime < i) "C2"
          else "C1")
  }

  class Query (time: Double) {
    val queryTime = time
  }


  /*
  for (i <- gradingScale to 1 by -1) {
    println(i)
  }
  */






  object bridgeInfo {
    val hyperBridgeWeibullShape4 = new ParameterPrior (0.5, 3)
    val hyperBridgeWeibullScale4 = new ParameterPrior (10, 50)

    val hyperBridgeWeibullShape3 = new ParameterPrior (0.6, 3)
    val hyperBridgeWeibullScale3 = new ParameterPrior (15, 50)

    val hyperBridgeWeibullShape2 = new ParameterPrior (0.8, 3)
    val hyperBridgeWeibullScale2 = new ParameterPrior (25, 50)

    val hyperBridgeWeibullShape1 = new ParameterPrior (1.2, 3)
    val hyperBridgeWeibullScale1 = new ParameterPrior (40, 50)
  }

  object concreteBridgeInfo {
    val concreteBridgeWeibullShape4 = new ParameterPrior(1, 1)
    val concreteBridgeWeibullScale4 = new ParameterPrior(30, 30)

    val concreteBridgeWeibullShape3 = new ParameterPrior(1, 1)
    val concreteBridgeWeibullScale3 = new ParameterPrior(35, 30)

    val concreteBridgeWeibullShape2 = new ParameterPrior(1, 1)
    val concreteBridgeWeibullScale2 = new ParameterPrior(45, 30)

    val concreteBridgeWeibullShape1 = new ParameterPrior(1, 1)
    val concreteBridgeWeibullScale1 = new ParameterPrior(60, 30)

  }

  object hyperConcreteBridgeInfo {
    val ifHyperConcreteBridgeWeibullShape4 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullShape4, 0.5, 0, 3)
    val ifHyperConcreteBridgeWeibullScale4 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullScale4, 5, 0, 100)

    val ifHyperConcreteBridgeWeibullShape3 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullShape3, 0.6, 0, 3)
    val ifHyperConcreteBridgeWeibullScale3 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullScale3, 10, 0, 100)

    val ifHyperConcreteBridgeWeibullShape2 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullShape2, 1.0, 0, 3)
    val ifHyperConcreteBridgeWeibullScale2 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullScale2, 15, 0, 100)

    val ifHyperConcreteBridgeWeibullShape1 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullShape1, 1.5, 0, 3)
    val ifHyperConcreteBridgeWeibullScale1 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullScale1, 20, 0, 100)
  }

  object steelBridgeInfo {
    val steelBridgeWeibullShape4 = new ParameterPrior(1, 2)
    val steelBridgeWeibullScale4 = new ParameterPrior(20, 30)

    val steelBridgeWeibullShape3 = new ParameterPrior(1.3, 2)
    val steelBridgeWeibullScale3 = new ParameterPrior(25, 30)

    val steelBridgeWeibullShape2 = new ParameterPrior(1.6, 2)
    val steelBridgeWeibullScale2 = new ParameterPrior(35, 30)

    val steelBridgeWeibullShape1 = new ParameterPrior(2.5, 2)
    val steelBridgeWeibullScale1 = new ParameterPrior(55, 30)
  }

  object hyperSteelBridgeInfo {
    val ifHyperSteelBridgeWeibullShape4 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullShape4, 1, 0, 5)
    val ifHyperSteelBridgeWeibullScale4 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullScale4, 5, 0, 100)

    val ifHyperSteelBridgeWeibullShape3 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullShape3, 1.2, 0, 5)
    val ifHyperSteelBridgeWeibullScale3 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullScale3, 10, 0, 100)

    val ifHyperSteelBridgeWeibullShape2 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullShape2, 1.5, 0, 5)
    val ifHyperSteelBridgeWeibullScale2 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullScale2, 20, 0, 100)

    val ifHyperSteelBridgeWeibullShape1 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullShape1, 2, 0, 5)
    val ifHyperSteelBridgeWeibullScale1 = new ParameterIfHasHyperParameter (bridgeInfo.hyperBridgeWeibullScale1, 35, 0, 100)
  }





  def loadData (fileName: String) = {
    val dataInput = Source.fromFile(fileDirectory+ fileName +".txt").getLines.drop(1) //get rid of the header
    dataInput
  }

  def generateConcreteDistribution (fileName: String) = {
    print("generating " + fileName + " elements: ")
    if (fileName.takeRight(1)==4){
      new Distribution (loadData (fileName), concreteBridgeInfo.concreteBridgeWeibullShape4.parameterPrior, concreteBridgeInfo.concreteBridgeWeibullScale4.parameterPrior)}
    else if (fileName.takeRight(1)==3){
      new Distribution (loadData (fileName), concreteBridgeInfo.concreteBridgeWeibullShape3.parameterPrior, concreteBridgeInfo.concreteBridgeWeibullScale3.parameterPrior)}
    else if (fileName.takeRight(1)==2){
      new Distribution (loadData (fileName), concreteBridgeInfo.concreteBridgeWeibullShape2.parameterPrior, concreteBridgeInfo.concreteBridgeWeibullScale2.parameterPrior)}
    else {
      new Distribution (loadData (fileName), concreteBridgeInfo.concreteBridgeWeibullShape1.parameterPrior, concreteBridgeInfo.concreteBridgeWeibullScale1.parameterPrior)}
  }


  def generateHyperConcreteDistribution (fileName: String)= {
    print("generating " + fileName + " elements with shared hyper-parameters: ")
    if (fileName.takeRight(1)==4){
      new Distribution (loadData (fileName), hyperConcreteBridgeInfo.ifHyperConcreteBridgeWeibullShape4.parameter, hyperConcreteBridgeInfo.ifHyperConcreteBridgeWeibullScale4.parameter)}
    else if (fileName.takeRight(1)==3){
      new Distribution (loadData (fileName), hyperConcreteBridgeInfo.ifHyperConcreteBridgeWeibullShape3.parameter, hyperConcreteBridgeInfo.ifHyperConcreteBridgeWeibullScale3.parameter)}
    else if (fileName.takeRight(1)==2){
      new Distribution (loadData (fileName), hyperConcreteBridgeInfo.ifHyperConcreteBridgeWeibullShape2.parameter, hyperConcreteBridgeInfo.ifHyperConcreteBridgeWeibullScale2.parameter)}
    else {
      new Distribution (loadData (fileName), hyperConcreteBridgeInfo.ifHyperConcreteBridgeWeibullShape1.parameter, hyperConcreteBridgeInfo.ifHyperConcreteBridgeWeibullScale1.parameter)}
  }

  def generateSteelDistribution (fileName: String) = {
    print("generating " + fileName + " elements: ")
    if (fileName.takeRight(1)==4){
      new Distribution (loadData (fileName), steelBridgeInfo.steelBridgeWeibullShape4.parameterPrior, steelBridgeInfo.steelBridgeWeibullScale4.parameterPrior)}
    else if (fileName.takeRight(1)==3){
      new Distribution (loadData (fileName), steelBridgeInfo.steelBridgeWeibullShape3.parameterPrior, steelBridgeInfo.steelBridgeWeibullScale3.parameterPrior)}
    else if (fileName.takeRight(1)==2){
      new Distribution (loadData (fileName), steelBridgeInfo.steelBridgeWeibullShape2.parameterPrior, steelBridgeInfo.steelBridgeWeibullScale2.parameterPrior)}
    else {new Distribution (loadData (fileName), steelBridgeInfo.steelBridgeWeibullShape1.parameterPrior, steelBridgeInfo.steelBridgeWeibullScale1.parameterPrior)}
  }

  def generateHyperSteelDistribution (fileName: String) = {
    print("generating " + fileName + " elements with shared hyper-parameters: ")
    if (fileName.takeRight(1)==4){
      new Distribution (loadData (fileName), hyperSteelBridgeInfo.ifHyperSteelBridgeWeibullShape4.parameter, hyperSteelBridgeInfo.ifHyperSteelBridgeWeibullScale4.parameter)}
    else if (fileName.takeRight(1)==3){
      new Distribution (loadData (fileName), hyperSteelBridgeInfo.ifHyperSteelBridgeWeibullShape3.parameter, hyperSteelBridgeInfo.ifHyperSteelBridgeWeibullScale3.parameter)}
    else if (fileName.takeRight(1)==2){
      new Distribution (loadData (fileName), hyperSteelBridgeInfo.ifHyperSteelBridgeWeibullShape2.parameter, hyperSteelBridgeInfo.ifHyperSteelBridgeWeibullScale2.parameter)}
    else {
      new Distribution (loadData (fileName), hyperSteelBridgeInfo.ifHyperSteelBridgeWeibullShape1.parameter, hyperSteelBridgeInfo.ifHyperSteelBridgeWeibullScale1.parameter)}
  }

  def learnFromOthers (dataSize: Int, dataThreshold: Int, filename: String) = {
    if (dataSize < dataThreshold){
      if (filename.contains("Concrete")){
        val target = generateHyperConcreteDistribution (filename)
        val source = generateHyperSteelDistribution  ("Steel"+filename.takeRight(1))
        target}
      else {
        val target = generateHyperSteelDistribution (filename)
        val source = generateHyperConcreteDistribution  ("Concrete"+filename.takeRight(1))
        target}}
    else {
      if (filename.contains("Concrete")){
        val target = generateConcreteDistribution (filename)
        target}
      else {
        val target = generateSteelDistribution (filename)
        target
      }
    }
  }






  def main(args: Array[String]) {
    val filename1 = "Concrete1"
    val filename2 = "Concrete2"
    val filename3 = "Concrete3"
    val dataThreshold = 2

    println("==========================================")

    val T1 = learnFromOthers(loadData (filename1).size, dataThreshold, filename1)
    val T2 = learnFromOthers(loadData (filename2).size, dataThreshold, filename2)
    val T3 = learnFromOthers(loadData (filename3).size, dataThreshold, filename3)

    val query = new Query(20)
    //when the asset is currently in C4, after 10 months, what's the probability distribution of its condition
    val condition = new Condition (T1, T2, T3, query)

    println("Total Number of elements: " + Universe.universe.activeElements.length)

    println("Start calculating")
    println("--------------------------")



    val alg = MetropolisHastings(ProposalScheme.default, condition.conditionDistribution)
    //val alg = MetropolisHastings(10000, ProposalScheme.default, condition.conditionDistribution)
    //val alg = Importance( condition.conditionDistribution)
    //val alg = Importance(1000, condition.conditionDistribution)
    //val alg = VariableElimination (condition.conditionDistribution)
    //val alg = BeliefPropagation (condition.conditionDistribution)

    alg.start()
    Thread.sleep(100000)

    //Thread.sleep(5000000)


    println("C4: " + alg.probability(condition.conditionDistribution, "C4"))
    println("C3: " + alg.probability(condition.conditionDistribution, "C3"))
    println("C2: " + alg.probability(condition.conditionDistribution, "C2"))
    println("C1: " + alg.probability(condition.conditionDistribution, "C1"))

    /*
    val alg = Importance(1000, condition.conditionDistribution, concreteBridgeDistribution1.predictedTransition, concreteBridgeDistribution1.weibullShape, concreteBridgeDistribution1.weibullScale)
    println("T1 Distribution: " + alg.distribution(concreteBridgeDistribution1.predictedTransition))
    println("T1 shape: " + alg.distribution(concreteBridgeDistribution1.weibullShape))
    println("T1 scale: " + alg.distribution(concreteBridgeDistribution1.weibullScale))
    */

    alg.kill()
    println("==========================================")

  }
}
