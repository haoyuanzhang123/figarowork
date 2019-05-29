/*
package example

import bridgeFigaro.Weibull
import com.cra.figaro.example.Hierarchy.{Truck, _}
import com.cra.figaro.language.{Constant, Dist, Element, ElementCollection}
import com.cra.figaro.library.atomic.continuous.{Exponential, Normal, Uniform}

object BridgePRM {


  abstract class Parameter extends ElementCollection{
    lazy val shape: Element[Int] = Constant(1)
  }

  class WeibullPrior (shapeMean: Double, shapeVariance: Double,
                      scaleMean: Double, scaleVariance: Double) extends Parameter  {
    override val shape: Element[Double] = Normal (shapeMean, shapeVariance)("shape", this)
    val scale: Element[Double] = Normal (scaleMean, scaleVariance)("scale", this)
  }

  class ExponentialPrior (shapeMean: Double, shapeVariance: Double) extends Parameter {
    override val shape: Element[Double] = Normal (shapeMean, shapeVariance)("shape", this)
  }

  class Data (val WeibullPrior: WeibullPrior,
              val ExponentialPrior: ExponentialPrior,
              GT: Double,
              LT: Double){
    val historicalTransition = Dist(
      0.4 -> Weibull(WeibullPrior.shape, WeibullPrior.scale),
      0.6 -> Exponential(ExponentialPrior.shape))

    historicalTransition.setCondition((i: Double) => i > GT)
    if (LT > 0) historicalTransition.setCondition((i: Double) => i < LT)
  }

  class HyperParameter (hyperShapeLow: Double, hyperShapeUpp: Double, hyperScaleLow: Double, hyperScaleUpp: Double) {
    val hyperShape = Uniform (hyperShapeLow, hyperShapeUpp)
    val hyperScale = Uniform (hyperScaleLow, hyperScaleUpp)
  }

  class WeibullPriorHasSuperClass (hyperParameter: HyperParameter) extends WeibullPrior (1.2, 1.2, 1.2, 1.2){

  }

  class Distribution (val WeibullPrior: WeibullPrior,
                      val ExponentialPrior: ExponentialPrior){
    val TransitionDistribution = Dist(
      0.4 -> Weibull(WeibullPrior.shape, WeibullPrior.scale),
      0.6 -> Exponential(ExponentialPrior.shape))
  }












  abstract class ConcreteParameter extends ElementCollection{
  }

  class ConcreteWeibullPrior extends ConcreteParameter {
    val shape: Element[Double] = Normal (1.2, 1)("shape", this)
    val scale: Element[Double] = Normal (50, 50)("scale", this)
  }

  class ConcreteExponentialPrior extends ConcreteParameter {
    val rate: Element[Double] = Normal (0.5, 2)("rate", this)
  }

  class ConcreteTransition (val ConcreteWeibullPrior: ConcreteWeibullPrior,
                         val ConcreteExponentialPrior: ConcreteExponentialPrior){
    val TransitionDistribution = Dist(
      0.4 -> Weibull(ConcreteWeibullPrior.shape, ConcreteWeibullPrior.scale),
      0.6 -> Exponential(ConcreteExponentialPrior.rate))
  }


  abstract class MetalParameter extends ElementCollection{
  }

  class MetalWeibullPrior extends MetalParameter {
    val shape: Element[Double] = Normal (1.5, 2)("shape", this)
    val scale: Element[Double] = Normal (30, 100)("scale", this)
  }

  class MetalExponentialPrior extends MetalParameter {
    val rate: Element[Double] = Normal (0.5, 5)("rate", this)
  }

  class MetalTransition (val MetalWeibullPrior: MetalWeibullPrior,
                         val MetalExponentialPrior: MetalExponentialPrior){
    val TransitionDistribution = Dist(
      0.2 -> Weibull(MetalWeibullPrior.shape, MetalWeibullPrior.scale),
      0.8 -> Exponential(MetalExponentialPrior.rate))
  }


  class Data (val distribution: MetalTransition, GT: Double, LT: Double){
    val historicalTransition = distribution.TransitionDistribution

    historicalTransition.setCondition((i: Double) => i > GT)
    if (LT > 0) historicalTransition.setCondition((i: Double) => i < LT)
  }














  object Truck {
    def generate: Element[Vehicle] = Dist(0.1 -> TwentyWheeler.generate, 0.3 -> Pickup.generate, 0.6 -> Constant[Vehicle](new Truck))
  }




  class HyperParameter (hyperShapeLow: Double, hyperShapeUpp: Double, hyperScaleLow: Double, hyperScaleUpp: Double) {
    val hyperShape = Uniform (hyperShapeLow, hyperShapeUpp)
    val hyperScale = Uniform (hyperScaleLow, hyperScaleUpp)
    // override the prior if this class exists: override val speed: Element[Int] = Uniform(70, 80)("speed", this)
  }

  class TargetBridge {
   // val dataSize = Constant
  }

 // 1. select bridgeDistribution
  // 2.if the target bridge's dataSize bigger than xxx, use the hyper parameter. override the parameter
  // object ConcretePrior {
   // def generate(name: String): Element[Vehicle] =
     // Dist(0.6 -> Car.generate, 0.4 -> Truck.generate)(name, universe)
  //}


  object ConcreteTransition {
    def generate(name: String): Element[ConcreteParameter] = Dist(
      0.2 -> ConcreteWeibullPrior.generate,
      0.8 -> ConcreteExponentialPrior.generate)
  }


  object ConcreteWeibullPrior {
    def generate: Element[ConcreteWeibullPrior] = Constant(new ConcreteWeibullPrior)
  }

  object ConcreteExponentialPrior {
    def generate: Element[ConcreteExponentialPrior] = Constant(new ConcreteExponentialPrior)
  }

  object MetalWeibullPrior {
    def generate: Element[MetalWeibullPrior] = Constant(new MetalWeibullPrior)
  }

  object MetalExponentialPrior {
    def generate: Element[MetalExponentialPrior] = Constant(new MetalExponentialPrior)
  }




}
*/