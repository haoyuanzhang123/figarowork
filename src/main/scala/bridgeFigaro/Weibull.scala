package bridgeFigaro

import com.cra.figaro.language._
import com.cra.figaro.util._
import scala.math.{ log, exp }
import scala.math.pow

/**
  * A Weibull distribution in which the mean and variance are constants.
  */
class AtomicWeibull(name: Name[Double], val shape: Double, val scale: Double, collection: ElementCollection)
  extends Element[Double](name, collection) with Atomic[Double] with Weibull {

  type Randomness = Double

  def shapeValue: Double = shape
  def scaleValue: Double = scale

  def generateRandomness() = scale * pow(-(log(random.nextDouble)), 1.0 / shape)

  def generateValue(rand: Randomness) = rand

  def density(d: Double) = if (d < 0.0) 0.0 else
    (shape/scale)*pow((d/scale),(shape-1))*exp(-pow((d/scale),shape))
  override def toString = "Weibull(" + shape + ", " + scale + ")"
}

/**
  * A Weibull distribution in which the shape is an element and the scale is constant.
  */
class WeibullCompoundShape(name: Name[Double], val shape: Element[Double], val scale: Double, collection: ElementCollection)
  extends NonCachingChain(
    name,
    shape,
    (s: Double) => new AtomicWeibull("", s, scale, collection),
    collection)
    with Weibull {

  def shapeValue = shape.value
  lazy val scaleValue = scale

  override def toString = "Weibull(" + shape + ", " + scale + ")"
}


/**
  * A Weibull distribution in which the shape is constant and the scale is an element.
  */
class WeibullCompoundScale(name: Name[Double], val shape: Double, val scale: Element[Double], collection: ElementCollection)
  extends NonCachingChain(
    name,
    scale,
    (sc: Double) => new AtomicWeibull("", shape, sc, collection),
    collection)
    with Weibull {

  def scaleValue = scale.value
  lazy val shapeValue = shape

  override def toString = "Weibull(" + shape + ", " + scale + ")"
}

/**
  * A Weibull distribution in which the shape and scale are both elements.
  */
class CompoundWeibull(name: Name[Double], val shape: Element[Double], val scale: Element[Double], collection: ElementCollection)
  extends NonCachingChain[Double, Double](
    name,
    shape,
    (m: Double) => new NonCachingChain(
      "",
      scale,
      (v: Double) => new AtomicWeibull("", m, v, collection),
      collection),
    collection)
    with Weibull {

  def shapeValue = shape.value
  def scaleValue = scale.value

  override def toString = "Weibull(" + shape + ", " + scale + ")"
}

trait Weibull extends Continuous[Double] {
  /**
    * Current shape value.
    */
  def shapeValue: Double

  /**
    * Current scale value.
    */
  def scaleValue: Double
  // TODO implement appropriate log transform
  def logp(value: Double) = Double.NegativeInfinity,
}

object Weibull extends Creatable {
  /**
    * Create a Weibull element in which both shape and scale parameters are constants.
    */
  def apply(shape: Double, scale: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicWeibull(name, shape, scale, collection)

  /**
    * Create a Weibull element in which the shape parameter is an element and scale is a constant.
    */
  def apply(shape: Element[Double], scale: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new WeibullCompoundShape(name, shape, scale, collection)

  /**
    * Create a Weibull element in which the shape parameter is an element and scale is a constant.
    */
  def apply(shape: Double, scale: Element[Double])(implicit name: Name[Double], collection: ElementCollection) =
    new WeibullCompoundScale(name, shape, scale, collection)
  /**
    * Create a Weibull element in which both the shape and scale parameters are elements.
    */
  def apply(shape: Element[Double], scale: Element[Double])(implicit name: Name[Double], collection: ElementCollection) =
    new CompoundWeibull(name, shape, scale, collection)

  type ResultType = Double

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Double]], args(1).asInstanceOf[Element[Double]])
}