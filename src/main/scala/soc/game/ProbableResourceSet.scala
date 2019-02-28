package game

import soc.game.{ResourceSet, SOCResourceConstants, SOCResourceSet}

case class UnknownResources(cl: Double = 0, or: Double = 0, sh: Double = 0, wh: Double = 0, wo: Double = 0) {

  ProbableResourceSet.resTypes.foreach(res => require(getAmount(res) >= 0))

  def getAmount(resourceType: Int): Double = resourceType match {
    case SOCResourceConstants.CLAY => cl
    case SOCResourceConstants.ORE => or
    case SOCResourceConstants.SHEEP => sh
    case SOCResourceConstants.WHEAT => wh
    case SOCResourceConstants.WOOD => wo
    case _ => 0
  }

  def add(amount: Double, resourceType: Int): UnknownResources = resourceType match {
    case SOCResourceConstants.CLAY => copy(cl = cl + amount)
    case SOCResourceConstants.ORE => copy(or = or + amount)
    case SOCResourceConstants.SHEEP => copy(sh = sh + amount)
    case SOCResourceConstants.WHEAT => copy(wh = wh + amount)
    case SOCResourceConstants.WOOD => copy(wo = wo + amount)
    case _ => copy()
  }

  def subtract(amount: Double, resourceType: Int): UnknownResources = resourceType match {
    case SOCResourceConstants.CLAY => if(cl - amount > 0) copy(cl = cl - amount) else copy()
    case SOCResourceConstants.ORE => if(or - amount > 0) copy(or = or - amount) else copy()
    case SOCResourceConstants.SHEEP => if(sh - amount > 0) copy(sh = sh - amount) else copy()
    case SOCResourceConstants.WHEAT => if(wh - amount > 0) copy(wh = wh - amount) else copy()
    case SOCResourceConstants.WOOD => if(wo - amount > 0) copy(wo = wo - amount) else copy()
    case _ => copy()
  }
}

case class ProbableResourceSet(known: SOCResourceSet, unknown: UnknownResources) extends ResourceSet{

//  private def doubleSplit(d: Double): (Int, Double) = {
//    val int = d.floor.toInt
//    val dub = d - int
//    (int, dub)
//  }

  def getTotalProbableAmount(resourceType: Int): Double = getAmount(resourceType) + getProbableAmount(resourceType)

  /**
    * How many resources of this type are contained in the set?
    *
    * @param resourceType the type of resource, like { @link SOCResourceConstants#CLAY}
    * @return the number of a kind of resource
    * @see #contains(int)
    * @see #getTotal()
    */
  override def getAmount(resourceType: Int): Int = known.getAmount(resourceType)

  def getProbableAmount(resourceType: Int): Double = unknown.getAmount(resourceType)


  /**
    * Does the set contain any resources of this type?
    *
    * @param resourceType the type of resource, like { @link SOCResourceConstants#CLAY}
    * @return true if the set's amount of this resource &gt; 0
    * @see #getAmount(int)
    * @see #contains(ResourceSet)
    */
  override def contains(resourceType: Int): Boolean = getAmount(resourceType) > 0

  def mightContain(resourceType: Int): Boolean = getTotalProbableAmount(resourceType) > 0

  def probabilityContains(resourceType: Int): Double = {
    if (contains(resourceType)) 1.0
    else getProbableAmount(resourceType) / getTotal
  }

  def getProbabilityOfResourceInHand(resourceType: Int): Double = getTotalProbableAmount(resourceType) / getTotal

  /**
    * Get the number of known resource types contained in this set:
    * {@link SOCResourceConstants#CLAY} to {@link SOCResourceConstants#WOOD},
    * excluding {@link SOCResourceConstants#UNKNOWN} or {@link SOCResourceConstants#GOLD_LOCAL}.
    * An empty set returns 0, a set containing only wheat returns 1,
    * that same set after adding wood and sheep returns 3, etc.
    *
    * @return The number of resource types in this set with nonzero resource counts.
    */
  override def getResourceTypeCount: Int = known.getResourceTypeCount

  def getResourceTypeMightCount: Int = ProbableResourceSet.resTypes.filter(mightContain).length

  /**
    * Get the total number of resources in this set
    *
    * @return the total number of resources
    * @see #getAmount(int)
    */
  override def getTotal: Int = getKnownTotal + getUnknownTotal

  def getKnownTotal: Int = known.getTotal

  def getUnknownTotal: Int = ProbableResourceSet.resTypes.map(getProbableAmount).sum.round.toInt

  /**
    * @return true if this contains at least the resources in other
    * @param other the sub set, can be { @code null} for an empty resource subset
    * @see #contains(int)
    */
  override def contains(other: ResourceSet): Boolean = known.contains(other)

  def mightContain(other: ResourceSet): Boolean = ProbableResourceSet.resTypes.forall { res => getTotalProbableAmount(res).ceil >= other.getAmount(res) }

  override def toString: String = ProbableResourceSet.resTypes.filter(getTotalProbableAmount(_) > 0).map { res =>
    s"${SOCResourceConstants.resName(res)}= ${getAmount(res) + (if(getUnknownTotal > 0) getProbableAmount(res) / getUnknownTotal else 0)}"
  }.mkString(", ")

  def copy(known: SOCResourceSet = known, unknown: UnknownResources = unknown): ProbableResourceSet = {
    ProbableResourceSet(new SOCResourceSet(known), unknown.copy())
  }

}

object ProbableResourceSet {

  val resTypes = (1 to 5)

  def apply(known: SOCResourceSet): ProbableResourceSet  = ProbableResourceSet(known, UnknownResources())
  def apply(cl: Int = 0, or: Int = 0, sh: Int = 0, wh: Int = 0, wo: Int = 0): ProbableResourceSet = apply(new SOCResourceSet(cl, or, sh, wh, wo, 0))

  def takeUnknown(from: ProbableResourceSet, to: ProbableResourceSet): (ProbableResourceSet, ProbableResourceSet) = {
    val fromKnown = new SOCResourceSet(from.known)
    var fromUnknown = from.unknown.copy()
    val fromTotal = from.getTotal

    val toKnown = new SOCResourceSet(to.known)
    var toUnknown = to.unknown.copy()

    resTypes.foreach { res =>
      val amount = from.getTotalProbableAmount(res)
      if (from.known.contains(res)) {
        fromKnown.subtract(1, res)
        fromUnknown = fromUnknown.add(1, res)
      }
      val amt = amount / fromTotal.toDouble
      fromUnknown = fromUnknown.subtract(amt, res)
      toUnknown = toUnknown.add(amt, res)
    }
    (new ProbableResourceSet(fromKnown, fromUnknown), new ProbableResourceSet(toKnown, toUnknown))
  }

  def undoSteal(from: ProbableResourceSet, to: ProbableResourceSet): (ProbableResourceSet, ProbableResourceSet) = {


    resTypes.foreach { res =>


    }
    null
  }

  def calculateProbableResources(transactions: List[SOCTransactions]): Map[Int, ProbableResourceSet] = {
    val transactionsWithIndex = transactions.zipWithIndex
    val nonStealTransactions = transactionsWithIndex.filterNot(_._1.isInstanceOf[Steal])
    val stealTransactions = transactionsWithIndex.filter(_._1.isInstanceOf[Steal])

    val gained = (0 to 3).map{ player =>
      player -> new SOCResourceSet()
    }.toMap
    val lost = (0 to 3).map{ player =>
      player -> new SOCResourceSet()
    }.toMap
    val nonStealResources = (0 to 3).map{ player =>
      player -> new SOCResourceSet()
    }.toMap
    var lastEvaluation = -1
    nonStealTransactions.foreach {
      case (Gain(player, set), i) =>
        nonStealResources(player).add(set)
        gained(player).add(set)
      case (Steal(robber, victim, resOpt), i) => resOpt match {
        case Some(resourceSet) =>
          nonStealResources(robber).add(resourceSet)
          gained(robber).add(resourceSet)
          nonStealResources(victim).subtract(resourceSet)
          lost(victim).subtract(resourceSet)
        case _ => ()
      }

       case (Lose(player, set), i) =>
        lost(player).add(set)
        if (nonStealResources(player).contains(set)) nonStealResources(player).subtract(set)
        else { //player stole resource and did not collect from roll or card
          val overFlow = new SOCResourceSet(set)
          overFlow.subtract(nonStealResources(player))
          println(ProbableResourceSet(overFlow))
        }
    }
    null
  }
}

object Tester extends App {
  val transactions = List(
    Gain(1, new SOCResourceSet(1, 3, 2, 0, 2, 0)),
    Steal(2, 1, None),
    Lose(1, new SOCResourceSet (2, 0, 0, 1, 1, 0))
  )
  ProbableResourceSet.calculateProbableResources(transactions)






//  val from = ProbableResourceSet(wo = 3, cl = 1)
//  val to = ProbableResourceSet(wo = 2, or = 2)
//  val (from2, to2) = ProbableResourceSet.takeUnknown(from, to)
//  println(s"from: $from")
//  println(s"to: $to")
//  println()
//  println(s"from2: $from2")
//  println(s"to2: $to2")
//  val(to3, from3) = ProbableResourceSet.takeUnknown(to2, from2)
//  println()
//  println(s"from3: $from3")
//  println(s"to3: $to3")
}

sealed trait SOCTransactions

case class Gain(player: Int, resourceSet: SOCResourceSet) extends SOCTransactions
case class Lose(player: Int, resourceSet: SOCResourceSet) extends SOCTransactions
case class Steal(robber: Int, victim: Int, resourceSet: Option[SOCResourceSet]) extends SOCTransactions
case class MonopolyTransaction(player: Int, resourceType: Int) extends SOCTransactions

