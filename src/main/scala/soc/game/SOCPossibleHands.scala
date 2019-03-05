package soc.game

import game.{ProbableResourceSet, _}

import scala.annotation.tailrec

case class Hand(cards: SOCResourceSet, multiplier: Int = 1)
case class PossibleHands(hands: Seq[Map[Int, Hand]]) {

  def playerGainCards(player: Int, set: SOCResourceSet): PossibleHands = PossibleHands {
        hands.map { hand =>
          hand.map {
            case (`player`, Hand(resources, mult)) =>
              val newSet = new SOCResourceSet(resources)
              newSet.add(set)
              player -> Hand(newSet, mult)
            case (p, rm) => p -> rm
          }
        }
      }

  def playerLoseCards(player: Int, set: SOCResourceSet): PossibleHands = PossibleHands {
      hands.filter(_ (player).cards.contains(set)).map { hand =>
        hand.map {
          case (`player`, Hand(resources, mult)) =>
            val newSet = new SOCResourceSet(resources)
            newSet.subtract(set)
            player -> Hand(newSet, mult)
          case (p,  rm) => p -> rm
        }
      }
    }

  def stealUnknownCards(robber: Int, victim: Int): PossibleHands = PossibleHands {
      hands.flatMap { hand =>
        ProbableResourceSet.resTypes.filter(hand(victim).cards.contains).map { res =>
          val amount = hand(victim).cards.getAmount(res)
          val resources = new SOCResourceSet
          resources.add(1, res)
          Hand(resources, amount)
        }.map { case Hand(set, multiplier) =>
          hand.map {
            case (`victim`, Hand(resources, mult)) =>
              val newSet = new SOCResourceSet(resources)
              newSet.subtract(set)
              victim -> Hand(newSet, mult * multiplier)
            case (`robber`, Hand(resources, mult)) =>
              val newSet = new SOCResourceSet(resources)
              newSet.add(set)
              robber -> Hand(newSet, mult * multiplier)
            case (p, rm) => p -> rm
          }
        }
      }
    }

  def playerMonopoly(player: Int, res: Int): PossibleHands = PossibleHands {
    hands.map { hand =>
      val totalRes = hand.values.map(_.cards.getAmount(res)).sum
      hand.map {
        case (`player`, Hand(resources, mult)) =>
          val newSet = new SOCResourceSet(resources)
          newSet.setAmount(totalRes, res)
          player -> Hand(newSet, mult)
        case (p, Hand(resources, mult)) =>
          val newSet = new SOCResourceSet(resources)
          newSet.subtract(resources.getAmount(res), res)
          p -> Hand(newSet, mult)
      }
    }
  }

  def toProbableResourceSets: Map[Int, ProbableResourceSet] = {

    hands.flatMap(_.toSeq).groupBy(_._1).mapValues(_.map(_._2)).map {
      case (player: Int, allResSets: Seq[Hand]) =>
        val knownResources = new SOCResourceSet()
        val numHands = allResSets.map(_.multiplier).sum
        val unknownMap = ProbableResourceSet.resTypes.map { res =>
          val amount = allResSets.map(_.cards.getAmount(res)).min
          knownResources.add(amount, res)
          val unknownAmount = allResSets.map { case Hand(resSet, multiplier) =>
            ((resSet.getAmount(res) - amount) * multiplier)
          }.sum.toDouble / numHands.toDouble
          res -> unknownAmount
        }.toMap
        val unknownResources = UnknownResources(unknownMap(1), unknownMap(2), unknownMap(3), unknownMap(4), unknownMap(5))
        player -> new ProbableResourceSet(knownResources, unknownResources)
    }
  }
}

object SOCPossibleHands {

  val emptyHands = PossibleHands {
    Seq{
      (0 to 3).map { i =>
        i -> Hand(new SOCResourceSet)
      }.toMap
    }
  }

  @tailrec
  def calculateHands(possibleHands: PossibleHands = emptyHands, transactions: List[SOCTransactions]): PossibleHands = {
    if (transactions.isEmpty) possibleHands
    else calculateHands(calculateHands(possibleHands, transactions.head), transactions.tail)
  }

  def calculateHands(possibleHands: PossibleHands, transaction: SOCTransactions): PossibleHands = transaction match {
    case Gain(player, set) => possibleHands.playerGainCards(player, set)
    case Lose(player, set) => possibleHands.playerLoseCards(player, set)
    case Steal(robber, victim, Some(set))=> possibleHands.playerLoseCards(victim, set).playerGainCards(robber, set)
    case Steal(robber, victim, None) => possibleHands.stealUnknownCards(robber, victim)
    case MonopolyTransaction(player, resourceType) => possibleHands.playerMonopoly(player, resourceType)
  }

}

sealed trait SOCTransactions

case class Gain(player: Int, resourceSet: SOCResourceSet) extends SOCTransactions
case class Lose(player: Int, resourceSet: SOCResourceSet) extends SOCTransactions
case class Steal(robber: Int, victim: Int, resourceSet: Option[SOCResourceSet]) extends SOCTransactions
case class MonopolyTransaction(player: Int, resourceType: Int) extends SOCTransactions

object TreeTester extends App {

  val list: List[SOCTransactions] = List(
    Gain(0, new SOCResourceSet(1, 1, 0, 0, 0, 0)),
    Steal(1, 0, None),
    Gain(0, new SOCResourceSet(1, 0, 2, 0, 0, 0)),
    Gain(1, new SOCResourceSet(0, 1, 0, 1, 1, 0)),
    Gain(2, new SOCResourceSet(1, 0,  0, 0, 1, 0)),
    Steal(0, 1, None),
    Gain(1, new SOCResourceSet(1, 0, 7, 0, 0, 0)),
    Steal(2, 0, None),
    Lose(0, new SOCResourceSet(2, 0, 0, 0, 0, 0)),
    Lose(0, new SOCResourceSet(0, 0, 1, 0, 0, 0)),
    MonopolyTransaction(0, 3),
    Steal(0, 1, None)

  )

  val a = SOCPossibleHands.calculateHands(transactions = list)

  a.toProbableResourceSets.foreach { case(player, prob) =>
    println(s"$player $prob")
  }
}

