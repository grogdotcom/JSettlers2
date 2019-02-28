package game

import soc.game._

sealed trait SOCPossibleMove

trait SOCPossibleBuild extends SOCPossibleMove
trait SOCPossibleTrade extends SOCPossibleMove
trait SOCPossibleDevCard extends SOCPossibleMove
trait TradeResponse extends SOCPossibleMove

case object RollDice extends SOCPossibleMove
case class Roll(number: Int) extends SOCPossibleMove {
  val dots: Int = 6 - Math.abs(7 - number)
  val prob: Double = dots / 36.0
}

case object EndTurn extends SOCPossibleMove
case class InitialPlacement(settlementNode: Int, roadNode: Int) extends SOCPossibleMove
case class RobberLocationsAndSteal(node: Int, playerStole: Option[Int]) extends SOCPossibleMove
case class DiscardResources(resourceSet: SOCResourceSet) extends SOCPossibleMove

case object BuyDevelopmentCard extends SOCPossibleBuild
case class BuildRoad(node: Int) extends SOCPossibleBuild
case class BuildSettlement(node: Int) extends SOCPossibleBuild
case class BuildCity(node: Int) extends SOCPossibleBuild

case class PortTrade(to: SOCResourceSet, from: SOCResourceSet) extends SOCPossibleTrade
case class Trade(player: Int, to: SOCResourceSet, from: SOCResourceSet)  extends SOCPossibleTrade

case class Knight(robber: RobberLocationsAndSteal) extends SOCPossibleDevCard
case class YearOfPlenty(res1: Int, res2: Int) extends SOCPossibleDevCard
case class Monopoly(res: Int) extends SOCPossibleDevCard
case class RoadBuilder(road1: Int, road2: Int) extends SOCPossibleDevCard
case object Point extends SOCPossibleDevCard

case object AcceptTrade extends TradeResponse
case object RejectTrade extends TradeResponse

case class PlayersMove(player: Int, move: SOCPossibleMove)


