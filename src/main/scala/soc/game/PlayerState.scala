package game

import soc.game._


case class PlayerState(name: String,
                       position: Int,
                       //currentKnownResources: SOCResourceSet = SOCResourceSet.EMPTY_SET,
                       boardPoints: Int = 0,
                       armyPoints: Int = 0,
                       roadPoints: Int = 0,
                       devPoints: Int = 0,
                       ports: Set[Int] =  Set.empty,
                       settlementNodes: List[Int] = Nil,
                       cityNodes: List[Int] = Nil,
                       roadEdges: List[Int] = Nil,
                       playedDCards: Int,
                       playableDCards: Int,
                       newCards: Int,
                       dots: SOCResourceSet  = SOCResourceSet.EMPTY_SET,
                       roadLength: Int = 0,
                       numKnights: Int = 0
//                       resGainedRolls: SOCResourceSet  = SOCResourceSet.EMPTY_SET,
//                       resGainedSteal: SOCResourceSet  = SOCResourceSet.EMPTY_SET,
//                       resGainedDev: SOCResourceSet  = SOCResourceSet.EMPTY_SET,
//                       resLost7: SOCResourceSet  = SOCResourceSet.EMPTY_SET,
//                       resLostSteal: SOCResourceSet  = SOCResourceSet.EMPTY_SET,
//                       resLostDev: SOCResourceSet  = SOCResourceSet.EMPTY_SET,
//                       numPortTrades: Int = 0,
//                       resTradedInPort: SOCResourceSet  = SOCResourceSet.EMPTY_SET,
//                       resReceivedPort: SOCResourceSet  = SOCResourceSet.EMPTY_SET,
//                       numTrades: Int = 0,
//                       resTradedInTrade: SOCResourceSet  = SOCResourceSet.EMPTY_SET,
//                       resReceivedTrade: SOCResourceSet  = SOCResourceSet.EMPTY_SET,
                      ) {



  def points = boardPoints + devPoints+ armyPoints + roadPoints


  def getStateArray(probableResourceSet: ProbableResourceSet): List[Double] = {
    position.toDouble ::
      probableResourceSet.getTotal.toDouble ::
      probableResourceSet.getKnown.getAmount(SOCResourceConstants.CLAY).toDouble ::
      probableResourceSet.unknown.getAmount(SOCResourceConstants.CLAY) ::
      probableResourceSet.getKnown.getAmount(SOCResourceConstants.ORE).toDouble ::
      probableResourceSet.unknown.getAmount(SOCResourceConstants.ORE) ::
      probableResourceSet.getKnown.getAmount(SOCResourceConstants.SHEEP).toDouble ::
      probableResourceSet.unknown.getAmount(SOCResourceConstants.SHEEP) ::
      probableResourceSet.getKnown.getAmount(SOCResourceConstants.WHEAT).toDouble ::
      probableResourceSet.unknown.getAmount(SOCResourceConstants.WHEAT) ::
      probableResourceSet.getKnown.getAmount(SOCResourceConstants.WOOD).toDouble ::
      probableResourceSet.unknown.getAmount(SOCResourceConstants.WOOD) ::
      dots.getAmount(SOCResourceConstants.CLAY).toDouble ::
      dots.getAmount(SOCResourceConstants.ORE).toDouble ::
      dots.getAmount(SOCResourceConstants.SHEEP).toDouble ::
      dots.getAmount(SOCResourceConstants.WHEAT).toDouble ::
      dots.getAmount(SOCResourceConstants.WOOD).toDouble ::
      points.toDouble :: boardPoints.toDouble :: armyPoints.toDouble ::
      roadPoints.toDouble :: devPoints.toDouble ::
      playedDCards.toDouble :: playableDCards.toDouble :: newCards.toDouble ::
      roadLength.toDouble :: numKnights.toDouble ::
      (if (ports.contains(SOCBoard.CLAY_PORT)) 1.0 else 0.0) ::
      (if (ports.contains(SOCBoard.ORE_PORT)) 1.0 else 0.0) ::
      (if (ports.contains(SOCBoard.SHEEP_PORT)) 1.0 else 0.0) ::
      (if (ports.contains(SOCBoard.WHEAT_PORT)) 1.0 else 0.0) ::
      (if (ports.contains(SOCBoard.WOOD_PORT)) 1.0 else 0.0) ::
      (if (ports.contains(SOCBoard.MISC_PORT)) 1.0 else 0.0) ::
      (if (ports.contains(SOCBoard.MISC_PORT)) 1.0 else 0.0) ::
      (if (ports.contains(SOCBoard.MISC_PORT)) 1.0 else 0.0) ::
      (if (ports.contains(SOCBoard.MISC_PORT)) 1.0 else 0.0) ::
      (settlementNodes.map(_.toDouble) ::: (1 to (5 - settlementNodes.size)).map(_ => 0.0).toList) :::
      (cityNodes.map(_.toDouble) ::: (1 to (4 - cityNodes.size)).map(_ => 0.0).toList) :::
      (roadEdges.map(_.toDouble) ::: (1 to (15 - roadEdges.size)).map(_ => 0.0).toList) ::: Nil
  }
}
