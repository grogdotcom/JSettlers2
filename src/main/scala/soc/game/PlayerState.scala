package game

import soc.game._


case class PlayerState(name: String,
                       position: Int,
                       //currentKnownResources: SOCResourceSet = SOCResourceSet.EMPTY_SET,
                       probableResourcesTree: MutableSOCResourceSetTree,
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
                      ) extends SOCState {



  def points = boardPoints + devPoints+ armyPoints + roadPoints

//  def collectedResourceFromRoll(resourceSet: SOCResourceSet): PlayerState = {
//    val c = copy(
//      currentKnownResources = new SOCResourceSet(currentKnownResources)
//    )
//    c.currentKnownResources.add(resourceSet)
//    c
//
//  }

  override def getStateArray: List[Double] = {
    val probableResourceSet =  probableResourcesTree.toProbableResourceSet
    position ::
      probableResourceSet.getTotal ::
      probableResourceSet.known.getAmount(SOCResourceConstants.CLAY) ::
      probableResourceSet.unknown.getAmount(SOCResourceConstants.CLAY) ::
      probableResourceSet.known.getAmount(SOCResourceConstants.ORE) ::
      probableResourceSet.unknown.getAmount(SOCResourceConstants.ORE) ::
      probableResourceSet.known.getAmount(SOCResourceConstants.SHEEP) ::
      probableResourceSet.unknown.getAmount(SOCResourceConstants.SHEEP) ::
      probableResourceSet.known.getAmount(SOCResourceConstants.WHEAT) ::
      probableResourceSet.unknown.getAmount(SOCResourceConstants.WHEAT) ::
      probableResourceSet.known.getAmount(SOCResourceConstants.WOOD) ::
      probableResourceSet.unknown.getAmount(SOCResourceConstants.WOOD) ::
      dots.getAmount(SOCResourceConstants.CLAY) ::
      dots.getAmount(SOCResourceConstants.ORE) ::
      dots.getAmount(SOCResourceConstants.SHEEP) ::
      dots.getAmount(SOCResourceConstants.WHEAT) ::
      dots.getAmount(SOCResourceConstants.WOOD) ::
      points :: boardPoints :: armyPoints :: roadPoints :: devPoints ::
      playedDCards :: playableDCards :: newCards ::  roadLength :: numKnights ::
      (ports.toList ::: (1 to (6 - ports.size)).map(_ => 0).toList) :::
      (settlementNodes ::: (1 to (5 - settlementNodes.size)).map(_ => 0).toList) :::
      (cityNodes ::: (1 to (4 - cityNodes.size)).map(_ => 0).toList) :::
      (roadEdges ::: (1 to (15 - roadEdges.size)).map(_ => 0).toList) ::: Nil
  }
}
