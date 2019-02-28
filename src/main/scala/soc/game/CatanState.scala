package game

import soc.game._

trait SOCState {
  def getStateArray: List[Double]
}

case class GameState(player: SOCPlayer,
                     game: SOCGame,
                     board: BoardState,
                     bank: BankState,
                     devCardsDeck: Int,
                     players: List[PlayerState],
                     canPlayDevCards: Boolean = true,
                     canRollDice: Boolean = true,
                     transactions: List[SOCTransactions]
                   ) extends SOCState {

  lazy val playersPossibleHandMap = players.map(p => p.position -> p.probableResourcesTree).toMap

  val levelNum = transactions.length + 1


  override def getStateArray: List[Double] =
    List(board.getStateArray, bank.getStateArray, List(devCardsDeck)).flatten ::: players.flatMap(_.getStateArray)
}

case class Hex(node: Int, res: Int, number: Option[Int])

case class BoardState(hexes: Map[Int, Hex], robberNode: Int, ports: List[Int]) extends SOCState {
  override def getStateArray: List[Double] =
    hexes.values.flatMap( hex => List(hex.node.toDouble, hex.number.getOrElse(0).toDouble)).toList :::
      ports.map(_.toDouble) :::
      List(robberNode)
}

case class BankState(resourceSet: SOCResourceSet) extends SOCState {
  override def getStateArray: List[Double] = {
    resourceSet.getAmount(SOCResourceConstants.CLAY) ::
      resourceSet.getAmount(SOCResourceConstants.ORE) ::
      resourceSet.getAmount(SOCResourceConstants.SHEEP) ::
      resourceSet.getAmount(SOCResourceConstants.WHEAT) ::
      resourceSet.getAmount(SOCResourceConstants.WOOD) :: Nil
  }
}

object GameState {

  def apply(game: SOCGame,
            currentPlayer: SOCPlayer,
            playerProbableHandMap: Map[Int, MutableSOCResourceSetTree],
            transactions: List[SOCTransactions]
           ): GameState = {
    val socBoard = game.getBoard
    val hexes = socBoard.getHexLayout.toList.map { coord =>
      val id = coord
      val hexType = socBoard.getHexTypeFromCoord(coord)
      val numberOpt = socBoard.getNumberOnHexFromCoord(coord) match {
        case 0 => None
        case n => Some(n)
      }
      Hex(id, hexType, numberOpt)
    }.groupBy(_.node).mapValues(_.head)
    val robberHex = socBoard.getRobberHex
    socBoard.getPortsLayout
    val ports = (0 to 5).flatMap(resType => socBoard.getPortCoordinates(resType).toArray).map(_.asInstanceOf[Int]).toList

    val board = BoardState(hexes, robberHex, ports)
    val bank = BankState(new SOCResourceSet(game.getBank))
    val devCardsDeck = game.getNumDevCards

    val playerList: Seq[SOCPlayer] = game.getPlayers.toList
    val curPlayerNumber = currentPlayer.getPlayerNumber
    val players = List(
      currentPlayer,
      playerList.find(_.getPlayerNumber == (curPlayerNumber + 1) % playerList.length).get,
      playerList.find(_.getPlayerNumber == (curPlayerNumber + 2) % playerList.length).get,
      playerList.find(_.getPlayerNumber == (curPlayerNumber + 3) % playerList.length).get,
    ).map { player =>
      val dots = new SOCResourceSet()
      val settlements = player.getSettlements.toArray.map(_.asInstanceOf[SOCSettlement])
      val cities = player.getCities.toArray.map(_.asInstanceOf[SOCCity])
      settlements.foreach { settlement =>
        val hexes = settlement.getAdjacentHexes.toArray.map(_.asInstanceOf[Int])
        hexes.foreach { hex =>
          val roll = Roll(socBoard.getNumberOnHexFromCoord(hex))
          val res = socBoard.getHexTypeFromCoord(hex)
          dots.add(roll.dots, res)
        }
      }
      cities.foreach { city =>
        val hexes = city.getAdjacentHexes.toArray.map(_.asInstanceOf[Int])
        hexes.foreach { hex =>
          val roll = Roll(socBoard.getNumberOnHexFromCoord(hex))
          val res = socBoard.getHexTypeFromCoord(hex)
          dots.add(roll.dots * 2, res)
        }
      }

      PlayerState(
        name = player.getName,
        position = player.getPlayerNumber,
        probableResourcesTree = playerProbableHandMap(player.getPlayerNumber),
        boardPoints = player.getSettlements.size + player.getCities.size,
        armyPoints = if(player.hasLargestArmy) 2 else 0,
        roadPoints = if(player.hasLongestRoad) 2 else 0,
        devPoints = player.getInventory.getNumVPItems,
        ports = (0 to 5).filter(player.getPortFlag).toSet,
        settlementNodes = settlements.map(_.getCoordinates).toList,
        cityNodes = cities.map(_.getCoordinates).toList,
        roadEdges = player.getRoadsAndShips.toArray.map(_.asInstanceOf[SOCRoutePiece]).map(_.getCoordinates).toList,
        playedDCards = player.getInventory.getByState(SOCInventory.KEPT).size,
        playableDCards = player.getInventory.getByState(SOCInventory.PLAYABLE).size,
        newCards = player.getInventory.getByState(SOCInventory.NEW).size,
        dots = dots,
        roadLength = player.calcLongestRoad2,
        numKnights = player.getInventory.getAmountByState(SOCInventory.KEPT, SOCDevCardConstants.KNIGHT)
      )
    }

    val canPlayDevCard = !currentPlayer.hasPlayedDevCard
    val canRollDice = game.canRollDice(currentPlayer.getPlayerNumber)

    GameState(currentPlayer, game, board, bank, devCardsDeck, players, canPlayDevCard, canRollDice, transactions)
  }

  def getPossibleInitialPlacements(player: SOCPlayer, board: SOCBoard): List[InitialPlacement] = {
    val settlementNodes: List[Int] = player.getPotentialSettlements_arr.toList
    settlementNodes.flatMap { node =>
      board.getAdjacentEdgesToNode_arr(node).toList.map(InitialPlacement(node, _))
    }
  }

  def getPossibleBuilds(player: SOCPlayer): List[SOCPossibleBuild] = {
    //val player = getOurPlayerData
    val containsResources: ResourceSet => Boolean = player.getResources.contains(_: ResourceSet)

    val buildPotentialSettlements: List[SOCPossibleBuild] = if (containsResources(SOCSettlement.COST)) {
      player.getPotentialSettlements_arr.toList.map(BuildSettlement)
    } else Nil

    val buildPotentialCities: List[SOCPossibleBuild] = if (containsResources(SOCCity.COST)) {
      player.getSettlements.toArray.toList.map{ settlement =>
        BuildCity(settlement.asInstanceOf[SOCSettlement].getCoordinates)
      }
    } else Nil

    val buildPotentialRoads: List[SOCPossibleBuild] = if (containsResources(SOCRoad.COST)) {
      player.getPotentialRoads_arr.toList.map(BuildRoad)
    } else Nil

    val buildDevelopmentCard: List[SOCPossibleBuild] = if(containsResources(SOCDevCard.COST)) List(BuyDevelopmentCard)
    else Nil

    buildPotentialSettlements ::: buildPotentialCities ::: buildPotentialRoads ::: buildDevelopmentCard
  }

  def getPossiblePortTrades(player: SOCPlayer): List[SOCPossibleMove] = {
    val resourceSet = player.getResources

    val resources = (1 to 5)
    val _3to1 = player.getPortFlag(0)
    resources.flatMap { res: Int =>
      val otherRes = resources.filterNot(_ == res)
      val num = resourceSet.getAmount(res)
      val to = new SOCResourceSet()
      val from = new SOCResourceSet()
      if (player.getPortFlag(res) && num >= 2) {
        to.add(2, res)
        otherRes.map{ r =>
          from.add(1, r)
          PortTrade(to,  from)
        }
      }
      else if (_3to1 && num >= 3) {
        to.add(3, res)
        otherRes.map{ r =>
          from.add(1, r)
          PortTrade(to,  from)
        }
      }
      else if(num >= 4) {
        to.add(4, res)
        otherRes.map{ r =>
          from.add(1, r)
          PortTrade(to,  from)
        }
      }
      else Nil
    }.toList
  }

  def getPossibleRobberLocations(player: SOCPlayer, game: SOCGame): List[RobberLocationsAndSteal] = {


    val allNodes = game.getBoard.getLandHexCoords.filterNot(_ == game.getBoard.getRobberHex)
    allNodes.flatMap { node =>
      game.getPlayersOnHex(node, null).toArray.map(_.asInstanceOf[SOCPlayer])
        .map(_.getPlayerNumber)
        .filterNot(_ == player.getPlayerNumber).toList match {
        case Nil => List(RobberLocationsAndSteal(node, None))
        case list => list.map( n => RobberLocationsAndSteal(node, Some(n)))
      }
    }
  }.toList

  def getPossibleDiscards(player: SOCPlayer): List[DiscardResources] = {

    case class MiniResourceSet(resourceSet: SOCResourceSet = new SOCResourceSet()) {
      def add(res: Int): MiniResourceSet = {
        val mini = MiniResourceSet(new SOCResourceSet(resourceSet))
        mini.resourceSet.add(1, res)
        mini
      }
      def subtract(res: Int): MiniResourceSet = {
        val mini = MiniResourceSet(new SOCResourceSet(resourceSet))
        mini.resourceSet.subtract(1, res)
        mini
      }
      def getTotal: Int = resourceSet.getTotal
    }

    val ourResources = MiniResourceSet(player.getResources)
    val numToDiscard = ourResources.getTotal / 2

    val resources = (1 to 5)
    var stack = List((MiniResourceSet(), ourResources))
    var discards: List[DiscardResources] = Nil

    while (stack.nonEmpty) {
      val (mini, ourRes) = stack.head
      stack = stack.tail

      if (mini.getTotal == numToDiscard) {
        discards = DiscardResources(mini.resourceSet) :: discards
      }
      else {
        resources.filter(ourRes.resourceSet.contains).foreach { res =>
          stack = (mini.add(res), ourRes.subtract(res)) :: stack
        }
      }
    }
    discards
  }

  def getPossibleDevelopmentCard(player: SOCPlayer, game: SOCGame): List[SOCPossibleDevCard] = {
    val getAmount: Int => Int = player.getInventory.getAmount(_ : Int)
    val resources = (1 to 5)

    val knight: List[SOCPossibleDevCard] = if (getAmount(SOCDevCardConstants.KNIGHT) > 0) {
      getPossibleRobberLocations(player, game).map(Knight)
    } else Nil

    val monopoly: List[SOCPossibleDevCard] = if (getAmount(SOCDevCardConstants.MONO) > 0) {
      resources.map(Monopoly).toList
    } else Nil

    val yearOfPlenty: List[SOCPossibleDevCard] = if (getAmount(SOCDevCardConstants.DISC) > 0) {
      resources.flatMap { res1 =>
        resources.map { res2 =>
          YearOfPlenty(res1, res2)
        }
      }.toList
    } else Nil


    val roads: List[SOCPossibleDevCard] = if (getAmount(SOCDevCardConstants.ROADS) > 0) {
      val potentialRoads = player.getPotentialRoads_arr.toList

      potentialRoads.flatMap { road1 =>
        (potentialRoads.filterNot(_ == road1) :::
          game.getBoard.getAdjacentEdgesToEdge(road1).toArray.map(_.asInstanceOf[Int])
            .filter(game.getBoard.roadOrShipAtEdge(_) == null).toList)
          .map{ road2 =>
            RoadBuilder(road1, road2)
          }
      }.distinct
    } else Nil

    knight ::: monopoly ::: yearOfPlenty ::: roads
  }

  def getPossibleMovesForState(state: GameState): List[SOCPossibleMove] = {

    val devCardMoves = if (state.canPlayDevCards) {
      getPossibleDevelopmentCard(state.player, state.game)
    } else Nil

    val beforeOrAfterDiceMoves = if (state.canRollDice) List(RollDice)
    else {
      getPossibleBuilds(state.player) :::
      getPossiblePortTrades(state.player)
      //getPossibleTrades(state.player, state.game)
    }

    devCardMoves ::: beforeOrAfterDiceMoves
  }

  def rollDiceStatesWithProbability(state: GameState): List[(GameState, Double)] = {
    (2 to 12).filterNot(_ == 7).map { num =>
      val roll = Roll(num)
      (rollDice(state, roll), roll.prob)
    }.toList
  }

  /**
    * roll the dice and the players collect the resources
    * @param diceRoll the roll of the dice. integer from 2 to 12
    * @see roll7 if dice roll is a 7
    */
  def rollDice(state: GameState, diceRoll: Roll): GameState = {
    val currentPlayer = state.players.head

    val totalResourcesCollected = new SOCResourceSet()
    // SIDE AFFECT: adds resources for player to totalResourcesCollected
    val actualResForPlayers = {
      val resForPlayers = state.players.map { p =>
        val player: SOCPlayer = state.game.getPlayer(p.position)
        val resources = state.game.getResourcesGainedFromRoll(player, diceRoll.number)
        totalResourcesCollected.add(resources)
        p -> resources
      }.toMap
      if(!state.bank.resourceSet.contains(totalResourcesCollected)) {
        val overflowTypes = {
          val total = new SOCResourceSet(state.bank.resourceSet)
          total.subtract(totalResourcesCollected)
          (1 to 5).filter(res => total.contains(res))
        }
        resForPlayers.map { case (player, resourceSet) =>
          val subtractResourceSet = new SOCResourceSet()
          overflowTypes.foreach (res => subtractResourceSet.add(resourceSet.getAmount(res), res))
          resourceSet.subtract(subtractResourceSet)
          totalResourcesCollected.subtract(subtractResourceSet)
          (player -> resourceSet)
        }
      } else resForPlayers
    }

    val transactions: List[SOCTransactions] = state.players.map { player =>
      Gain(player.position, actualResForPlayers.getOrElse(player, SOCResourceSet.EMPTY_SET))
    }

    val newHandMap = SOCPossibleHandTree.calculateHands(state.playersPossibleHandMap, transactions, state.levelNum)
    val players = state.players.map { player =>
      player.copy(probableResourcesTree = newHandMap(player.position))
    }

    val updatedBank = {
      val bnk = BankState(new SOCResourceSet(state.bank.resourceSet))
      bnk.resourceSet.subtract(totalResourcesCollected)
      bnk
    }

    state.copy(
      bank = updatedBank,
      players = players,
      canRollDice = false,
      transactions = state.transactions ::: transactions
    )
  }

  /**
    * when a 7 is rolled move the robber to a new location.
    * Steal a card from a player on the hex of the new robber location.
    * If players have more than 7 cards indicate which cards they discarding
    * @param robberLocation the new robber location
    * @param playerStole the player being stolen from
    * @param cardsLost which players lost which cards
    */
  def roll7(state: GameState,
            cardsLostMap: Map[PlayerState, DiscardResources],
            robberLocation: Int,
            steal: Option[Steal]): GameState = {
    val currentPlayer = state.players.head

    val cardsLost = cardsLostMap.mapValues(_.resourceSet)


    val transactions = List(
      cardsLostMap.map { case (player, discard) =>
        Lose(player.position, discard.resourceSet)
      }.toList,
      steal.map(List(_)).getOrElse(Nil)
    ).flatten

    val newHandMap = SOCPossibleHandTree.calculateHands(state.playersPossibleHandMap, transactions, state.levelNum)
    val players = state.players.map { player =>
      player.copy(probableResourcesTree = newHandMap(player.position))
    }

    val updatedBank = {
      val bank = new SOCResourceSet(state.bank.resourceSet)
      state.players.foreach(p => bank.add(cardsLost.getOrElse(p, SOCResourceSet.EMPTY_SET)))
      BankState(bank)
    }

    val updatedBoard = state.board//.moveRobber(robberLocation)

    state.copy(
      board = updatedBoard,
      bank = updatedBank,
      players = players,
      transactions = state.transactions ::: transactions
    )
  }

  /**
    *
    * @param toBuild
    */
  def build(state: GameState, toBuild: SOCPossibleBuild): GameState = {
    val currentPlayer = state.players.head
    val board = state.game.getBoard

    val updatedState = toBuild match {
      case BuildSettlement(node) =>
        val player = currentPlayer.copy (
          settlementNodes = currentPlayer.settlementNodes ::: List(node),
          boardPoints = currentPlayer.boardPoints + 1,
          ports = currentPlayer.ports + board.getPortTypeFromNodeCoord(node),
          dots = {
            val dots = new SOCResourceSet(currentPlayer.dots)
            board.getAdjacentHexesToNode(node).toArray.map(_.asInstanceOf[Int]).foreach { hex =>
              val roll = Roll(board.getNumberOnHexFromCoord(hex))
              val res = board.getHexTypeFromCoord(hex)
              dots.add(roll.dots, res)
            }
            dots
          }
        )
        state.copy(
          players = player :: state.players.tail
        )
      case BuildCity(node) =>
        val player = currentPlayer.copy (
          settlementNodes = currentPlayer.settlementNodes.filterNot(_ == node),
          cityNodes = currentPlayer.cityNodes ::: List(node),
          boardPoints = currentPlayer.boardPoints + 1,
          dots = {
            val dots = new SOCResourceSet(currentPlayer.dots)
            board.getAdjacentHexesToNode(node).toArray.map(_.asInstanceOf[Int]).foreach { hex =>
              val roll = Roll(board.getNumberOnHexFromCoord(hex))
              val res = board.getHexTypeFromCoord(hex)
              dots.add(roll.dots, res)
            }
            dots
          }
        )

        state.copy(
          players = player :: state.players.tail
        )
      case BuildRoad(edge) =>
        val cost = SOCRoad.COST

        // HAVE TO CALC LONGEST ROAD UPDATE
        val player = currentPlayer.copy (
          roadEdges = currentPlayer.roadEdges ::: List(edge)
        )
        state.copy(
          players = player :: state.players.tail
        )
      case BuyDevelopmentCard =>
        val player = currentPlayer.copy (
          newCards = currentPlayer.newCards + 1
        )
        state.copy(
          players = player :: state.players.tail
        )
      case _ => state
    }

    val cost = toBuild match {
      case BuildSettlement(_) => SOCSettlement.COST
      case BuildCity(_) => SOCCity.COST
      case BuildRoad(_) => SOCRoad.COST
      case BuyDevelopmentCard => SOCDevCard.COST
    }

    val transactions = List(Lose(currentPlayer.position, cost))
    val newHandMap = SOCPossibleHandTree.calculateHands(updatedState.playersPossibleHandMap, transactions, state.levelNum)
    val players = updatedState.players.map { player =>
      player.copy(probableResourcesTree = newHandMap(player.position))
    }

    val bank = {
      val bank = new SOCResourceSet(updatedState.bank.resourceSet)
      bank.add(cost)
      BankState(bank)
    }

    updatedState.copy(
      players = players,
      bank = bank
    )
  }

  def playDevelopmentCard(state: GameState, dCard: SOCPossibleDevCard): GameState = {
    val currentPlayer = state.players.head

    val playedDevCardState: GameState = dCard match {
      case Knight(robber) =>

        val updatedBoard = state.board//.moveRobber(robberLocation)

        val playersLA = {
          val numKnightsCurPlayer = currentPlayer.numKnights + 1
          val currentLAOpt = state.players.filter(_.numKnights >= 3).sortWith(_.numKnights > _.numKnights).headOption

          val playerLostArmy: Option[PlayerState] = currentLAOpt match {
            case Some(player) if player == currentPlayer => None
            case Some(player) if numKnightsCurPlayer >= player.numKnights => Some(player)
            case None => None
          }
          val updatedCurrentPlayer = (playerLostArmy match {
            case None if currentLAOpt.isDefined => currentPlayer
            case None if numKnightsCurPlayer < 3 => currentPlayer
            case _ =>
              currentPlayer.copy(
                armyPoints = 2
              )
          }).copy(
            numKnights = numKnightsCurPlayer
          )
          val updatedTail = state.players.tail.map { player =>
            playerLostArmy match {
              case Some(`player`) =>
                player.copy(
                  armyPoints = 0
                )
              case _ => player
            }
          }
          updatedCurrentPlayer :: updatedTail
        }
        //val players = stealFunc(playerStole, playersLA)
        state.copy(
          //players = players
        )
      case RoadBuilder(road1, road2) => state

      case YearOfPlenty(res1, res2) =>
        val resourceSet = SOCResourceSet.EMPTY_SET
        resourceSet.add(1, res1)
        resourceSet.add(1, res2)

        val transactions = List(Gain(currentPlayer.position, resourceSet))
        val newHandMap = SOCPossibleHandTree.calculateHands(state.playersPossibleHandMap, transactions, state.levelNum)
        val players = state.players.map { player =>
          player.copy(probableResourcesTree = newHandMap(player.position))
        }

        val updatedBank = {
          val bank = new SOCResourceSet(state.bank.resourceSet)
          bank.subtract(resourceSet)
          BankState(bank)
        }
        state.copy(
          bank = updatedBank,
          players = players,
          transactions = state.transactions ::: transactions
        )

      case Monopoly(resource) => state
        val transactions = List(MonopolyTransaction(currentPlayer.position, resource))
        val newHandMap = SOCPossibleHandTree.calculateHands(state.playersPossibleHandMap, transactions, state.levelNum)
        val players = state.players.map { player =>
          player.copy(probableResourcesTree = newHandMap(player.position))
        }
        state.copy(
          players = players,
          transactions = state.transactions ::: transactions
        )

      case Point => state
      case _ => state
    }
    playedDevCardState.copy(
      canPlayDevCards = false,
    )
  }

  def trade(state: GameState, trade: Trade): GameState = {
    val currentPlayer = state.players.head

    val transactions = List(
      Lose(currentPlayer.position, trade.from),
      Lose(trade.player, trade.to),
      Gain(currentPlayer.position, trade.to),
      Gain(trade.player, trade.from)
    )

    val newHandMap = SOCPossibleHandTree.calculateHands(state.playersPossibleHandMap, transactions, state.levelNum)
    val players = state.players.map { player =>
      player.copy(probableResourcesTree = newHandMap(player.position))
    }

    state.copy(
      players = players,
      transactions = state.transactions ::: transactions
    )

  }

  def portTrade(state: GameState, portTrade: PortTrade): GameState = {
    val currentPlayer = state.players.head

    val updatedBank = {
      val bank = new SOCResourceSet(state.bank.resourceSet)
      bank.subtract(portTrade.to)
      bank.add(portTrade.from)
      BankState(bank)
    }

    val transactions = List(
      Lose(currentPlayer.position, portTrade.from),
      Gain(currentPlayer.position, portTrade.to)
    )
    val newHandMap = SOCPossibleHandTree.calculateHands(state.playersPossibleHandMap, transactions, state.levelNum)
    val players = state.players.map { player =>
      player.copy(probableResourcesTree = newHandMap(player.position))
    }

    state.copy (
      bank = updatedBank,
      players = players,
      transactions = state.transactions ::: transactions
    )
  }

  def initialSettlement(player: PlayerState, settlement: InitialPlacement): Unit = ()

  def getStateWeightedAverage(curState: GameState, statesWithProb: List[(GameState, Double)]): GameState = {
    null
  }
}

case class Robber(playerStole: PlayerState, resourceStolen: Int)
//case class Trade(tradePartner: CatanPlayer, cardsReceived: ResourceSet, cardsSent: ResourceSet)





