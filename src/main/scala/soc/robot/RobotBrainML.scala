package soc.robot

import game._
import soc.disableDebug.D
import soc.game._
import soc.message._
import soc.util.{CappedQueue, SOCRobotParameters}

import scala.util.Random

class RobotBrainML(rc: SOCRobotClient,
                   params: SOCRobotParameters,
                   ga: SOCGame,
                   mq: CappedQueue[SOCMessage]
                  ) extends SOCRobotBrain(rc, params, ga, mq) {

  val possibleHands = SOCPossibleHands.emptyHands

  var counterOffer: Option[Trade] = None
  var moveAndSteal: Option[RobberLocationsAndSteal] = None
  var monopolyChoice: Option[Int] = None

  //Temporary!! Anything with two steps probably needs to be done seperately
  def doMove(move: SOCPossibleMove): Unit = move match {
      case RollDice =>
        expectDICERESULT = true
        counter = 0
        client.rollDice(game)
      case EndTurn =>
        waitingForGameState = true
        counter = 0
        expectROLL_OR_CARD = true
        waitingForOurTurn = true

        doneTrading = robotParameters.getTradeFlag != 1
        client.endTurn(game)
      case InitialPlacement(settlementNode, roadEdge) =>
        client.putPiece(game, new SOCSettlement(ourPlayerData, settlementNode, null)) //Phase1A
        client.putPiece(game, new SOCRoad(ourPlayerData, roadEdge, null))             //Phase 1B
      case RobberLocationsAndSteal(bestHex, choicePl) =>
        moveAndSteal = Some(RobberLocationsAndSteal(bestHex, choicePl))

        client.moveRobber(game, ourPlayerData, bestHex)
      case BuyDevelopmentCard => client.buyDevCard(game)
      case BuildSettlement(node) =>
        val targetPiece = new SOCPossibleSettlement(ourPlayerData, node, null)
        lastMove = targetPiece
        currentDRecorder = (currentDRecorder + 1) % 2
        negotiator.setTargetPiece(this.getOurPlayerNumber, targetPiece)

        waitingForGameState = true
        counter = 0
        expectPLACING_SETTLEMENT = true
        whatWeWantToBuild = new SOCSettlement(ourPlayerData, targetPiece.getCoordinates, null)
        if (!(whatWeWantToBuild == whatWeFailedToBuild)) {
          D.ebugPrintln("!!! BUILD REQUEST FOR A SETTLEMENT " + Integer.toHexString(targetPiece.getCoordinates) + " !!!")
          client.buildRequest(game, SOCPlayingPiece.SETTLEMENT)
        }
        else { // We already tried to build this.
          cancelWrongPiecePlacementLocal(whatWeWantToBuild)
          // cancel sets whatWeWantToBuild = null;
        }
      case BuildCity(node) =>
        val targetPiece = new SOCPossibleCity(ourPlayerData, node)
        lastMove = targetPiece
        currentDRecorder = (currentDRecorder + 1) % 2
        negotiator.setTargetPiece(this.getOurPlayerNumber, targetPiece)

        waitingForGameState = true
        counter = 0
        expectPLACING_CITY = true
        whatWeWantToBuild = new SOCCity(ourPlayerData, targetPiece.getCoordinates, null)
        if (!(whatWeWantToBuild == whatWeFailedToBuild)) {
          D.ebugPrintln("!!! BUILD REQUEST FOR A CITY " + Integer.toHexString(targetPiece.getCoordinates) + " !!!")
          client.buildRequest(game, SOCPlayingPiece.CITY)
        }
        else { // We already tried to build this.
          cancelWrongPiecePlacementLocal(whatWeWantToBuild)
          // cancel sets whatWeWantToBuild = null;
        }
      case BuildRoad(edge) =>
        val targetPiece = new SOCPossibleRoad(ourPlayerData, edge, null)
        lastMove = targetPiece
        currentDRecorder = (currentDRecorder + 1) % 2
        negotiator.setTargetPiece(this.getOurPlayerNumber, targetPiece)

        waitingForGameState = true
        counter = 0
        expectPLACING_ROAD = true
        whatWeWantToBuild = new SOCRoad(ourPlayerData, targetPiece.getCoordinates, null)
        if (!(whatWeWantToBuild == whatWeFailedToBuild)) {
          D.ebugPrintln("!!! BUILD REQUEST FOR A ROAD AT " + Integer.toHexString(targetPiece.getCoordinates) + " !!!")
          client.buildRequest(game, SOCPlayingPiece.ROAD)
        }
        else { // We already tried to build this.
          cancelWrongPiecePlacementLocal(whatWeWantToBuild)
          // cancel sets whatWeWantToBuild = null;
        }

      case PortTrade(give, get) =>
        client.bankTrade(game, give, get)
        waitingForTradeMsg = true
      case Trade(offer) => ()
      case Knight(robber) =>
        expectPLACING_ROBBER = true
        waitingForGameState = true
        counter = 0
        moveAndSteal = Some(robber)

        client.playDevCard(game, SOCDevCardConstants.KNIGHT)
      //      client.moveRobber(game, ourPlayerData, robber.node)
      //      client.choosePlayer(game, robber.playerStole.getOrElse(-1))
      case YearOfPlenty(res1, res2) =>
        val resources = new SOCResourceSet()
        resources.add(1, res1)
        resources.add(1, res2)
        resourceChoices = resources

        client.playDevCard(game, SOCDevCardConstants.DISC)
      case Monopoly(res) =>
        monopolyChoice = Some(res)

        client.playDevCard(game, SOCDevCardConstants.MONO)
      case RoadBuilder(edge1, edge2) =>
        waitingForGameState = true
        counter = 0
        expectPLACING_FREE_ROAD1 = true
        whatWeWantToBuild = new SOCRoad(ourPlayerData, edge1, null)

        val targetPiece = new SOCPossibleRoad(ourPlayerData, edge2, null)
        buildingPlan.push(targetPiece)

        client.playDevCard(game, SOCDevCardConstants.ROADS)
    }

  def selectBestMove(moves: List[SOCPossibleMove]): SOCPossibleMove = moves(rand.nextInt(moves.length))

  override def considerOffer(offer: SOCTradeOffer): Int = {
    var response = SOCRobotNegotiator.IGNORE_OFFER
    val offeringPlayer = game.getPlayer(offer.getFrom)
    if ((offeringPlayer.getCurrentOffer != null) && offer == offeringPlayer.getCurrentOffer) {
      val offeredTo = offer.getTo
      if (offeredTo(getOurPlayerNumber)) {
        val trade = Trade(offer)
        val moves = GameState.getPossibleTradeResponses(trade, getOurPlayerData, getGame)
        response = selectBestMove(moves) match {
          case AcceptTrade => SOCRobotNegotiator.ACCEPT_OFFER
          case RejectTrade => SOCRobotNegotiator.REJECT_OFFER
          case CounterTrade(counter) =>
            counterOffer = Some(counter)
            SOCRobotNegotiator.COUNTER_OFFER
        }
      }
    }
    response
  }

  override def makeCounterOffer(offer: SOCTradeOffer): Boolean = {
    counterOffer match {
      case Some(Trade(offer)) =>
        getOurPlayerData.setCurrentOffer(offer)
        offerRejections(offer.getFrom) = false
        waitingForTradeResponse = true
        counter = 0
        client.offerTrade(game, offer)
        true
      case None =>
        getOurPlayerData.setCurrentOffer(null)
        doneTrading = true
        waitingForTradeResponse = false
        false
    }
  }

  override def rollOrPlayCard(): Unit = doActionsOnTurn

  override def moveRobber(): Unit = {
    moveAndSteal = Some(moveAndSteal.getOrElse {
      val state = GameState(getGame, getGame.getCurrentPlayerNumber, possibleHands)
      val moves = GameState.getPossibleRobberLocations(state.player, state.game)
      selectBestMove(moves)
    }.asInstanceOf[RobberLocationsAndSteal])

    val bestHex = moveAndSteal.map(_.node).getOrElse (0)
    D.ebugPrintln("!!! MOVING ROBBER !!!")
    client.moveRobber(game, ourPlayerData, bestHex)
    //pause(2000)
  }

  override def pickMonopolyResources(): Unit = {
    waitingForGameState = true
    expectPLAY1 = true
    counter = 0
    client.pickResourceType(game, this.monopolyChoice.getOrElse(0))
    pause(1500)
  }

  override protected def doActionsOnTurn(): Unit = {
    val state = GameState(getGame, getGame.getCurrentPlayerNumber, possibleHands)
    val moves = GameState.getPossibleMovesForState(state)
    val bestMove = selectBestMove(moves)
    doMove(bestMove)

  }

  override def pickPlayer(msg: SOCChoosePlayerRequest): Unit = {

    val choicePl = moveAndSteal.flatMap(_.playerStole).getOrElse(-1)
    counter = 0
    client.choosePlayer(game, choicePl)
    moveAndSteal = None
  }

  override def discardCards(numToDiscard: Int): SOCResourceSet = {
    val state = GameState(getGame, getGame.getCurrentPlayerNumber, possibleHands)
    val bestDiscard = selectBestMove(GameState.getPossibleDiscards(getOurPlayerData, numToDiscard)).asInstanceOf[DiscardResources]
    bestDiscard.resourceSet
  }



/*
  override def run: Unit = {
    //Thread name for debug
    try
      Thread.currentThread.setName("robo tBrain-" + client.getNickname + "-" + game.getName)
    catch {
      case th: Throwable =>
    }

    if (pinger != null) {

      pinger.start
      //
      // Along with actual game events, the pinger sends a TIMINGPING message
      // once per second, to aid the robot's timekeeping counter.
      //

      while (alive) {
        try
          val mes = gameEventQ.get // Sleeps until message received

          val mesType = {
            if (mes != null) { // Debug aid: When looking at message contents or setting a per-message breakpoint,
              // skip the pings; note (mesType != SOCMessage.TIMINGPING) here.
              val temp = mes.getType
              if (temp != SOCMessage.TIMINGPING) turnEventsCurrent.addElement(mes)
              if (D.ebugOn) D.ebugPrintln("mes - " + mes)
              temp
            }
            else -1
          }

          if (waitingForTradeMsg && (counter > 10)) {
            waitingForTradeMsg = false
            counter = 0
          }

          if (waitingForTradeResponse && (counter > 100)) { // Remember other players' responses, call client.clearOffer,
            // clear waitingForTradeResponse and counter.
            tradeStopWaitingClearOffer()
          }

          if (waitingForGameState && (counter > 10000)) { //D.ebugPrintln("counter = "+counter);
            //D.ebugPrintln("RESEND");
            counter = 0
            client.resend()
          }

          if (mesType == SOCMessage.GAMESTATE)
            handleGAMESTATE(mes.asInstanceOf[SOCGameState].getState)
            // clears waitingForGameState, updates oldGameState, calls ga.setGameState

          else if (mesType == SOCMessage.STARTGAME)
            handleGAMESTATE(mes.asInstanceOf[SOCStartGame].getGameState)
            // clears waitingForGameState, updates oldGameState, calls ga.setGameState

          if (mesType == SOCMessage.TURN) {
            // Start of a new player's turn.
            // Update game and reset most of our state fields.
            // See also below: if ((mesType == SOCMessage.TURN) && ourTurn).
            handleGAMESTATE(mes.asInstanceOf[SOCTurn].getGameState)
            // clears waitingForGameState, updates oldGameState, calls ga.setGameState
            game.setCurrentPlayerNumber(mes.asInstanceOf[SOCTurn].getPlayerNumber)
            game.updateAtTurn()
            //
            // remove any expected states
            //
            expectROLL_OR_CARD = false
            expectPLAY1 = false
            expectPLACING_ROAD = false
            expectPLACING_SETTLEMENT = false
            expectPLACING_CITY = false
            expectPLACING_SHIP = false
            expectPLACING_ROBBER = false
            expectPLACING_FREE_ROAD1 = false
            expectPLACING_FREE_ROAD2 = false
            expectPLACING_INV_ITEM = false
            expectDICERESULT = false
            expectDISCARD = false
            expectMOVEROBBER = false
            expectWAITING_FOR_DISCOVERY = false
            expectWAITING_FOR_MONOPOLY = false
            // reset the selling flags and offers history
            if (robotParameters.getTradeFlag == 1) doneTrading = false
            else doneTrading = true
            waitingForTradeMsg = false
            waitingForTradeResponse = false
            negotiator.resetIsSelling()
            negotiator.resetOffersMade()
            waitingForPickSpecialItem = null
            waitingForSC_PIRI_FortressRequest = false
            // check or reset any special-building-phase decisions
            decidedIfSpecialBuild = false
            if (game.getGameState == SOCGame.SPECIAL_BUILDING) {
              if (waitingForSpecialBuild && !buildingPlan.isEmpty) {
                // Keep the building plan.
                // Will ask during loop body to build.
              } else {
                // We have no plan, but will call planBuilding()
                // during the loop body.  If buildingPlan still empty,
                // bottom of loop will end our Special Building turn,
                // just as it would in gamestate PLAY1.  Otherwise,
                // will ask to build after planBuilding.
              }
            }
            else { // reset any plans we had
              buildingPlan.clear()
            }
            negotiator.resetTargetPieces()
            // swap the message-history queues
            val oldPrev = turnEventsPrev
            turnEventsPrev = turnEventsCurrent
            oldPrev.clear()
            turnEventsCurrent = oldPrev

            turnExceptionCount = 0
          }

          if (game.getCurrentPlayerNumber == ourPlayerNumber) {
            ourTurn = true
            waitingForSpecialBuild = false
          }
          else ourTurn = false

          if ((mesType == SOCMessage.TURN) && ourTurn) {
            waitingForOurTurn = false
            // Clear some per-turn variables.
            // For others, see above: if (mesType == SOCMessage.TURN)
            whatWeFailedToBuild = null
            failedBuildingAttempts = 0
            rejectedPlayDevCardType = -1
            rejectedPlayInvItem = null
          }

          /**
            * Handle some message types early.
            *
            * When reading the main flow of this method, skip past here;
            * search for "it's time to decide to build or take other normal actions".
            */
          mesType match {
            case SOCMessage.PLAYERELEMENT =>
              // If this during the ROLL_OR_CARD state, also updates the
              // negotiator's is-selling flags.
              // If our player is losing a resource needed for the buildingPlan,
              // clear the plan if this is for the Special Building Phase (on the 6-player board).
              // In normal game play, we clear the building plan at the start of each turn.
              handlePLAYERELEMENT(mes.asInstanceOf[SOCPlayerElement])
            case SOCMessage.PLAYERELEMENTS =>
              // Multiple PLAYERELEMENT updates;
              // see comment above for actions taken.
              handlePLAYERELEMENTS(mes.asInstanceOf[SOCPlayerElements])
            case SOCMessage.RESOURCECOUNT =>
              handlePLAYERELEMENT(null, mes.asInstanceOf[SOCResourceCount].getPlayerNumber, SOCPlayerElement.SET, SOCPlayerElement.RESOURCE_COUNT, mes.asInstanceOf[SOCResourceCount].getCount)
            case SOCMessage.DICERESULT =>
              game.setCurrentDice(mes.asInstanceOf[SOCDiceResult].getResult)
            case SOCMessage.PUTPIECE =>
              handlePUTPIECE_updateGameData(mes.asInstanceOf[SOCPutPiece])
              // For initial roads, also tracks their initial settlement in SOCPlayerTracker.
            case SOCMessage.MOVEPIECE =>
              val mpm = mes.asInstanceOf[SOCMovePiece]
              val sh = new SOCShip(game.getPlayer(mpm.getPlayerNumber), mpm.getFromCoord, null)
              game.moveShip(sh, mpm.getToCoord)

            case SOCMessage.CANCELBUILDREQUEST =>
              handleCANCELBUILDREQUEST(mes.asInstanceOf[SOCCancelBuildRequest])
            case SOCMessage.MOVEROBBER =>
              //
              // Note: Don't call ga.moveRobber() because that will call the
              // functions to do the stealing.  We just want to set where
              // the robber moved, without seeing if something was stolen.
              // MOVEROBBER will be followed by PLAYERELEMENT messages to
              // report the gain/loss of resources.
              //
              moveRobberOnSeven = false
              val newHex = mes.asInstanceOf[SOCMoveRobber].getCoordinates
              if (newHex >= 0) game.getBoard.setRobberHex(newHex, true)
              else game.getBoard.asInstanceOf[SOCBoardLarge].setPirateHex(-newHex, true)

            case SOCMessage.MAKEOFFER =>
              if (robotParameters.getTradeFlag == 1) handleMAKEOFFER(mes.asInstanceOf[SOCMakeOffer])
            case SOCMessage.CLEAROFFER =>
              if (robotParameters.getTradeFlag == 1) {
                val pn = mes.asInstanceOf[SOCClearOffer].getPlayerNumber
                if (pn != -1) game.getPlayer(pn).setCurrentOffer(null)
                else {
                  var i = 0
                  while ( {
                    i < game.maxPlayers
                  }) game.getPlayer(i).setCurrentOffer(null) {
                    i += 1; i
                  }
                }
              }
            case SOCMessage.ACCEPTOFFER =>
              if (waitingForTradeResponse && (robotParameters.getTradeFlag == 1)) if ((ourPlayerNumber == (mes.asInstanceOf[SOCAcceptOffer]).getOfferingNumber) || (ourPlayerNumber == mes.asInstanceOf[SOCAcceptOffer].getAcceptingNumber)) waitingForTradeResponse = false
            case SOCMessage.REJECTOFFER =>
              if (robotParameters.getTradeFlag == 1) handleREJECTOFFER(mes.asInstanceOf[SOCRejectOffer])
            case SOCMessage.DEVCARDACTION =>
              val dcMes = mes.asInstanceOf[SOCDevCardAction]
              if (dcMes.getAction != SOCDevCardAction.CANNOT_PLAY) handleDEVCARDACTION(dcMes)
              else { // rejected by server, can't play our requested card
                rejectedPlayDevCardType = dcMes.getCardType
                waitingForGameState = false
                expectPLACING_FREE_ROAD1 = false
                expectWAITING_FOR_DISCOVERY = false
                expectWAITING_FOR_MONOPOLY = false
                expectPLACING_ROBBER = false
              }

            case SOCMessage.SIMPLEREQUEST =>
              // These messages can almost always be ignored by bots,
              // unless we've just sent a request to attack a pirate fortress.
              // Some request types are handled at the bottom of the loop body;
              // search for SOCMessage.SIMPLEREQUEST
              if (ourTurn && waitingForSC_PIRI_FortressRequest) {
                val rqMes = mes.asInstanceOf[SOCSimpleRequest]
                if ((rqMes.getRequestType == SOCSimpleRequest.SC_PIRI_FORT_ATTACK) && (rqMes.getPlayerNumber == -1)) { // Attack request was denied: End our turn now.
                  // Reset method sets waitingForGameState, which will bypass
                  // any further actions in the run() loop body.
                  waitingForSC_PIRI_FortressRequest = false
                  resetFieldsAtEndTurn()
                  client.endTurn(game)
                }
                // else, from another player; we can ignore it
              }

            case SOCMessage.SIMPLEACTION =>
              // Most action types are handled later in the loop body;
              // search for SOCMessage.SIMPLEACTION
              mes.asInstanceOf[SOCSimpleAction].getActionType match {
                case SOCSimpleAction.SC_PIRI_FORT_ATTACK_RESULT =>
                  if (ourTurn && waitingForSC_PIRI_FortressRequest) { // Our player has won or lost an attack on a pirate fortress.
                    // When we receive this message, other messages have already
                    // been sent to update related game state. End our turn now.
                    waitingForSC_PIRI_FortressRequest = false
                    resetFieldsAtEndTurn()
                    // client.endTurn not needed; making the attack implies sending endTurn
                  }
              }
            case SOCMessage.INVENTORYITEMACTION =>
              if (mes.asInstanceOf[SOCInventoryItemAction].action == SOCInventoryItemAction.CANNOT_PLAY) {
                val itms = ourPlayerData.getInventory.getByStateAndType(SOCInventory.PLAYABLE, mes.asInstanceOf[SOCInventoryItemAction].itemType)
                if (itms != null) rejectedPlayInvItem = itms.get(0) // any item of same type# is similar enough here
                waitingForGameState = false
                expectPLACING_INV_ITEM = false // in case was rejected placement (SC_FTRI gift port, etc)

              }
            // switch(mesType)}


      }


    }

  }
  */

}
