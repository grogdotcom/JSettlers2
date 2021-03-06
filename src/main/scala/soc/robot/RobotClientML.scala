package soc.robot

import soc.game.SOCGame
import soc.message.SOCMessage
import soc.util.{CappedQueue, SOCFeatureSet, SOCRobotParameters, Version}

/**
  * Constructor for a robot which will connect to the specified host, on the specified port.
  * Does not actually connect: Call {@link #init()} when ready.
  *
  * @param h  host
  * @param p  port
  * @param nn nickname for robot
  * @param pw password for robot
  * @param co cookie for robot connections to server
  */
class RobotClientML(h: String, p: Int, nn: String, pw: String, co: String) extends SOCRobotClient(h, p, nn, pw, co) {

  def this(s: String, nn: String, pw: String, co: String) = {
    this(null, 0, nn, pw, co)
    strSocketName = s
  }

  this.rbclass = "soc.robot.RobotBrainML"

  override def createBrain(params: SOCRobotParameters, ga: SOCGame, mq: CappedQueue[SOCMessage]) = new RobotBrainML(this, params, ga, mq)

  override def buildClientFeats: SOCFeatureSet = new SOCFeatureSet(false, false)
}

object RobotClientML {
  def main(args: Array[String]): Unit = {
    if (args.length < 5) {
      System.err.println("Java Settlers robotclient " + Version.version + ", build " + Version.buildnum)
      System.err.println("usage: java soc.robot.SOCRobotClient host port_number userid password cookie")
      return
    }

    val ex1 = new RobotClientML(args(0), args(1).toInt, args(2), args(3), args(4))
    ex1.init()
  }




}


