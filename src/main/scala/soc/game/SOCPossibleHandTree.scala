package soc.game

import game.{ProbableResourceSet, _}

import scala.annotation.tailrec

sealed trait SOCResourceSetTree {

  type MTree = MutableSOCResourceSetTree

  var removed = false

  val indecies: List[Index]
  val depth: Int
  val multiplier: Int
  val linkedTree: Option[MTree]

  val index = indecies.head
  def path = indecies.reverse

  override def toString: String = {

    val a = this match {
      case n: Node => n.children.map(c => s"\n${"  " * (n.depth + 1)}$c").mkString("")
      case l: Leaf => ProbableResourceSet.resTypes.map(l.resSet.getAmount(_)).mkString(",")
      case _ => ""
    }
    s"${index.level}${index.v.map(v => s".$v- ").getOrElse("- ")}" + a

  }

  def getAllPossibleHands: List[Leaf] = this match {
    case n: Node => n.children.flatMap(_.getAllPossibleHands)
    case l: Leaf => List(l)
  }

  def getHandsForIndex(path: List[Index]): List[Leaf] = getTreeFromIndexPath(path).flatMap(_.getAllPossibleHands)

  def remove(tree: SOCResourceSetTree): Option[SOCResourceSetTree] = {
    if (tree.isRemoved) Some(this)
    else {
      this match {
        case n: Node if n.children.contains(tree) =>
          val ret = Some(n.copy(children = n.children.filterNot(_ == tree)))
          tree.removed = true
          tree.linkedTree.map(_.removeTreeAndClear(tree.index))
          ret
        case n: Node => Some(n.copy(children = n.children.flatMap(_.remove(tree))))
        case l: Leaf => Some(l)
      }
    }
  }

  def getEmptyNodes: List[Node] = this match {
    case _: Leaf => Nil
    case n: Node if n.children.isEmpty => List(n)
    case n: Node if n.isRemoved => List(n)
    case n: Node => n.children.flatMap(_.getEmptyNodes)
  }

  def clearNodes: Option[SOCResourceSetTree] = {
    val empty = getEmptyNodes
    if (empty.isEmpty) Some(this)
    else {
      remove(empty.head).flatMap(_.clearNodes)
    }
  }

  def isRemoved = removed

  def getTreeFromIndexPath(path: List[Index]): List[SOCResourceSetTree] = this match {
    case _ if path.isEmpty => List(this)
    case n: Node if n.index.level < path.head.level => n.children.flatMap(_.getTreeFromIndexPath(path))
    case n: Node if n.index.level > path.head.level => n.getTreeFromIndexPath(path.tail)
    case n: Node => (n.index.v, path.head.v) match {
      case (None, None) if path.tail.isEmpty => List(this)
      case (Some(t), Some(o)) if t == o && path.tail.isEmpty => List(this)
      case (None, None) =>  n.children.flatMap(_.getTreeFromIndexPath(path.tail))
      case (None, Some(o)) => Nil
      case (Some(t), None) => Nil
      case (Some(t), Some(o)) if t == o => n.children.flatMap(_.getTreeFromIndexPath(path.tail))
      case (Some(t), Some(o)) if t != o => Nil
    }
    case l: Leaf if path.tail.isEmpty => (l.index.v, path.head.v) match {
      case (None, None) => List(l)
      case (None, Some(o)) => Nil
      case (Some(t), None) => Nil
      case (Some(t), Some(o)) if t == o => List(l)
      case (Some(t), Some(o)) if t != o => Nil
    }
  }

  def replaceTree_recur(toReplace: SOCResourceSetTree, replacement: SOCResourceSetTree): Option[SOCResourceSetTree] = this match {
    case _ if toReplace == this => Some(replacement)
    case n: Node if n.children.contains(toReplace) =>
      Some(n.copy(children = replacement :: n.children.filterNot(_ == toReplace)))
    case n: Node => Some(n.copy(children = n.children.flatMap(_.replaceTree_recur(toReplace, replacement))))
    case l: Leaf => Some(l)
  }

  def replaceTrees(toReplace: List[SOCResourceSetTree], replacement: SOCResourceSetTree): Option[SOCResourceSetTree] = {
    if (toReplace.isEmpty) Some(this)
    else replaceTree_recur(toReplace.head, replacement).flatMap(_.replaceTrees(toReplace.tail, replacement))
  }

  def replaceTrees(toReplace: List[SOCResourceSetTree], replacement: List[SOCResourceSetTree]): Option[SOCResourceSetTree] = {
    require(toReplace.size == replacement.size)
    if (toReplace.isEmpty) Some(this)
    else replaceTree_recur(toReplace.head, replacement.head).flatMap(_.replaceTrees(toReplace.tail, replacement.tail))
  }

  def replaceTreeFromIndexPath(replacement: SOCResourceSetTree, path: List[Index]): Option[SOCResourceSetTree] = {
    replaceTrees(getTreeFromIndexPath(path), replacement)
  }

  private def gainResources_recur(linkedTree: Option[MTree], resSets: TransactionInfo*): Option[SOCResourceSetTree] = this match {
    case l: Leaf =>
      val leaves = resSets.map { case TransactionInfo(index, resourceSet, mult) =>
        val resources = new SOCResourceSet(l.resSet)
        resources.add(resourceSet)
        Leaf(indecies = index :: l.indecies, resources, depth = l.depth + 1, linkedTree = linkedTree, multiplier = l.multiplier * mult)
      }.toList
      Some(Node(l.indecies, leaves, depth = l.depth))
    case n: Node => Some(n.copy(children = n.children.flatMap(_.gainResources_recur(linkedTree, resSets:_*))))
  }

  def gainResources(linkedTree: Option[MTree], resSets: TransactionInfo*): Option[SOCResourceSetTree] = {
    gainResources_recur(linkedTree, resSets: _*)
  }

  def gainResourcesAtPath(path: List[Index], linkedTree: Option[MTree], resSets: TransactionInfo*): Option[SOCResourceSetTree] = {
    val toReplace = getTreeFromIndexPath(path)
    val replacements = toReplace.flatMap(_.gainResources(linkedTree, resSets: _*))
    replaceTrees(toReplace, replacements)
  }

  private def loseResources_recur(linkedTree: Option[MTree], resSets: TransactionInfo*): Option[SOCResourceSetTree] = this match {
    case l: Leaf =>
      val leaves = resSets.map { case TransactionInfo(index, resourceSet, mult) =>
        val resources = new SOCResourceSet(l.resSet)
        val check = resources.contains(resourceSet)
        resources.subtract(resourceSet)
        Leaf(indecies = index :: l.indecies, resources, depth = l.depth + 1, linkedTree = linkedTree, multiplier = l.multiplier * mult) -> check
      }.filter(_._2).map(_._1).toList
      Some(Node(l.indecies, leaves, linkedTree = l.linkedTree, depth = l.depth))
    case n: Node => Some(n.copy(children = n.children.flatMap(_.loseResources_recur(linkedTree, resSets:_*))))
  }

  def loseResourcesAtPath(path: List[Index], linkedTree: Option[MTree], resSets: TransactionInfo*): Option[SOCResourceSetTree] = {
    val toReplace = getTreeFromIndexPath(path)
    val replacements = toReplace.flatMap(_.loseResources(linkedTree, resSets: _*))
    replaceTrees(toReplace, replacements)
  }

  def loseResources(linkedTree: Option[MTree], resSets: TransactionInfo*): Option[SOCResourceSetTree] = {
    loseResources_recur(linkedTree, resSets:_*).flatMap(_.clearNodes)
  }


  def contains(index: Index): Boolean = this match {
    case l: Leaf => l.index == index
    case n: Node if n.index == index => true
    case n: Node if n.index.level > index.level => false
    case n: Node => n.children.exists(_.contains(index))
  }

  def getTreesAtLevel(level: Int): List[SOCResourceSetTree] = {
    if (!getLevels.contains(level)) Nil
    else {
      this match {
        case n: Node if n.index.level < level => n.children.flatMap(_.getTreesAtLevel(level))
        case n: Node if n.index.level == level => List(n)
        case l: Leaf if l.index.level == level => List(l)
        case _ => Nil

      }
    }
  }

  def getLevels: List[Int] = this match {
    case n: Node => n.index.level :: n.children.flatMap(_.getLevels)
    case l: Leaf => List(l.index.level)
  }

  def toProbableResourceSet = {
    val allResSets = this.getAllPossibleHands.map(hand => (hand.resSet, hand.multiplier))
    val knownResources = new SOCResourceSet()
    val numHands = allResSets.map(_._2).sum
    val unknownMap = ProbableResourceSet.resTypes.map { res =>
      val amount = allResSets.map(_._1.getAmount(res)).min
      knownResources.add(amount, res)
      val unknownAmount = allResSets.map { case (resSet, multiplier) =>
        ((resSet.getAmount(res) - amount) * multiplier)
      }.sum.toDouble / numHands.toDouble
      res -> unknownAmount
    }.toMap
    val unknownResources = UnknownResources(unknownMap(1), unknownMap(2), unknownMap(3), unknownMap(4), unknownMap(5))
    new ProbableResourceSet(knownResources, unknownResources)
  }

  def copyTree(emptyTrees: Map[Int, MutableSOCResourceSetTree]): SOCResourceSetTree = this match {
    case n: Node => n.copy(children = n.children.map(_.copyTree(emptyTrees)), linkedTree = n.linkedTree.map(t => emptyTrees(t.player)))
    case l: Leaf => l.copy(resSet = new SOCResourceSet(l.resSet), linkedTree = l.linkedTree.map(t => emptyTrees(t.player)))
  }
}

class MutableSOCResourceSetTree(var tree: SOCResourceSetTree, val player: Int) {

  def gainResources(linkedTree: Option[MutableSOCResourceSetTree], resSets: TransactionInfo*): Unit = {
    tree = tree.gainResources(linkedTree, resSets:_*).get
  }

  def gainResourcesAtPath(path: List[Index], linkedTree: Option[MutableSOCResourceSetTree], resSets: TransactionInfo*): Unit = {
    tree = tree.gainResourcesAtPath(path, linkedTree, resSets: _*).get
  }

  def loseResources(linkedTree: Option[MutableSOCResourceSetTree],resSets: TransactionInfo*): Unit = {
    tree = tree.loseResources(linkedTree, resSets:_*).get
  }

  def loseResourcesAtPath(path: List[Index], linkedTree: Option[MutableSOCResourceSetTree], resSets: TransactionInfo*): Unit = {
    tree = tree.loseResourcesAtPath(path, linkedTree, resSets: _*).get
  }

  def getHandsForIndex(path: List[Index]): List[Leaf] = tree.getHandsForIndex(path)

  def getLevels = tree.getLevels

  def removeTreeAndClear(toRemoveIndex: Index) = {

    tree.getTreeFromIndexPath(List(toRemoveIndex)).headOption match {
      case Some(toRemove) =>
        tree = tree.remove(toRemove).get
        tree = tree.clearNodes.get
      case None => ()
    }


  }

  def copyTree(emptyTrees: Map[Int, MutableSOCResourceSetTree]) = tree.copyTree(emptyTrees)

  def getAllPossibleHands = tree.getAllPossibleHands

  override def toString = tree.toString

  def toProbableResourceSet = tree.toProbableResourceSet
}



case class Leaf(indecies: List[Index],
                resSet: SOCResourceSet,
                linkedTree: Option[MutableSOCResourceSetTree] = None,
                multiplier: Int = 1,
                depth: Int = 0) extends SOCResourceSetTree

case class Node(indecies: List[Index] = Nil,
                children: List[SOCResourceSetTree],
                linkedTree: Option[MutableSOCResourceSetTree] = None,
                multiplier: Int = 1,
                depth: Int = 0) extends SOCResourceSetTree

case class TransactionInfo(index: Index, resourceSet: SOCResourceSet, multiplier: Int = 1)

case class Index(level: Int, v: Option[Int] = None)

object SOCPossibleHandTree {

  def calculateHands(playerToHandTreeMap: Map[Int, MutableSOCResourceSetTree], transactions: List[SOCTransactions], startIndex: Int): Map[Int, MutableSOCResourceSetTree] = {
    var treeMap = playerToHandTreeMap
    transactions.zipWithIndex.foreach { case (transaction, index) =>
      treeMap = calculateHands(treeMap, transaction, startIndex + index + 1)
    }
    treeMap
  }

  def calculateHands(transactions: List[SOCTransactions]): Map[Int, MutableSOCResourceSetTree] = {
    val playerToHandTreeMap: Map[Int, MutableSOCResourceSetTree] = (0 to 3).map { player =>
      player -> generateMutable(player)
    }.toMap
    calculateHands(playerToHandTreeMap, transactions, 0)
  }

  def calculateHands(oldMap: Map[Int, MutableSOCResourceSetTree], transaction: SOCTransactions, index: Int):  Map[Int, MutableSOCResourceSetTree] = {

    val playerToHandTreeMap = copyPlayerPossibleHandMap(playerToHandTreeMap)

    transaction match {
      case Gain(player, set) =>
        playerToHandTreeMap(player).gainResources(None, TransactionInfo(Index(index), set))
      case Lose(player, set) =>
        playerToHandTreeMap(player).loseResources(None, TransactionInfo(Index(index), set))
      case Steal(robber, victim, Some(set)) =>
        playerToHandTreeMap(robber).gainResources(None, TransactionInfo(Index(index), set))
        playerToHandTreeMap(victim).loseResources(None, TransactionInfo(Index(index), set))
      case Steal(robber, victim, None) =>
        val commonIndexes = getCommonIndexPaths(playerToHandTreeMap(victim), playerToHandTreeMap(robber))
        var v = 0
        commonIndexes.foreach { path =>

          var resourceStoleList:  List[TransactionInfo] = Nil

          val handTrees = playerToHandTreeMap(victim).getHandsForIndex(path)
          handTrees.foreach { vHand =>

            val resourceStole: List[TransactionInfo] = ProbableResourceSet.resTypes.filter(vHand.resSet.contains)
              .map { res =>
                val amount = vHand.resSet.getAmount(res)
                val vi = v
                v += 1

                val resources = new SOCResourceSet
                resources.add(1, res)
                TransactionInfo(Index(index, Some(vi)), resources, amount)
              }.toList
            resourceStoleList = resourceStole ::: resourceStoleList
            playerToHandTreeMap(victim).loseResourcesAtPath(vHand.path, Some(playerToHandTreeMap(robber)), resourceStole: _*)
          }
          playerToHandTreeMap(robber).gainResourcesAtPath(path, Some(playerToHandTreeMap(victim)), resourceStoleList: _*)
        }
      case MonopolyTransaction(monopolyPlayer, resourceType) =>
        var v = 0
        playerToHandTreeMap.keys.filterNot(_ == monopolyPlayer).foreach { player =>
          playerToHandTreeMap(player).getAllPossibleHands.map { hand =>
            val resources = new SOCResourceSet()
            resources.add(hand.resSet.getAmount(resourceType), resourceType)
            val infos = TransactionInfo(Index(index, Some(v)), resources)
            playerToHandTreeMap(player).loseResourcesAtPath(hand.path, None, infos)
            playerToHandTreeMap(monopolyPlayer).gainResourcesAtPath(hand.path, None, infos)
            v = v + 1
          }
        }
      }
    playerToHandTreeMap
  }

  def copyPlayerPossibleHandMap(playerHandMap: Map[Int, MutableSOCResourceSetTree]): Map[Int, MutableSOCResourceSetTree] = {
    val emptyTrees = playerHandMap.keysIterator.map(k => k -> generateMutable(k)).toMap
    val copiedTrees = playerHandMap.mapValues(_.tree.copyTree(emptyTrees))
    emptyTrees.keysIterator.foreach { player =>
      emptyTrees(player).tree = copiedTrees(player)
    }
    emptyTrees
  }

  def getCommonIndexPaths(tree1: MutableSOCResourceSetTree, tree2: MutableSOCResourceSetTree): List[List[Index]] = {
    val commonLevels = tree1.getLevels.filter(tree2.getLevels.contains)
    val allHands = tree1.tree.getAllPossibleHands
    allHands.map(_.path.filter(i => commonLevels.contains(i.level))).distinct
  }

  def generateMutable(player: Int) = new MutableSOCResourceSetTree(Leaf(List(Index(0)), new SOCResourceSet()), player)


}

object TreeTester extends App {

//  val tree1 = Leaf(Index(0), new SOCResourceSet(1, 0, 2, 0, 0, 0))
//  println(tree1)
//  val tree2 = tree1.gainResources(List((Index(1), new SOCResourceSet(2, 0, 0, 0, 0, 0)))).get
//  println(tree2)
//  val tree3 = tree2.gainResources(List(
//    (Index(2, Some(1)), new SOCResourceSet(0, 0, 0, 1, 0, 0)),
//    (Index(2, Some(2)), new SOCResourceSet(0, 1, 0, 1, 1, 0))
//  )).get
//  println(tree3)
//  val tree4 = tree3.gainResources(List(
//    (Index(3, Some(1)), new SOCResourceSet(1, 1, 0, 0, 0, 0)),
//    (Index(3, Some(2)), new SOCResourceSet(0, 0, 2, 1, 1, 0)),
//    (Index(3, Some(3)), new SOCResourceSet(0, 2, 0, 2, 1, 0)),
//  )).get
//  println(tree4)
//
//  //tree4.asInstanceOf[Node].children.head.asInstanceOf[Node].children.head.asInstanceOf[Node].children.head
//  val removeNode = tree4.getTreeFromIndexPath(List(Index(2, Some(1)))).head
//
//  val tree5 = tree4.remove(removeNode).get
//  println(tree5)
//
//  val tree6 = tree4.loseResources(List((Index(4), new SOCResourceSet(0, 0, 0, 0, 1, 0)))).flatMap(_.clearNodes).get
//  println(tree6)

//  val emptyTree = tree6.getTreeFromIndexPath(List(Index(2, Some(2)), Index(3, Some(2)))).head
//  emptyTree.asInstanceOf[Node].children.filterNot(_.getAllPossibleHands.isEmpty).foreach(println)





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

  val a = SOCPossibleHandTree.calculateHands(list)

  a.foreach { case (i, tree) =>
      println(s"$i ${tree}")

  }
}

