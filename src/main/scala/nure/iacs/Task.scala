package nure.iacs

import vegas.DSL.Vegas
import vegas.spec.Spec

import scala.annotation.tailrec
import scala.util.Random
import java.io.{File, FileWriter}

//git on https://github.com/Yevhenii-V0lk0v/IACS.git
//you can find an executable file there: IACS-assembly-0.1.jar

object Task extends App {
  val random = new Random(0)

//  val nodeAmount = 8192
    val nodeAmount = 1000

  @tailrec
  def solve(counter: Int, colorDistribution: Double, nodes: List[Node], arcs: List[Arc]): (List[Node], List[Arc]) = {
    if (counter % 100 == 0) {
      println(counter)
    }
    counter match {
      case 0 => (nodes, arcs)
      case _ =>
        val node = Node(random.nextFloat() <= colorDistribution, nodes.size)
        val nodePowers = parallelMap(nodes.filter(n => n.blue != node.blue), 300, (n: Node) => (n, arcs.count(_.contains(n)).toDouble)).toList

        val powerSum = nodePowers.map(_._2).sum

        def getRandomNode(randomPoint: Double): Node = {
          @tailrec
          def getRandomNodeInternal(randomPoint: Double, candidates: List[(Node, Double)]): Node = {
            if (randomPoint <= candidates.head._2) {
              candidates.head._1
            } else {
              getRandomNodeInternal(randomPoint - candidates.head._2, candidates.tail)
            }
          }

          getRandomNodeInternal(randomPoint, nodePowers)
        }

        solve(counter - 1, colorDistribution, node :: nodes, Arc(node, getRandomNode(random.nextDouble()), arcs.length) :: arcs)
    }
  }

  def saveGraph(nodes: List[Node], arcs: List[Arc]): Unit = {
    val graphFile = new File("node_graph.csv")

    if (graphFile.exists() || graphFile.createNewFile()) {
      val fileWriter = new FileWriter(graphFile)
      arcs.foreach(a => {
        fileWriter.write(s"${a.a};${a.b}\n")
        fileWriter.flush()
      })
      fileWriter.close()
    }
  }

  val a = Node(blue = true, 0)
  val b = Node(blue = false, 1)


  val (nodes, arcs) = solve(nodeAmount - 2, 0.6, List(a, b), List(Arc(a, b, 0)))

//    saveGraph(nodes.sortBy(_.id), arcs.sortBy(_.id))

  val (blues, pinks) = nodes.partition(_.blue)

  def printStats(nodeGroup: List[Node]): Unit = {
    println(s"Nodes: ${nodeGroup.length} (${nodeGroup.length.toDouble / nodeAmount * 100}%)")
    println(s"Mean power: ${nodeGroup.map(n => arcs.count(_.contains(n))).sum / nodeGroup.length.toDouble}")
    println(s"Max power: ${nodeGroup.map(n => arcs.count(_.contains(n))).max}")
  }

  println("Overall stats")
  printStats(nodes)
  println("Stats for blues")
  printStats(blues)
  println("Stats for pinks")
  printStats(pinks)

  def getPowers(nodeGroup: List[Node]): List[(Int, Int)] = (for (n <- nodeGroup) yield (n, arcs.count(_.contains(n)))).groupBy(_._2).mapValues(_.length).toList

  def plotFrequencyDistribution(): Unit = {
    val pinkPowers = for (row <- getPowers(pinks)) yield Map("color" -> "pink", "power" -> row._1, "amount" -> row._2)
    val bluePowers = for (row <- getPowers(blues)) yield Map("color" -> "blue", "power" -> row._1, "amount" -> row._2)

    val powers = pinkPowers ++ bluePowers

    Vegas(description = "Frequency power distribution", width = 400, height = 400)
      .withData(powers)
      .encodeX(field = "power",
        title = "Node's power",
        dataType = Spec.TypeEnums.Quantitative,
        sortOrder = Spec.SortOrderEnums.Ascending,
        scale = Spec.Scale(`type` = Option(Spec.ScaleTypeEnums.Log))
      )
      .encodeY(field = "amount",
        title = "Amount of nodes",
        dataType = Spec.TypeEnums.Quantitative,
        scale = Spec.Scale(`type` = Option(Spec.ScaleTypeEnums.Log))
      )
      .encodeColor(
        field = "color",
        title = "Nodes' color",
        dataType = Spec.TypeEnums.Nominal
      )
      .mark(Spec.MarkEnums.Point)
      .show
  }

  def plotRankDistribution(): Unit = {
    val pinkRank = (for (n <- pinks) yield {
      Map("color" -> "pink", "power" -> arcs.count(_.contains(n)))
    }).sortBy(m => -m("power").asInstanceOf[Int])
      .zipWithIndex
      .map(p => p._1 + ("rank" -> p._2))

    val blueRank = (for (n <- blues) yield {
      Map("color" -> "blue", "power" -> arcs.count(_.contains(n)))
    }).sortBy(m => -m("power").asInstanceOf[Int])
      .zipWithIndex
      .map(p => p._1 + ("rank" -> p._2))

    Vegas(description = "Rank power distribution", width = 400, height = 400)
      .withData(pinkRank ++ blueRank)
      .encodeX(field = "rank",
        title = "Rank",
        dataType = Spec.TypeEnums.Quantitative,
        sortOrder = Spec.SortOrderEnums.Ascending,
        scale = Spec.Scale(`type` = Option(Spec.ScaleTypeEnums.Log))
      )
      .encodeY(field = "power",
        title = "Node's power",
        dataType = Spec.TypeEnums.Quantitative,
        scale = Spec.Scale(`type` = Option(Spec.ScaleTypeEnums.Log))
      )
      .encodeColor(
        field = "color",
        title = "Nodes' color",
        dataType = Spec.TypeEnums.Nominal
      )
      .mark(Spec.MarkEnums.Point)
      .show
  }

  plotRankDistribution()
  plotFrequencyDistribution()

}

case class Node(blue: Boolean, id: Int) {
  override def toString: String = s"${if (blue) "b" else "p"}$id"
}

case class Arc(a: Node, b: Node, id: Int) {
  def contains(n: Node): Boolean = a == n || b == n

  override def toString: String = s"y$id"
}