package aoc.solutions

import aoc.AocTest

import scala.collection.mutable
import scala.collection.immutable.Queue
import scala.annotation.tailrec
import aoc.*

class Day20 extends AocTest:
  import Module.*

  enum Pulse:
    case Low, High

  case class Result(module: Module, pulse: Option[Pulse])

  sealed trait Module:
    def handle(sourceName: String, pulse: Pulse): Result

  object Module:
    case object Broadcast extends Module:
      def handle(sourceName: String, pulse: Pulse): Result =
        Result(Broadcast, Some(pulse))
    case class FlipFLop(state: Boolean = false) extends Module:
      def handle(sourceName: String, pulse: Pulse): Result =
        pulse match
          case Pulse.High => Result(this, None)
          case Pulse.Low  => Result(FlipFLop(!state), Some(if state then Pulse.Low else Pulse.High))
    case class Conjunction(memory: Map[String, Pulse] = Map.empty) extends Module:
      def handle(sourceName: String, pulse: Pulse): Result =
        val newMemory = memory.updated(sourceName, pulse)
        val output    = if newMemory.forall(_._2 == Pulse.High) then Pulse.Low else Pulse.High
        Result(Conjunction(newMemory), Some(output))
  end Module

  case class PulseTrip(from: String, to: String, pulse: Pulse)
  case class Simulation(c: Circuit, trips: Vector[PulseTrip])
  case class Circuit(modules: Map[String, Module], outputs: Map[String, Seq[String]], inputs: Map[String, Set[String]]):

    def simulation: Simulation =
      @tailrec
      def inner(queue: Queue[PulseTrip], state: Map[String, Module], pulses: Vector[PulseTrip]): Simulation =
        if queue.isEmpty then Simulation(this.copy(modules = state), pulses)
        else
          val (trip, rest)               = queue.dequeue
          val PulseTrip(from, to, pulse) = trip
          state.get(to) match
            case None => inner(rest, state, pulses.appended(trip))
            case Some(module) =>
              val toOutputs                   = outputs(to)
              val Result(newModule, newPulse) = module.handle(from, pulse)
              val newState                    = state.updated(to, newModule)
              val newPulses = newPulse.fold(List.empty[PulseTrip])(p =>
                for o <- toOutputs
                yield PulseTrip(to, o, p)
              )
              inner(rest.enqueue(newPulses), newState, pulses :+ trip)
          end match
        end if
      end inner
      inner(Queue(PulseTrip("button", "broadcaster", Pulse.Low)), modules, Vector.empty)
    end simulation

    def productOfCount(n: Int): Int =
      val (_, pulseCount) = (0 until n)
        .foldLeft((this, Map.empty[Pulse, Int].withDefaultValue(0))) { case ((circuit, count), _) =>
          val Simulation(newCircuit, pulses) = circuit.simulation
          val newCount = pulses.foldLeft(count) { case (count, PulseTrip(_, _, pulse)) =>
            count.updated(pulse, count(pulse) + 1)
          }
          (newCircuit, newCount)
        }
      pulseCount.values.product
    end productOfCount

    def count(name: String): Long =
      @tailrec
      def loop(circuit: Circuit, todo: Set[String], found: Map[String, Int], i: Int): Map[String, Int] =
        if todo.isEmpty then found
        else
          val Simulation(newCircuit, pulses) = circuit.simulation
          val (newTodo, newFound) = pulses.foldLeft((todo, found)) {
            case ((todo, found), PulseTrip(from, _, Pulse.High)) if todo.contains(from) =>
              (todo - from, found.updated(from, i))
            case (acc, _) => acc
          }
          loop(newCircuit, newTodo, newFound, i + 1)

      val named = inputs(name)
      val todo  = inputs(named.head).toSet
      val found = loop(this, todo, Map.empty[String, Int], 1)
      found.values.map(BigInt(_)).reduce(_.lcm(_)).longValue
    end count
  end Circuit

  def parseModule(s: String): (String, (Module, Seq[String])) = s match
    case s"$moduleStr -> $outputsStr" =>
      val (name, module) = moduleStr match
        case s"broadcaster" => ("broadcaster", Broadcast)
        case s"%$name"      => (name, FlipFLop())
        case s"&$name"      => (name, Conjunction())
      val outputs = outputsStr.split(", ").toSeq
      (name, (module, outputs))

  def parseCircuit(input: String): Circuit =
    val moduleOutputs = input.linesIterator.map(parseModule).toMap
    val modules       = moduleOutputs.view.mapValues(_._1).toMap
    val outputs       = moduleOutputs.view.mapValues(_._2).toMap

    val inputs = (for
      (module, outputs) <- outputs.iterator
      output            <- outputs
    yield module -> output).toSeq.groupMapReduce(_._2)(p => Set(p._1))(_ ++ _)

    val initializedModules = modules.map({
      case (module, Conjunction(_)) =>
        module -> Conjunction(inputs(module).map(_ -> Pulse.Low).toMap)
      case other => other
    })

    Circuit(initializedModules, outputs, inputs)
  end parseCircuit

  val example = parseCircuit("""broadcaster -> a
                               |%a -> inv, con
                               |&inv -> b
                               |%b -> con
                               |&con -> output""".stripMargin)

  test("part1"):
    assertEquals(example.productOfCount(1000), 11687500)
    assertEquals(parseCircuit(input.mkString("\n")).productOfCount(1000), 866435264)

  test("part2"):
    assertEquals(parseCircuit(input.mkString("\n")).count("rx"), 229215609826339L)

end Day20
