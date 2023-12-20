package aoc.y2023

import aoc.utils.Math

import scala.annotation.{tailrec, targetName}

object Day20 extends Aoc2023("input_20.txt"):
  enum ModuleType:
    case Conjunction, FlipFlop, Broadcaster

  case class Module(name: String, moduleType: ModuleType, inputs: Set[String], outputs: Set[String])

  val pulseData = input.map { line =>
    val regex = """(.*) -> (.*)""".r
    line match
      case regex(input, output) =>
        val (inputType, name) = input.charAt(0) match
          case '%' => (ModuleType.FlipFlop, input.substring(1))
          case '&' => (ModuleType.Conjunction, input.substring(1))
          case _   => (ModuleType.Broadcaster, input)

        val outputs = output.split(", ").toSet
        (name, (inputType, outputs))
  }.toMap

  val inputs = pulseData.map { case (name, (_, outputs)) =>
    outputs.map(_ -> name)
  }.flatten
    .groupBy(_._1)
    .map((output, inputs) => output -> inputs.map(_._2).toSet)
    .withDefaultValue(Set.empty)

  val modules = pulseData.map { case (name, (inputType, outputs)) =>
    (name, Module(name, inputType, inputs(name), outputs))
  }.withDefault(name => Module(name, ModuleType.Broadcaster, Set.empty, Set.empty))

  enum Pulse:
    case High, Low

  enum OnOff:
    case On, Off

  sealed trait ModuleState:
    def process(input: String, pulse: Pulse): (Option[Pulse], ModuleState)

  case class FlipFlopState(state: OnOff = OnOff.Off) extends ModuleState:
    override def process(input: String, pulse: Pulse): (Option[Pulse], ModuleState) =
      (pulse, state) match
        case (Pulse.High, _)        => (None, this)
        case (Pulse.Low, OnOff.Off) => (Some(Pulse.High), FlipFlopState(OnOff.On))
        case (Pulse.Low, OnOff.On)  => (Some(Pulse.Low), FlipFlopState(OnOff.Off))

  case class ConjunctionState(states: Map[String, Pulse]) extends ModuleState {
    override def process(input: String, pulse: Pulse): (Option[Pulse], ModuleState) =
      val newStates = states.updated(input, pulse)
      val newPulse  = if newStates.values.forall(_ == Pulse.High) then Pulse.Low else Pulse.High
      (Some(newPulse), ConjunctionState(newStates))
  }

  object ConjunctionState {
    def apply(inputs: Set[String]): ConjunctionState =
      ConjunctionState(inputs.map(_ -> Pulse.Low).toMap)
  }

  case object NoState extends ModuleState:
    override def process(input: String, pulse: Pulse): (Option[Pulse], ModuleState) =
      (Some(pulse), this)

  type MachineState = Map[String, ModuleState]

  val initialMachineState = modules.values.map { module =>
    val state = module.moduleType match
      case ModuleType.FlipFlop    => FlipFlopState()
      case ModuleType.Conjunction => ConjunctionState(module.inputs)
      case ModuleType.Broadcaster => NoState

    module.name -> state
  }.toMap.withDefault(_ => NoState)

  case class BeamCount(highs: Long = 0, lows: Long = 0) {
    @targetName("plusPulse")
    def +(pulse: Pulse, amount: Int = 1): BeamCount = pulse match
      case Pulse.High => copy(highs = highs + amount)
      case Pulse.Low  => copy(lows = lows + amount)

    @targetName("plusResult")
    def +(result: BeamCount): BeamCount = BeamCount(highs + result.highs, lows + result.lows)

    def product: Long = highs * lows
  }

  @tailrec
  def process(
      visitingQueue: List[(String, String, Pulse)],
      machineState: MachineState,
      probes: Map[String, BeamCount] = Map.empty,
      totalBeams: BeamCount = BeamCount()
  ): (Map[String, BeamCount], BeamCount, MachineState) = visitingQueue match
    case Nil => (probes, totalBeams, machineState)
    case (from, to, pulse) :: tail =>
      val module               = modules(to)
      val (newPulse, newState) = machineState(to).process(from, pulse)
      val newStates            = machineState.updated(to, newState)
      val newQueue = newPulse match
        case Some(value) =>
          tail ::: module.outputs.map(to => (module.name, to, value)).toList
        case None =>
          tail
      val newProbes = probes.get(to) match
        case Some(probe) => probes.updated(to, probe + pulse)
        case None        => probes
      process(newQueue, newStates, newProbes, totalBeams + pulse)

  val startQueue = List(("button", "broadcaster", Pulse.Low))

  def processLoop(probes: Map[String, BeamCount] = Map()): LazyList[(Map[String, BeamCount], BeamCount)] = {
    def loop(state: MachineState): LazyList[(Map[String, BeamCount], BeamCount)] =
      process(startQueue, state, probes) match
        case (probes, result, newStates) => (probes, result) #:: loop(newStates)
    loop(initialMachineState)
  }

  val total = processLoop().take(1000).map(_._2).reduce(_ + _)
  println(total.product) // 737679780

  // Only module pointing to rx is ft, which is a conjunction of qh, vz, bq, lt
  // So check toggle frequency of those modules, when they all are high, rx is low
  val probes = Map(
    "qh" -> BeamCount(),
    "vz" -> BeamCount(),
    "bq" -> BeamCount(),
    "lt" -> BeamCount()
  )

  val cycles = processLoop(probes)
    .zipWithIndex
    .filter { case ((probes, _), _) => probes.values.exists(_.lows > 0) }
    .take(probes.size * 2)
    .map { case ((probes, _), index) =>
      val (name, _) = probes.find(_._2.lows > 0).get
      (name, index)
    }
    .toList
    .groupBy(_._1)
    .map((_, list) => list.map(_._2.toLong).sorted.reverse)

  val lcm = Math.lcm(cycles.map(_.reduce(_ - _)).toList)
  println(lcm) // 227411378431763
