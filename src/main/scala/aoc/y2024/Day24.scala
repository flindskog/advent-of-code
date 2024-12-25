package aoc.y2024

object Day24 extends Aoc2024("input_24.txt"):

  enum Operation:
    case And, Or, Xor

  case class Connection(input1: String, input2: String, output: String, operation: Operation)
  case class Value(value: Boolean)
  type Part = Connection | Value

  val LazyList(initial, connection) = input.splitByEmptyLine

  val inputRegex = """(\w+): ([0-1])""".r
  val initialValues = initial.map: str =>
    val inputRegex(name, value) = str
    (name, value == "1")

  val connectionRegex = """(\w+) (\w+) (\w+) -> (\w+)""".r
  val connections = connection.map: str =>
    val connectionRegex(input1, operation, input2, output) = str
    val op = operation match {
      case "AND" => Operation.And
      case "OR"  => Operation.Or
      case "XOR" => Operation.Xor
    }
    Connection(input1, input2, output, op)

  val circuit: Map[String, Part] = initialValues.map { (name, value) =>
    name -> Value(value)
  }.toMap ++ connections.map { connection =>
    connection.output -> connection
  }.toMap

  println(circuit)

  def evaluate(name: String, circuit: Map[String, Part]): (Boolean, Map[String, Part]) =
    circuit(name) match
      case value: Value => (value.value, circuit)
      case connection: Connection =>
        val (value1, circuit1) = evaluate(connection.input1, circuit)
        val (value2, circuit2) = evaluate(connection.input2, circuit1)
        val result = connection.operation match
          case Operation.And => value1 && value2
          case Operation.Or  => value1 || value2
          case Operation.Xor => value1 ^ value2
        (result, circuit2.updated(connection.output, Value(result)))

  val outputs = circuit.keys
    .filter(_.startsWith("z"))
    .toList
    .sorted
    .foldLeft((circuit, List.empty[Boolean])) { case ((circuit, outputs), name) =>
      val (value, newCircuit) = evaluate(name, circuit)
      (newCircuit, value :: outputs)
    }
    ._2

  val result = java.lang.Long.parseLong(outputs.map(v => if v then "1" else "0").mkString, 2)

  println(result) // 59336987801432
