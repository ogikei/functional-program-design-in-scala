package week2

class Pouring(capacity: Vector[Int]) {

  // State
  type State = Vector[Int]
  val initialState = capacity map (x => 0)

  // Moves
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State) = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State) = {
      // state(from) is capacity for current glass to have.
      // capacity(to) is capacity of poured glass.
      // state(to) is capacity for poured glass to have.
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if (from != to)) yield Pour(from, to))

  // Paths
  // Path represent pouring pattern.
  // e.g. Fill(0) Pour(0,1) --> Vector(0, 4)
  // history is List(Fill(0), Pour(0, 1)) and want to output State(Vector(0, 4)).
  class Path(history: List[Move], val endState: State) {
    // def endState: State = trackState(history)
    // private def trackState(xs: List[Move]): State = xs match {
    // case Nil => initialState
    // case move :: xs1 => move change trackState(xs1)

    // Following is more elegant implement.
    // This function represents state.
    // e.g. Fill(0) Pour(0,1) --> Vector(0, 4)
    // -> Fill(0) change Pour(0, 1)

    // but following endState function is inefficient, so don't use it.
    // def endState: State = (history foldRight initialState)(_ change _)
    def extend(move: Move) = new Path(move :: history, move change endState)

    override def toString = (history.reverse mkString " ") + " --> " + endState
  }

  val initialPath = new Path(Nil, initialState)

  // e.g. Set(Fill(0), Pour(0,1))
  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        // try all move patterns
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  // target is glasses's water amount.
  // e.g. in the case of 6
  // Stream(Fill(1) Pour(1,0) Empty(0) Pour(1,0) Fill(1) Pour(1,0) --> Vector(4, 6), ?)
  // e.g. in the case of 7
  // Stream(Fill(1) --> Vector(0, 7), ?)
  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      // whether containing target state(e.g. Vector(4, 6))
      if path.endState contains target
    } yield path

}
