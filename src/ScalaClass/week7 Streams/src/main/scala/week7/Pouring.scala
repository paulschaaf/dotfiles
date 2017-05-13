package week7

class Pouring(capacities: Vector[Int]) {
  // States
  type State = Vector[Int]
  
  //Moves
  trait Action {
    def applyTo(state: State): State
    def roomLeftIn(glass: Int, state: State) = capacities(glass) - state(glass)
  }
  case class Empty(glass: Int) extends Action {
    def applyTo(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Action {
    def applyTo(state: State) = state updated (glass, capacities(glass))
  }
  case class Pour(from: Int, to: Int) extends Action {
    def applyTo(state: State) = {
      val amountMoved = state(from) min roomLeftIn(to, state)
      state updated (from, state(from) - amountMoved) updated (to, state(to) + amountMoved)
    }
  }
  
  val glasses = 0 until capacities.length
  
  val possibleActions =
    (for (g <- glasses) yield Empty(g)) ++
    	(for (g <- glasses) yield Fill(g)) ++
    	(for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))
    	
  // Paths
  class Path(history: List[Action], val endState: State) {
    def takeAction(action: Action) = new Path(action :: history, action applyTo endState)
    override def toString = (history.reverse mkString " ") + "--> " + endState
  }
  
  val initialState = capacities map (_ => 0)    // all glasses are empty
  val initialPath = new Path(Nil, initialState)
  
  def allPathsStartingFrom(
		  paths: Set[Path],
		  explored: Set[State] = Set(initialState)
		  ): Stream[Set[Path]] = {
	  println("starting allPathsStartingFrom with paths set of size: " + paths.size)
	  if (paths.isEmpty) Stream.empty
	  else {
		  val frontier = for {
			  path <- paths
			  nextStep <- possibleActions map path.takeAction
			  if !(explored contains nextStep.endState)  // no cycles
		  } yield nextStep
		  paths #:: allPathsStartingFrom(frontier, explored ++ (frontier map (_.endState))) ++ Stream()
	  }
  }
  
  def allPathsStartingFrom2(
      pathSets: Stream[Set[Path]],
      explored: Set[State] = Set(initialState)
  ): Stream[Set[Path]] = {
    println("starting allPathsStartingFrom with paths set of size: " + pathSets.size)
    if (pathSets.isEmpty) Stream.empty
    else {
      val frontier = for {
        paths <- pathSets
        path <- paths
        nextStep <- possibleActions map path.takeAction
        if !(explored contains nextStep.endState)  // no cycles
      } yield nextStep
      pathSets ++ allPathsStartingFrom2(Stream(frontier.toSet), explored ++ (frontier map (_.endState))) ++ Stream()
    }
  }
  
  val pathSets = allPathsStartingFrom2(Stream(Set(initialPath)))
  
  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}