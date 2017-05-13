class WaterPouringProblem(val capacities: Vector[Int]) {
  type Glass = Int
  type State = Vector[Glass]

  trait Action {
    def applyTo(state: State)
    def roomLeftIn(glass: Glass, state: State) = capacities(glass) - state(glass)
    
  }
  case class Empty(glass: Glass) extends Action {
    def applyTo(state: State) = state.updated(glass, 0)
  }
  case class Fill(glass: Glass) extends Action {
    def applyTo(state: State) = state.updated(glass, capacities(glass))
  }
  case class PourInto(fromGlass: Glass, toGlass: Glass) extends Action {
    def roomLeft(state: State) = capacities(toGlass) - state(toGlass)
    def applyTo(state: State) = {
      val transferAmount = roomLeft(state) min state(fromGlass)
      state.updated(fromGlass, state(fromGlass) - transferAmount).
      		updated(toGlass, state(toGlass) + transferAmount)
    }
  }
  
  val glasses = 0 until capacities.length
  val allPossibleActions =
    (for (glass <- glasses) yield Empty(glass)) ++
  	(for (glass <- glasses) yield Fill(glass)) ++
  	(for (from <- glasses; to <- glasses; if from != to) yield PourInto(from, to))
  	
  class ActionList {
    
  }
    
  def movesFollowing(states: Stream[State], explored: Set[State]): Stream[(Set[State], Set[State])) = {
    val frontier = for {
      state <- states
      nextState <- allPossibleActions.map(_.applyTo(state))
      if !explored.contains(nextState)
    } yield {
      explored
    }nextState
  }
}