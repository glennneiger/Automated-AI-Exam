#' @keywords internal
q1=function() {
  cat("Explain the two ways a planning graph can be used to provide a ",
      "heuristic for A*. (2 marks)\n",sep="")
}
#' @keywords internal
a1=function() {
  cat("1. The max-level heuristic: The number of levels between the initil state layer (given by the state whose heuristic value is being estimated) and the first state layer where all goal literals present.\n")
  cat("2. The set-level heuristic: The number of levels between the initil state layer (given by the state whose heuristic value is being estimated) and the first state layer where all goal literals present AND there are no mutexs between any pair of goal literals.\n")
}
#' @keywords internal
q2=function() {
  cat("What is PDDL? Explain all components of a PDDL problem. Be as ",
      "precise and concise as possible.  (4 marks)\n",sep="")
}
#' @keywords internal
a2=function() {
  cat("PDDL stands for planning domain definition language. It is used to ",
    "specify planning problems in a way that is easy to solve using ",
    "search-based techniques. It works with database semantics, such ",
    "that all objects are uniquely named.\n",sep="")
  cat("\nThe components of PDDL are:\n")
  cat("  1. States. These are lists of positive literals (where literals ",
      "are logically atomic statements - i.e. without 'and', 'or', 'if... ",
      "then', and 'not'). Negation is by ommission.\n",sep="")
  cat("  2. Actions. These contain lists of preconditions and effects. These ",
    "lists contain positive and negative literals and show what is required ",
    "to be true in a state for an action to be permitted, and what changes ",
    "are effected when an action is performed. Actions play the role of ",
    "transitions between states.\n")
  cat("  3. Goal state specification. This is a list of positive and ",
      "negative literals specifying what must be true for a state to be ",
      "a goal state.\n")
  cat("In addition, for a PDDL problem to be fully specified an initial state ",
      "must be given.\n")
}
#' @keywords internal
q3=function(){
  cat("Explain how the goals of AI are different in industry and academia compared with in computer games. (1 mark)\n")
}
#' @keywords internal
a3=function(){
  cat("AI in industry and academic normally focuses on providing the optimal solution to problems posed. AI in computer games, on the other hand, normally uses AI to produce competitive or realistic characters and opponents as an aid to entertainment. Computer games do not want AI opponents to be impossible to beat!\n")
}
#' @keywords internal
q4=function(){
  cat("Under what conditions could a depth-first search FAIL to find a solution (in a finite search space with at most a single edge between any two nodes)? (1 mark)")
}
#' @keywords internal
a4=function(){
  cat("When the search space contains loops.")
}
#' @keywords internal
q5=function(){
  cat("Explain iterated deepening. (2 marks)\n")
}
#' @keywords internal
a5=function(){
  cat("Iterated deepening performs a depth first search to a designated depth controlled by a parameter t. ")
  cat("States at this depth treated as if they had no outgoing transitions. ")
  cat("The terminal depth parameter t is initially set to 1. If a depth constrained search terminates without finding the goal state, the terminal depth parameter is increased (typically by 1) and a new search is begun.")
}
#' @keywords internal
q6=function(){
  cat("Greedy Hill Climb suffers from the problem of local optima. Name and provide a brief explanation of three alternative local search strategies covered in this course that attempt to overcome or minimize this problem. (3 marks)\n")
}
#' @keywords internal
a6=function(){
  cat("Any three of these four answers is correct:\n")
  cat("  1. Greedy Hill Climb with random restarts. Here the greedy hill climb is run repeatedly from random initial positions.\n")
  cat("  2. Simulated Annealing. At each step a random neighbor of the current state is selected. If the neighbor has a higher fitness, the transition is made. ")
  cat(    "If not, the transition will be made with a probability equal to the neighbors fitness divided by the current nodes fitness multiplied by a temperature parameter. ")
  cat(    "The temperature parameter is initialized with the value 1 and is then reduced to 0 following a cooling schedule.\n")
  cat("  3. Local Beam Search. Multiple initial nodes are selected, each known as a particle. Where we have n particles, at each search step all neighbors and current nodes of these n particles are placed in a candidate set, and the particles move to the n nodes in this set with the highest fitness values.\n")
  cat("  4. Stochastic Local Beam Search. As local beam search, except that particles move to nodes in the candidate set probabilistically. Typically, a particle can move to an unoccupied candidate node with probability equal to the fitness of that node divided by the sum of the fitness values of all unoccupied nodes in the candidate set. Initially, all nodes in the candidate set are considered unoccupied (including current nodes). Once one particle moves to a node in the candidate set it is considered occupied.\n")
}
#' @keywords internal
q7=function(){
  cat("Give a basic explanation (as per what was discussed in the course) of the bias and variance components of expected error and their relationship to model complexity. (3 marks)\n")
}
#' @keywords internal
a7=function(){
  cat("The bias component of expected error is the expected error arising from the model being too simple to model the system well. The variance component of expected error is (in a simplified sense) the expected error arising from the model overfitting to data. As model complexity increases, the bias component of expected error decreases and the variance component increases.")
}
#' @keywords internal
q8=function(){
  cat("What is the purpose of including randomness in the action-deciding process of a reinforcement learning system? (1 mark)")
}
#' @keywords internal
a8=function(){
  cat("Including randomless allows the system to occasionally chose to make actions that it does not consider optimal. This permits it to explore the effect of such actions on the environment and learn more about the system it is controlling.\n")
}
#' @keywords internal
q9=function(){
  cat("Assume you have a GAN where the discriminator network is a simple binary (Genuine/Fake) classifier. Briefly explain how the generator network is trained. (2 marks)\n")
}
#' @keywords internal
a9=function(){
  cat("During training, the generator network will generates fake data cases from random noise based on its parameters. These are passed to the discriminator for classification. The discriminator will give a probability value to the image being genuine/fake. To train the generator network, we take the probability the image is fake as the loss function (we want to minimize the probability the discriminator assigns to a generated image being fake), and proceed to take the derivatives of this with regard to the parameters of the generator network. These derivatives 'pass through' the discriminator network's parameters by the chain rule of derivation, but the discriminators weights are not adjusted.\n")
}
#' @keywords internal
q10=function(){
  cat("Explain the steps involving the memory vector in a pass through a LSTM layer at time t. Mention what is done to the memory vector (non-mathematically) and/or what the memory vector is used for in each of these steps. Make reference to the input at time t, and the outputs of time t-1 and t. (2 marks)")
}
#' @keywords internal
a10=function(){
  cat("The joint vector of the input at time t and the output at time t-1 is used (in conjunction with the weights of the LSTM unit) to update the memory vector. This has three steps: (i) The joint vector determines which elements in the memory vector should be 'forgotten', and to what degree; (ii) The joint vector determines which elements of the memory vector should be updated, and to what degree; and (iii) The joint vector determines the values that should be used to update the memory vector. This updated memory vector is then used in conjunction with the joint vector to determine the output of the LSTM at time t.")
}

#' @keywords internal
prepareTextQuestions=function() {
  list(q1,q2,
       #q3,                   # Question Three is excluded as now in supplementary lecture
       q4,q5,q6,q7,q8,q9,q10)
}
#' @keywords internal
prepareTextAnswers=function() {
  list(a1,a2,
       #a3,                   # Question Three is excluded as now in supplementary lecture
       a4,a5,a6,a7,a8,a9,a10)
}
#' @keywords internal
prepareTextQuestionMarks=function(){
  c(2,4,
      #1,                   # Question Three is excluded as now in supplementary lecture
      1,2,3,3,1,2,2)
}
#' @keywords internal
prepareTextQuestionHeadings=function(){
  c("Planning Graphs","PDDL",
      #"AI in Computer Games",                   # Question Three is excluded as now in supplementary lecture
      "Depth-First Search","Iterated Deepening","Local Search","Bias-Variance","Reinforcement Learning","GANs","LSTMs")
}
#' @keywords internal
getTextQuestionMarks=function(seed,n){
  set.seed(seed)
  marks=prepareTextQuestionMarks()
  q_=sample(length(marks),n)
  sum(marks[q_])
}
#' makeTextQuestions
#'
#' Randomly select text questions from the available set.
#' @param seed The random seed to use to generate the values. If NA, they are generated from the
#' current seed.
#' @param answer Should the answer(s) be outputed to console
#' @param latex Should latex script be outputed. This is for use with the makeExam function.
#' @param n The number of questions to be selected
#' @return NULL
#' @export
makeTextQuestions=function(seed=NA,answer=T,latex=F,n=1) {
  if (!is.na(seed))
    set.seed(seed)
  qs=prepareTextQuestions()
  as=prepareTextAnswers()
  headings=prepareTextQuestionHeadings()
  q_=sample(length(qs),n)
  first=T
  for (q in q_) {
    if (latex) {
      cat("\\clearpage\n")
      cat("\\section{",headings[q],"}\n\n")
      qs[[q]]()
    }
    else {
      if (first)
        first=F
      else
        readline("Press Enter to see the next question...")
      qs[[q]]()
    }
    if (answer) {
      readline("Press Enter to see a sample answer...")
      as[[q]]()
    }
  }
}
#' makeTextQuestions2
#'
#' Select specified text questions.
#' @param questions The questions to be selected. This should be an integer vector containing elements
#' between 1 and 9 inclusive.
#' @param answer Should the answer(s) be outputed to console
#' @return NULL
#' @export
makeTextQuestions2=function(questions,answer=T) {
  qs=prepareTextQuestions()
  as=prepareTextAnswers()
  headings=prepareTextQuestionHeadings()
  first=T
  for (q in questions) {
    if (first)
      first=F
    else
      readline("Press Enter to see the next question...")
    qs[[q]]()
    if (answer) {
      readline("Press Enter to see a sample answer...")
      as[[q]]()
    }
  }
}
