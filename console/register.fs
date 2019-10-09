module Prisoner.Register

let registerAll() =
  AlwaysCollaborateStrategy.register()
  AdvancedStrategy.register()
  CopyStrategy.register()
  RandomStrategy.register()
