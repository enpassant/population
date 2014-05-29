package main

import akka.actor.ActorSystem
import akka.actor.Props
import ga.Algorithm
import ga.Chromosoma
import samples.TicTacToe
import samples.RandomNumbers
import ga.Reaper

object Popu extends App {

  type Evaluate = (Chromosoma) => Chromosoma

  // Create the 'population' actor system
  val system = ActorSystem("population")
  
  // Create the 'reaper' actor
  val reaper = system.actorOf(Props[Reaper], "reaper")
  
  // Create the 'population' actor
  val algorithm1 = system.actorOf(Props(new Algorithm(6000, RandomNumbers.length, 80, RandomNumbers)), "population1")

  // Create the 'population' actor
  val algorithm2 = system.actorOf(Props(new Algorithm(1200, TicTacToe.reducedTables.size, 120, TicTacToe)), "population2")
  
  reaper ! Reaper.WatchMe(algorithm1)
  reaper ! Reaper.WatchMe(algorithm2)
}
