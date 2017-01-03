package main

import akka.actor.ActorSystem
import akka.actor.Props
import ga.Algorithm
import ga.Chromosoma
import samples.TicTacToe
import samples.RandomNumbers
import ga.Reaper
import samples.Painter

object Popu extends App {

  type Evaluate = (Chromosoma) => Chromosoma

  val system = ActorSystem("population")

  val reaper = system.actorOf(Props[Reaper], "reaper")

  //val algorithm1 = system.actorOf(
		//Props(
			//new Algorithm(
				//6000,
				//RandomNumbers.length,
				//80,
				//RandomNumbers)),
		//"population1")

  val algorithm2 = system.actorOf(
		Props(
			new Algorithm(
				1200,
				TicTacToe.reducedTables.size,
				120,
				TicTacToe)),
		"population2")

  //val algorithm3 = system.actorOf(
		//Props(
			//new Algorithm(
				//1200,
				//Painter.count * Painter.itemCount,
				//1000, Painter)),
		//"population3")

  //reaper ! Reaper.WatchMe(algorithm1)
  reaper ! Reaper.WatchMe(algorithm2)
  //reaper ! Reaper.WatchMe(algorithm3)
}
