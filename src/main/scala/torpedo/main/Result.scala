/**
  * Copyright (C) 2017  Alessandro M. Rizzi <alessandromaria.rizzi@polimi.it>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Affero General Public License as
  * published by the Free Software Foundation, either version 3 of the
  * License, or (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Affero General Public License for more details.
  *
  * You should have received a copy of the GNU Affero General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
package torpedo.main

sealed abstract class Result[+T] {

  def map[S](f : T => S) : Result[S];

  def flatMap[S](f : T => Result[S]) : Result[S];

  def foreach(f : T => Unit);

}

case class Success[T](value : T) extends Result[T] {

  override def map[S](f: T => S): Result[S] = Success(f(value));

  override def flatMap[S](f: T => Result[S]): Result[S] = f(value);

  override def toString: String = value.toString;

  override def foreach(f: T => Unit): Unit = f(value);

}

case object NoError extends NoValue {

  override def map[S](f: Null => S): Result[S] = Success(f(null));

  override def flatMap[S](f: Null => Result[S]): Result[S] = f(null);

  override def foreach(f: Null => Unit): Unit = {};

  override def toString: String = "";

}

sealed abstract class Failure extends Result[Nothing] {

  override def map[S](f: Nothing => S): Result[S] = this;

  override def flatMap[S](f: Nothing => Result[S]): Result[S] = this;

  override def foreach(f: Nothing => Unit): Unit = {};

}

case class InvalidFileFailure(filename : String) extends Failure{
  override def toString: String = "Invalid file \"" + filename + "\"!";
}

case class FileNotFoundFailure(filename : String) extends Failure{
  override def toString: String = "File \"" + filename + "\" not found!";
}

case class WriteFailure(filename : String) extends Failure{
  override def toString: String = "Write error on \"" + filename + "\"!";
}

case object ModelCheckerFailure extends Failure{
  override def toString: String = "Error occurred in external model checker! Is Docker daemon running?";
}

case object ProofFailure extends Failure{
  override def toString: String = "Error occurred in external prover!";
}