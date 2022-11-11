/*
 * Copyright 2022 Foldables
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.foldables.sinum

import cats.data.ReaderT
import cats.implicits.*
import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple:

  val input = """(let (x 42) x)"""
  val value = Eval.fromParsed(Parser.parse(input))

  def run: IO[Unit] =
    value.flatMap(Eval.run).run(Eval.BasicEnv).map(_.show).flatMap(IO.println)
 
