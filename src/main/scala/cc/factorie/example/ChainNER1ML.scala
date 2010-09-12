/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie.example

import scala.io.Source
import java.io.File
import cc.factorie._
import cc.factorie.er._
import cc.factorie.application.LabeledTokenSeqs
import cc.factorie.application.LabeledTokenSeqs.LabeledTokenSeq

object ChainNER1ML {

  // Define the variable classes
  class Token(word: String, labelString: String) extends LabeledTokenSeqs.Token[Label, Token](word) {
    val label = new Label(labelString, this)
  }
  class Label(tag: String, token: Token) extends LabeledTokenSeqs.Label[Token, Label](tag, token)

  // Define the model:
  val model = new Model(
    Foreach[Label] {label => Score(label)},
    Foreach[Label] {label => Score(label.prev, label, label.token)}
    )

  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER1 trainfile testfile")

    // Read training and testing data.
    val trainSentences = LabeledTokenSeq.fromOWPL[Token, Label](Source.fromFile(new File(args(0))), (word, lab) => new Token(word, lab), "-DOCSTART-")
    val testSentences = LabeledTokenSeq.fromOWPL[Token, Label](Source.fromFile(new File(args(1))), (word, lab) => new Token(word, lab), "-DOCSTART-")

    // Get the variables to be inferred
    val trainVariables = trainSentences.map(_.labels)
    val testVariables = testSentences.map(_.labels)
    val allTestVariables = testVariables.flatMap(l => l)

    // Train and test
    println("*** Starting training (#sentences=%d)".format(trainSentences.size))
    val trainer = new LogLinearMaximumLikelihood(model)
    trainer.process(trainVariables)

    val objective = new Model(new LabelTemplate[Label])
    // slightly more memory efficient - kedarb
    println("*** Starting inference (#sentences=%d)".format(testSentences.size))
    testVariables.foreach {variables => new BPInferencer(model).inferTreewiseMax(variables)}
    println("test token accuracy=" + objective.aveScore(allTestVariables))
  }
}