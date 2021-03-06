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

package cc.factorie.variable

import scala.collection.mutable.ArrayBuffer
import cc.factorie.la._


/** A DiscreteVar whose integers 0...N are associated with an categorical objects of type C.
    Concrete implementations include CategoricalVariable and BooleanVariable. 
    @author Andrew McCallum */
trait CategoricalVar[V<:CategoricalValue[C],C] extends DiscreteVar with CategoricalVectorVar[C] with ValueBound[CategoricalValue[C]] with VarWithDomain[CategoricalValue[C]]  {
  def domain: CategoricalDomain[C]
  def value: CategoricalValue[C]
  def categoryValue: C = if (value ne null) value.category else null.asInstanceOf[C]
  override def toString = printName + "(" + (if (categoryValue == null) "null" else if (categoryValue == this) "this" else categoryValue.toString) + ")"
}

/** An abstract variable whose values are CategoricalValues, 
    each corresponding to an integer 0...domain.size, and each associated with a category of type C (usually a String).
    @author Andrew McCallum */
trait MutableCategoricalVar[V<:CategoricalValue[C],C] extends CategoricalVar[V,C] with MutableDiscreteVar[V] {
  def setCategory(newCategory:C)(implicit d: DiffList): Unit = {
    val i = domain.index(newCategory)
    if (i >= 0) set(i)(d)
    else throw new Error("Category not in domain: "+newCategory.toString)
  }
  override def value: V = domain.apply(_value).asInstanceOf[V]
}

/** A MutableDiscreteVar whose integers 0...N are associated with a category of type C (usually a String). 
    @author Andrew McCallum */
abstract class CategoricalVariable[C] extends MutableDiscreteVar[CategoricalValue[C]] with MutableCategoricalVar[CategoricalValue[C],C] {
  def this(initialCategory:C) = {
    this()
    val idx = domain.index(initialCategory)
    if (idx < 0) throw new Error("Initial category not in domain: "+initialCategory.toString)
    _set(idx)
  }
}





// TODO Replace this with Catalog? -akm
// But then we couldn't use syntax like:  PersonDomain.size
// But this doesn't matter any more.

/** An Observation put into an index, and whose value is the Observation variable itself.  
    For example, you can create 10 'Person extends ItemizedObservation[Person]' objects, 
    and upon creation each will be mapped to a unique integer 0..9.
    p1 = new Person; p1.index == 0; p1.categoryValue == p1. 
    @author Andrew McCallum */
trait ItemizedVar[This<:ItemizedVar[This]] extends CategoricalVar[CategoricalValue[This],This] with VarWithConstantValue {
  this: This =>
  def domain: CategoricalDomain[This]
  // Put the variable in the CategoricalDomain and remember it.
  override val value = domain.value(this)
  override def categoryValue = this
}

