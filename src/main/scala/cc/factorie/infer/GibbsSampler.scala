package cc.factorie.infer

import scala.collection.mutable.{ArrayBuffer,HashMap}
import cc.factorie.directed.{DirectedModel, DirectedFactor}
import cc.factorie.variable._
import cc.factorie.model.{Model, Factor}
import cc.factorie._

/** Sample a value for a single variable.  This sampler works in one of two ways:  
    If the model is a DirectedModel, then sampling is performed based on a suite a handlers
    selected according to the variable type and its neighboring factors.
    If the model is not a DirectedModel, then the variable should inherit from IterableSettings
    which is used to create a list of Proposals with alternative values. */
class GibbsSampler(val model:Model, val objective:Model = null)(implicit val random: scala.util.Random) extends ProposalSampler[Var] {
  type V = Var
  private var _handlers: Iterable[GibbsSamplerHandler] = null 
  def defaultHandlers = GibbsSamplerDefaultHandlers
  def setHandlers(h:Iterable[GibbsSamplerHandler]): Unit = _handlers = h
  def handlers: Iterable[GibbsSamplerHandler] = if (_handlers eq null) defaultHandlers else _handlers
  val cacheClosures = true
  def closures = new HashMap[V, GibbsSamplerClosure]
  val doProcessByHandlers = model.isInstanceOf[DirectedModel]
  override def process1(v:V): DiffList = if (doProcessByHandlers) processByHandlers(v) else processProposals(proposals(v))
  def processByHandlers(v:V): DiffList = {
    val d = newDiffList
    // If we have a cached closure, just use it and return
    if (cacheClosures && closures.contains(v)) { closures(v).sample(d); return d }
    // Get factors, in sorted order of the their classname
    val factors = model.factors(v).toSeq.sortWith((f1:Factor,f2:Factor) => f1.getClass.getName < f2.getClass.getName).toSeq
    var done = false
    val handlerIterator = handlers.iterator
    while (!done && handlerIterator.hasNext) {
      val closure = handlerIterator.next.sampler(v, factors, this)
      if (closure ne null) {
        done = true
        closure.sample(d)
        if (cacheClosures) closures(v) = closure
      }
    }
    if (!done) throw new Error("GibbsSampler: No sampling handler found for "+factors)
    d
  }

  def proposals(v:V): Seq[Proposal] = model match {
    case m:DirectedModel => throw new Error("Not yet implemented")
    case m:Model => v match {
      case v:DiscreteVariable => proposals(v)
      case v:Var with IterableSettings => proposals(v.settings)
    } 
  }
  def proposals(si:SettingIterator): Seq[Proposal] = {
    val dmodel = model: Model
    val dobjective = objective: Model
    val props = new ArrayBuffer[Proposal]()
    while (si.hasNext) {
      val d = si.next()
      val (m,o) = d.scoreAndUndo(dmodel,dobjective)
      props += new Proposal(d, m, o, m/temperature)
    }
    props
  }
  // Special case for a bit more efficiency
  def proposals(dv:DiscreteVariable): Seq[Proposal] = {
    var i = 0; val len = dv.domain.size
    val result = new ArrayBuffer[Proposal](len)
    while (i < len) {
      val diff = new DiffList
      dv.set(i)(diff)
      val (modelScore, objectiveScore) = diff.scoreAndUndo(model, objective)
      result += new Proposal(diff, modelScore, objectiveScore, modelScore/temperature)
      i += 1
    }
    result
  }
}

object GibbsSamplerDefaultHandlers extends ArrayBuffer[GibbsSamplerHandler] {
  this += GeneratedVarGibbsSamplerHandler
}



trait GibbsSamplerHandler {
  def sampler(v:Var, factors:Seq[Factor], sampler:GibbsSampler): GibbsSamplerClosure
}
trait GibbsSamplerClosure {
  def sample(implicit d:DiffList = null): Unit
}


object GeneratedVarGibbsSamplerHandler extends GibbsSamplerHandler {
  class Closure(val variable:MutableVar[_], val factor:DirectedFactor)(implicit random: scala.util.Random) extends GibbsSamplerClosure {
    def sample(implicit d:DiffList = null): Unit = variable.set(factor.sampledValue.asInstanceOf[variable.Value])
  }
  def sampler(v:Var, factors:Seq[Factor], sampler:GibbsSampler): GibbsSamplerClosure = {
    factors match {
      case List(factor:DirectedFactor) => {
        v match {
          case v:MutableVar[_] => new Closure(v, factor)(sampler.random)
        }
      }
      case _ => null
    }
  }
}

// TODO Create MixtureChoiceGibbsSamplerHandler, IterableSettingsGibbsSamplerHandler
