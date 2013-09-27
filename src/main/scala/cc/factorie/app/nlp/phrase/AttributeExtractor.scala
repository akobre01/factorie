package cc.factorie.app.nlp.phrase

import cc.factorie.app.nlp.ner.ConllNerDomain
import cc.factorie.app.nlp.TokenSpan
import cc.factorie.app.nlp.{Section, Document, Token}
import cc.factorie.app.nlp.lexicon.ClasspathResourceLexicons
import collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.phrase.RelationNames.RelationName


/**
 * Created with IntelliJ IDEA.
 * User: akobren
 * Date: 9/27/13
 * Time: 2:14 PM
 *
 * This file is a modification of the attribute extraction that was used in TACKBP 2013.
 * The result of attribute extraction is a seq of Attribute relations. Each NounPhrase here should
 * add the relevant attribute relations
 * */

trait AttributeExtractor {

  def extract(np: NounPhrase): Seq[AttributeRelationSpan]

  def notProper(token: Token): Boolean = token.posLabel.categoryValue != "NNP" && token.posLabel.categoryValue != "NNPS"

  def isNounButNotProper(token: Token): Boolean =
    token.posLabel.isNoun && (token.posLabel.categoryValue != "NNP" && token.posLabel.categoryValue != "NNPS")

  /* TODO AK: get the strings out of the following function */
  def isPerOrUkn(np: NounPhrase): Boolean = {
    np.attr[NounPhraseEntityType].categoryValue == "PERSON" || np.attr[NounPhraseEntityType].categoryValue == "MISC"
  }

  def isPlural(token: Token): Boolean = token.posLabel.categoryValue.endsWith("S")

  /* TODO AK: once we add dates we can use the following */
  //def hasDocDate(dm: NounPhrase) = dm.section.attr.contains[DocumentDate]

  /* Handles looking at the token and extraction the correct phrase centered on the token
   * This code is really only tuned for titles */
  def createPhraseAttributeRelation(np: NounPhrase, token: Token, rel: RelationName): AttributeRelationSpan = {

    if (token.hasPrev && token.prev.string == "-" && token.prev.hasPrev && token.prev.prev.parseParent == token && isNounButNotProper(token.prev.prev))
      new AttributeRelationSpan(np.section, token.prev.prev.positionInSection, 3, rel)
    else if (token.hasPrev && token.prev.string == "of" && token.prev.hasPrev && isNounButNotProper(token.prev.prev))
      new AttributeRelationSpan(np.section, token.prev.prev.positionInSection, 3, rel)
    else if (token.hasNext && token.next.string == "of" && token.next.hasNext && isNounButNotProper(token.next.next))
      new AttributeRelationSpan(np.section, token.positionInSection, 3, rel)
    else if (token.hasPrev && isNounButNotProper(token.prev))  new AttributeRelationSpan(np.section, token.prev.positionInSection, 2, rel)
    else new AttributeRelationSpan(np.section, token.positionInSection, 1, rel)
  }

  def preprocess(d: Document) {}

  def process(doc: Document) = {

    preprocess(doc)

    /* TODO AK: I need to be able to get a list of noun phrases from the document
       We need something like the following

    for (np <- nps:Seq[NounPhrase] {
      val attRelSpan = extract(np)
      np.attr[AttributeRelationSpan] += attRelSpan
    }
  */
  }
}

class AttributeRelationSpan(section: Section, start: Int, len: Int, val relName: RelationName) extends TokenSpan(section, start, len)

object RelationNames {

  val names: collection.mutable.HashMap[String, RelationName] = new collection.mutable.HashMap

  class RelationName(val name: String, val disallowed: Set[ConllNerDomain.CategoryType] = Set.empty,
                     val arg2Type: ConllNerDomain.CategoryType = "MISC") {

    /* TODO AK: get these strings from a canonical place */
    val PER  = "PERSON"
    val ORG  = "ORG"
    val GPE  = "GPE"
    val MISC = "MISC"
    assert(!names.contains(name))
    names(name) = this

    val argType: String =
      if (name.startsWith("per"))      PER
      else if (name.startsWith("org")) ORG
      else if (name.startsWith("gpe")) GPE
      else MISC

    def strValue = name

    override def toString = name
  }

  /* This is the only relation type we're using right now */
  val perTitle = new RelationName("per:title")

  /*
  val perAltNames = new RelationName("per:alternate_names")
  val perDateOB = new RelationName("per:date_of_birth")
  val perAge = new RelationName("per:age")
  val perDateOD = new RelationName("per:date_of_death")
  val perCauseOD = new RelationName("per:cause_of_death")
  val perReligion = new RelationName("per:religion")
  val perCharges = new RelationName("per:charges")

  val orgAltNames = new RelationName("org:alternate_names")
  val orgPolitic = new RelationName("org:political_religious_affiliation")
  val orgDateFounded = new RelationName("org:date_founded")
  val orgDateDissolved = new RelationName("org:date_dissolved")
  val orgNumEmployees = new RelationName("org:number_of_employees_members")
  val orgWebsite = new RelationName("org:website")

  def attributeRelations: Set[RelationName] = Set(
    perAltNames, perTitle,
    perCharges, perDateOB,
    perAge, perDateOD,
    orgAltNames, orgNumEmployees, orgPolitic,
    orgDateFounded, orgDateDissolved,
    perCauseOD, perReligion, orgWebsite
  )

*/
  def all = names.values.toSet[RelationName]
}


class AttributeExtractors(val attr: AttributeExtractor*) extends AttributeExtractor {

  override def preprocess(d: Document) = {
    /*try {
      val dates = DocDateParser.getAllDatesInDoc(d)
      DocDateParser.createDateSpans(d, dates)
    } catch {
      case e: Exception => e.printStackTrace()
    }
    attr.foreach(_.preprocess(d))*/
  }

  def extract(np: NounPhrase) = attr.flatMap(_.extract(np))
}

object TitleExtractor extends AttributeExtractor {

  val triggerTokens = Some(
    Set("secretary","technician", "correspondent", "soldier", "engineer", "pilot",
      "columnist", "rep", "congressman", "congresswoman", "representative", "ambassador", "lieutenant", "captain",
      "gen.", "rep.", "gov.", "capt.", "col.", "sen.")
  )

  val FALSE_TITLES = Set("mr.", "mrs.", "ms.", "miss")
  val JOB_TITLE_LEXICON = ClasspathResourceLexicons.iesl.JobTitle
  val PER_HONORIFIC_LEXICON = ClasspathResourceLexicons.iesl.PersonHonorific
  val ABBRV_TRIGGERS = Set("rep", "gov")

  private def isFalseTitle(token: Token): Boolean = FALSE_TITLES.contains(token.string.toLowerCase)

  /* the strategy here is to look for obvious titles and associate them with mentions
     that occur in that sentence; specifically, we look if a word in the lexicon is a child
     the docMention
   */
  private def extractWithLexicon(np: NounPhrase): Seq[AttributeRelationSpan]= {
    val toks = np.sentence.tokens
      .filter(tok => (!tok.isDigits && !isFalseTitle(tok)) && (JOB_TITLE_LEXICON.contains(tok) || PER_HONORIFIC_LEXICON.contains(tok) || triggerTokens.get.contains(tok.string.toLowerCase)))
    if (toks.nonEmpty)
    //      toks.filter(tok => tok.parseParent == docMention.headToken || docMention.headToken.parseParent == tok)
    //        .map(tok => createPhraseAttributeRelation(docMention, tok, RelationNames.perTitle))
      toks.map(tok => createPhraseAttributeRelation(np, tok, RelationNames.perTitle))

    else ArrayBuffer.empty[AttributeRelationSpan]
  }

  /* if this mention has a child that is an appos and also has a child who is another mention,
  that mention should also be tagged with the same title. This is to find things like congressmen A and B
   TODO AK: This needs more work */

  /*private def extractPerList(np: NounPhrase): Seq[NounPhrase] = {
    val mentsInSent = np.document.attr[NounPhrase].filter(d => d.sentence == np.sentence)
    val children    = np.headToken.parseChildren
    val childrenDms = mentsInSent.filter(m => children.contains(m.headToken))
    childrenDms ++ childrenDms.map(extractPerList(_)).flatten
  }
    */
  /* return or more per:title relations extracted from the sentence containing
     a given docMention

TODO AK: Rewrite the following rules to reflect the current state of things
     Current Rules:
     1) Look at the dependency parse of a sentence containing a given mention;
        if that sentence contains an appositive which points to the mention,
        return the mention/appositive pair as a per:title relation (e.g. "Hilary Clinton,
        Secretary of State")

     2) Similar to rule 1 but look for a mention that points to an appositive (e.g.
        "Secretary of State, Hilary Clinton")

     3) find words that are obviously titles and associate them with each mention in the sentence

     4) If the word before or after the token in question is 'of' and the next word is a noun, extract
        the phrase. Additionally, if the word before the found title is a noun, include that word in the
        extraction
  */
  def extract(np: NounPhrase): Seq[AttributeRelationSpan] = {

    def extractTitle(np: NounPhrase): Seq[AttributeRelationSpan] = {
      var relations  = new scala.collection.mutable.ArrayBuffer[AttributeRelationSpan] // mutable so we can add to it while looping
      val APPOSITIVE = "appos"

      val head     = np.headToken
      val children = head.parseChildren
      val parent   = head.parseParent

      for (child <- children) {
        if (child.parseLabel.categoryValue == APPOSITIVE && notProper(child) && !child.isDigits && !isFalseTitle(child)) {
          relations += createPhraseAttributeRelation(np, child, RelationNames.perTitle)

          /* we'd like to be able to identify the title of people that appear in lists but this functionality
             isn't quite there yet
           */
          // val perList = extractPerList(docMention)
          // perList.foreach(dm => relations += createPhraseAttributeRelation(dm, child, RelationNames.perTitle))
        }
      }

      if (parent != null && parent.parseLabel.categoryValue == APPOSITIVE && notProper(parent) && !parent.isDigits && !isFalseTitle(parent))
        relations += createPhraseAttributeRelation(np, parent, RelationNames.perTitle)

      /* add in common titles extraction */
      relations ++= extractWithLexicon(np)

      relations
    }

    if (isPerOrUkn(np)) extractTitle(np)
    else ArrayBuffer.empty[AttributeRelationSpan]
  }
}
