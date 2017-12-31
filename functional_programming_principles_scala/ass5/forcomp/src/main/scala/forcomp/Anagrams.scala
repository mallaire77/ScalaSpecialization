package forcomp

import scala.collection.mutable


object Anagrams {
  /** A word is simply a `String`. */
  type Word =
    String

  /** A sentence is a `List` of words. */
  type Sentence =
    List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences =
    List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] =
    loadDictionary


  /**
    * Counts occurrences of char in a given word.
    */
  def charOccurrences(char: Char, word: Word): Int =
    word.foldLeft(0) { (acc, _char) =>
      if (char == _char)
        acc + 1
      else
        acc
    }

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase
      .view
      .groupBy(identity)
      .mapValues(_.size)
      .toList
      .sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    s.view
      .flatMap(wordOccurrences)
      .groupBy(_._1)
      .mapValues(_.foldLeft(0) {
        (acc, occ) =>
          acc + occ._2
      })
      .toList
      .sorted

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(wordOccurrences)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences.getOrElse(wordOccurrences(word), List.empty[Word])

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def occurrenceList(occurrence: (Char, Int), acc: Occurrences = List.empty[(Char, Int)]): Occurrences =
      if(occurrence._2 > 0) {
        occurrenceList(occurrence.copy(_2 = occurrence._2 - 1), acc :+ occurrence)
      } else {
        acc
      }

    def innerCombination(occurrences: Occurrences): Occurrences =
      occurrences.foldLeft(List.empty[(Char, Int)]) {
        (acc, occ) =>
          acc ++ occurrenceList(occ)
      }

    def anagramPermutations(list: Occurrences): List[Occurrences] = {
      def innerAnagramPermutations(combos: Int, idx: Int = 0, acc: List[Occurrences] = List.empty[Occurrences]): List[Occurrences] = {
        if (idx == combos)
          acc ++ list.combinations(idx)
        else
          innerAnagramPermutations(combos, idx + 1, acc ++ list.combinations(idx))
      }

      innerAnagramPermutations(list.map(_._1).distinct.size).collect {
        case permutation if permutation.size == permutation.map(_._1).distinct.size =>
          permutation
      }
    }

    anagramPermutations(innerCombination(occurrences))
  }

  def canSubtract(x: Occurrences, y: Occurrences): Boolean =
    y.map(_._1).forall(x.map(_._1).contains) &&
    !x.exists {
      case (character, occ1) =>
        y.find(_._1 == character).fold(false) {
          case (_, occ2) =>
            occ1 - occ2 < 0
        }
    }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    x.foldLeft(y.toMap) { (acc, occ) =>
      acc.get(occ._1).fold {
        acc + (occ._1 -> occ._2)
      } { occurrences =>
        if (occ._2 - occurrences < 0)
          throw new IllegalArgumentException("The precondition is that the occurrence list `y` is a subset of the occurrence list `x` -- any character appearing in `y` must  appear in `x`, and its frequency in `y` must be smaller or equal than its frequency in `x`")
        else if (occ._2 - occurrences == 0)
          acc - occ._1
        else
          acc.updated(occ._1, occ._2 - occurrences)
      }
    }
    .toList
    .sorted

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def processCombos(
      wordIdx: Int,
      word: Word,
      sentenceOccs: Occurrences,
      combos: List[(Word, Int)]
    ): List[List[Word]] = {
      def innerProcessCombos(currIdx: Int, acc: List[List[Word]] = List(List.empty[Word])): List[List[Word]] = {
        if (currIdx < combos.size - 1) {
          var _sentenceOccsAcc: Occurrences = sentenceOccs
          val _acc: mutable.ListBuffer[Word] = mutable.ListBuffer[Word](word)

          for (
            i <- currIdx until combos.size;
            _word = combos(i)._1;
            word2Occ = wordOccurrences(_word)
            if i != wordIdx && canSubtract(_sentenceOccsAcc, word2Occ)
          ) {
            _sentenceOccsAcc = subtract(_sentenceOccsAcc, word2Occ)
            _acc += _word
          }

          innerProcessCombos(currIdx + 1, acc :+ _acc.toList)
        } else {
          acc
        }
      }

      innerProcessCombos(0)
    }

    if (sentence.isEmpty)
      List(List.empty[Word])
    else {
      val sentenceOccs = sentenceOccurrences(sentence)
      val combos = combinations(sentenceOccs)
        .flatMap(dictionaryByOccurrences.get)
        .flatten
        .zipWithIndex

      combos
        .view
        .foldLeft(List(List.empty[Word])) { (acc, word) =>
          acc ++ processCombos(word._2, word._1, subtract(sentenceOccs, wordOccurrences(word._1)), combos)
        }
        .filter { combo =>
          subtract(sentenceOccurrences(sentence), sentenceOccurrences(combo)).isEmpty
        }
        .flatMap(_.permutations)
        .distinct
    }
  }
}
