package com.amardinsa

/**
 * A Huffman tree node
 */
case class Node (
    symbol: Option[Char], 
    weight: Int, 
    left: Option[Node] = None,
    right: Option[Node] = None)
  extends Ordered[Node] {

  def compare(that: Node) = -1 * (this.weight compareTo that.weight)
}

object huffman {

  val usage = """
    |Usage: huffman [OPTION] INPUT_FILE OUTPUT_FILE 
    |Compress or decompress INPUT_FILE and write to OUTPUT_FILE 
    |
    | Options:
    |    -c               Compress 
    |    -d               Decompress
    """.stripMargin

  val hufCoder = new Huffman()

  def main(args : Array[String]) : Unit = {

    args.toList match {
      case List("-c", inFile, outFile) =>
        compress(inFile, outFile)
      case List("-d", inFile, outFile) =>
        decompress(inFile, outFile)
      case _ =>
        println(usage)
    }
  }

  import java.io._
    
  def compress(inFile : String, outFile: String) : Unit = {

    import scala.io.Source

    val inputFile = new File(inFile)
    val inputMsg = Source.fromFile(inputFile).getLines.mkString("\n")

    val compressedMsg = hufCoder.compress(inputMsg)

    val os = new FileOutputStream(outFile)
    os.write(compressedMsg)
    os.close()

    print(makeStats(inputFile.length, compressedMsg.length))
  }

  def decompress(inFile : String, outFile: String) : Unit = {

    val is = new FileInputStream(inFile)
    val bos = new ByteArrayOutputStream()
    var buf = is.read
    while (buf != -1) {
      bos.write(buf)
      buf = is.read
    }
    is.close()
    bos.flush()

    val os = new FileWriter(outFile)
    os.write(hufCoder.decompress(bos.toByteArray))
    // Put an EOL character
    os.write('\n')
    bos.close()
    os.close()
  }

  /**
   * Returns formatted string with file sizes before/after 
   * compression and space savings percentage.
   */
  def makeStats(uncompressedSize : Long, 
                compressedSize : Long) : String = { 

    val buf = new StringBuilder

    buf.append("%25s".format("Uncompressed size: ") + 
      uncompressedSize + " bytes\n")
    buf.append("%25s".format("Compressed size: ") + 
      compressedSize + " bytes\n")

    val savings = (1 - compressedSize.toDouble / 
      uncompressedSize.toDouble) * 100

    buf.append("%25s".format("Space savings: ") + 
      "%.2f".format(savings) + "%\n")

    buf.toString
  }
}

class Huffman {
    
  // Assuming an 8-bit character encoding, use 256 as EOF symbol
  private val EOF = 256.toChar

  /**
   * Compress input string and return compressed data
   * as an array of bytes.
   */
  def compress(input : String) : Array[Byte] = {

    val inputWithEOF = input + EOF

    val frequencies = getFrequencies(inputWithEOF)

    val tree = buildTree(frequencies)

    val encodedBitString = inputWithEOF.map(
      symbol => codeFor(tree, symbol)).mkString

    val header = getHeader(frequencies)

    // Size of compressed body in bytes
    val bodySize = (java.lang.Math.ceil(
      encodedBitString.length / (java.lang.Short.SIZE-1.0)) * 2.0).toInt

    import java.nio.ByteBuffer
    val buf = ByteBuffer.allocate(header.length + bodySize)

    buf.put(header)
    
    // Split the encoded bitstring into groups of Short.SIZE-1
    // bits, pad the right side of the last group with '0's,
    // get the Short value of each group, and put each value
    // in the ByteBuffer.
    encodedBitString.grouped(java.lang.Short.SIZE-1).toList.map(
      bitString =>
        ("%-" + (java.lang.Short.SIZE-1) + "s").format(bitString)
          .replace(" ", "0")).map(
            shortString => 
              java.lang.Short.parseShort(shortString,2)).foreach(
                shortVal => buf.putShort(shortVal))

    buf.position(0)
    
    buf.array
  }

  /**
   * Decompress data in input byte array and return decoded string.
   */
  def decompress(input : Array[Byte]) : String = {

    import java.nio.ByteBuffer

    val buf = ByteBuffer.allocate(input.length)
    buf.put(input)
    buf.position(0)
    // Reconstruct frequencies list from header
    val frequencies = List.range(0, buf.getInt).map(
      _ => (buf.getChar, buf.getInt))

    // Reconstruct encoded bitstring by reading the rest of
    // the buffer as a sequence of Short values, formatting
    // each value as a binary string and padding the left
    // sides with '0's.
    val bitString = (0 until 
      (buf.position to buf.capacity).length/2).map(
        _ => buf.getShort).map(
          shortVal => 
            ("%" + (java.lang.Short.SIZE-1) + "s").format( 
              java.lang.Integer.toString(shortVal,2))
                .replace(" ", "0")).mkString

    decodeMessage(bitString, buildTree(frequencies))
  }

  /** 
   * Returns list of tuples containing each unique
   * character in input string paired with its
   * frequency
   */
  private def getFrequencies(input : String) : List[(Char, Int)] = {

    input.toCharArray.distinct.toList.map( 
      symbol => (symbol, input.count(_ == symbol))
    ) 
  }

  /**
   * Build a Huffman tree from a list of (character, frequency)
   * tuples.
   */
  private def buildTree(frequencies : List[(Char, Int)]) : Node = {

    import scala.collection.mutable.PriorityQueue

    val queue = PriorityQueue.empty[Node]

    for ((symbol, weight) <- frequencies)
      queue.enqueue(Node(Some(symbol), weight))

    while (queue.size > 1) {
      val first = queue.dequeue()
      val second = queue.dequeue()

      queue.enqueue(Node(None, first.weight + second.weight, 
        Some(first), Some(second)))
    }

    queue.dequeue()
  }

  /**
   * Decode bitstring using the provided Huffman tree.
   */
  private def decodeMessage(bitString : String, root : Node) : String = {

    // Decode the first character found in bitstring, returning
    // that character and the remaining bitstring
    def decodeChar(input : String, tree : Node) : (String,String) = {
      if (tree.symbol.isDefined) 
        (tree.symbol.get.toString, input)
      else if (input.charAt(0) == '0')
        decodeChar(input.substring(1), tree.left.get)
      else
        decodeChar(input.substring(1), tree.right.get)
    }

    var (curMsg, remainingBits) = decodeChar(bitString, root)
    while (curMsg.charAt(curMsg.length-1) != EOF) {
      decodeChar(remainingBits, root) match {
        case (newMsg, newRemaining) => 
          curMsg += newMsg; remainingBits = newRemaining
      }
    }
    curMsg.substring(0,curMsg.length-1)
  }

  /**
   * Returns a file header for the given (character, frequency)
   * distribution as an array of bytes
   *
   * Header format:
   * 
   * | Number of (character, frequency) pairs (4 byte Int)
   * | Symbol 1 (2 byte Char) | Frequency 1 (4 byte Int) 
   * | Symbol 2 (2 byte Char) | Frequency 2 (4 byte Int)
   * ...
   *
   * Total size: 4 + (4 + 2) * n bytes where n is number of 
   *             char, frequency pairs
   */
  private def getHeader(frequencies : List[(Char, Int)]) : Array[Byte] = {

    import java.nio.ByteBuffer

    val INT_BYTES = java.lang.Integer.SIZE / 8
    val CHAR_BYTES = java.lang.Character.SIZE / 8

    val headerSize = INT_BYTES + 
                    (INT_BYTES + CHAR_BYTES) *
                    frequencies.length
                    
    val buf = ByteBuffer.allocate(headerSize)
    buf.putInt(frequencies.length)
    for ((symbol, frequency) <- frequencies) {
      buf.putChar(symbol)
      buf.putInt(frequency)
    }

    buf.array()
  }

  // Traverse tree to find node with symbol goal
  private def contains(root : Node, goal : Char) : Boolean = {

    if (root.symbol.isDefined && root.symbol.get == goal) 
      true
    else if (!root.left.isDefined)
      false
    else
      contains(root.left.get, goal) || contains(root.right.get, goal)
  }

  // Return binary code for symbol goal
  private def codeFor(root : Node, goal : Char) : String = {

    if (root.symbol.isDefined && root.symbol.get == goal)
      ""
    else if (root.left.isDefined && contains(root.left.get, goal)) 
      "0" + codeFor(root.left.get, goal)
    else 
      "1" + codeFor(root.right.get, goal)
  }
}
