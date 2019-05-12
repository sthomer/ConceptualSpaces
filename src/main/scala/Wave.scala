import space._
import Implicits._

import java.io._
import javax.sound.sampled._

object Wave extends App {

  withAudio("src/main/scala/export.wav") { input =>
    println("Transforming...")
    val size = 64
    val timeSpace = new EuclidianSpace
    val timeConcepts = input
      .map(r => Concept(r))
    timeSpace.concepts = timeConcepts
    val timeTrajectories = timeSpace.concepts
      .grouped(size) //.sliding(size, size / 2)
      .map(a => Trajectory(a))
      .toVector
    val space = new EuclidianSpace
    var freqConcepts = timeTrajectories.map(space.transform)
    // Last one will (always) be shorter, so pad it
    freqConcepts = freqConcepts.init :+
      Concept(freqConcepts.last.tensor.padTo(freqConcepts.head.tensor.length))

    val mem = Memory.make(freqConcepts.head)
    freqConcepts.tail.foreach(c => mem.perceive(c))
    val nodes = mem.dimensions.indices.map(mem.collectN(_, mem.root))

    val rows = nodes.head.indices map { i =>
      val row = nodes map { dimension =>
        dimension(i) match {
          case None => 0
          case Some(node) => space.norm(node.concept)
        }
      }
      row.mkString(",")
    }
    toDat("slide", rows)

    println("Inverting...")
    mem.dimensionAt(1).space.concepts
      .flatMap(space.inverse(_).concepts)
      .flatMap(space.inverse(_).concepts)
      .map(_.tensor)
      .map({ case c: Complex => c re })
  }

  def toDat(name: String, data: Seq[Any]): Unit = {
    val out = data.mkString("\n")
    val file = new File("src/main/scala/analysis/out/" + name + ".dat")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(out)
    bw.close()
  }

  def withAudio(filename: String)(op: Vector[Double] => Vector[Double]): Unit = {
    // Read file
    println("Loading wav...")
    val file = new File(filename) // PCM Signed 16-bit
    val audioIn = AudioSystem.getAudioInputStream(file)
    val audioLength = audioIn.getFrameLength * audioIn.getFormat.getFrameSize
    val bytes = new Array[Byte](audioLength.toInt)
    val readLength = audioIn.read(bytes)
    assert(audioLength == readLength)

    // Prepare conversion
    val bitsPerSample = audioIn.getFormat.getSampleSizeInBits
    val fullScale = Math.pow(2, bitsPerSample - 1)
    val sampleSize = bitsPerSample / 8 // Byte = 8 bits
    val bitsToExtend = 64 - bitsPerSample // Long = 64 bits
    val longs = bytes.map(_.toLong)

    // Decode
    val samplesIn = (for {
      group <- longs grouped sampleSize
      sampleRaw = group reduce ((less, more) =>
        (less & 0xffL) | ((more & 0xffL) << 8)) // little-endian
      sampleSigned = (sampleRaw << bitsToExtend) >> bitsToExtend // signed
      sample = sampleSigned.toDouble / fullScale
    } yield sample).toVector

    // Do the damn thing.
    val samplesTrans = op(samplesIn).toArray

    // Encode
    println("Playing...")
    val ratio = 1 //samplesTrans.length.toFloat / samplesIn.length
    val samplesOut = (for {
      sample <- samplesTrans
      long = (sample * fullScale).toLong // signed
      bytes = for (i <- 0 until sampleSize)
        yield ((long >>> i * 8) & 0xffL).toByte // little-endian
    } yield bytes).flatten

    // Play
    val format = new AudioFormat( // Matches audioIn format
      AudioFormat.Encoding.PCM_SIGNED, // Encoding
      16000.toFloat * ratio, // Sample Rate (for slow: * 0.5F)
      16, // Sample Size in Bits
      1, // Channels
      2, // Frame Size
      16000.toFloat * ratio, // FrameRate (for slow: * 0.5F)
      false) // Big-Endian?

    val sampleIn = new AudioInputStream(
      new ByteArrayInputStream(samplesOut), format, samplesOut.length)

    val sampleIn2 = new AudioInputStream(
      new ByteArrayInputStream(samplesOut), format, samplesOut.length)

    AudioSystem.write(
      sampleIn, AudioFileFormat.Type.WAVE,
      new File("src/main/scala/analysis/out/out.wav"))

    val clip = AudioSystem.getClip(null)
    clip.open(sampleIn2)
    clip.start()
    clip.drain()
    clip.close()
  }

  // Concatenate all WAVs in dir and subdirs with:
  //  sox $(find -iname '*.WAV') ~/Downloads/out.wav
}

//def test(input: Vector[Double]): Vector[Double] = {
//    println("Transforming...")
//    val size = 256
//    val timeSpace = new EuclidianSpace
//    val timeConcepts = input
//      .map(r => Concept(r))
//    timeSpace.concepts = timeConcepts
//    val timeTrajectories = timeSpace.concepts
//      .grouped(size)
//      .map(a => Trajectory(a))
//      .toVector
//    val space = new EuclidianSpace
//    var freqConcepts = timeTrajectories.map(space.transform)
//    // Last one will (always) be shorter, so pad it
//    freqConcepts = freqConcepts.init :+
//      Concept(freqConcepts.last.tensor.padTo(freqConcepts.head.tensor.length))
//
//    println("Categorizing...")
//    val freqSpace = new RawEuclidianSpace
//    freqSpace.fill(freqConcepts)
//    val catSpace = new EuclidianSpace
//    catSpace.fill(freqSpace.categorize)
//    val catSpace2 = new EuclidianSpace
//    catSpace2.concepts = catSpace.categorize
//
//    println("# Original: " + freqSpace.concepts.distinct.length)
//    println("# Categories: " + catSpace.concepts.distinct.length)
//    println("# Categories 2: " + catSpace2.concepts.distinct.length)
//
//    val segSpace = new EuclidianSpace
//    segSpace.concepts = catSpace2.concepts
//    //    segSpace.chop(catSpace2.concepts)
//    segSpace.concepts = segSpace.segmentize
//
//    val intSpace = new EuclidianSpace
//    val inters = segSpace.segments.map(segment => space.interpolate(segment))
//
//    println("# Categories 3: " + segSpace.concepts.distinct.length)
//
//    toDat("cats", catSpace2.concepts.map(c => space.norm(c)))
//    toDat("segs", segSpace.concepts.map(c => space.norm(c)))
//
//    val frequencies = Trajectory(segSpace.concepts)
//    //  val frequencies2 = space.transform(frequencies)
//    //  val inverses2 = space.inverse(frequencies2)
//
//    println("Inverting...")
//    val inverses = frequencies.concepts.map(space.inverse)
//    val outConcepts = inverses.flatMap(t => t.concepts)
//    outConcepts
//      .map(c => c.tensor)
//      .map({ case c: Complex => c.getReal })
//  }
