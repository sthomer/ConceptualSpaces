import space._
import Implicits._

import java.io._
import javax.sound.sampled._

object Wave extends App {

  // Concatenate all WAVs in dir and subdirs with:
  //  sox $(find -iname '*.WAV') ~/Download/out.wav
  // Read file
  //val file = new File("src/main/TIMIT/TRAIN/DR1/FCJF0/SA1.WAV")
  val file = new File("src/main/scala/export.wav") // PCM Signed 16-bit
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
  } yield sample).toArray

  // Transformation
  val size = 256 // samplesIn.length
  val space = new FastEuclidianSpace

  val timeSpace = new FastEuclidianSpace
  val timeConcepts = samplesIn
    .map(r => Concept(r))
    .toVector
  timeSpace.concepts = timeConcepts

  val timeTrajectories = timeSpace.concepts
    .grouped(size)
    .map(a => Trajectory(a))
    .toVector
  timeSpace.trajectories = timeTrajectories

  val freqSpace = new FastEuclidianSpace
  val freqConcepts = timeSpace.trajectories.map(space.transform)
  // Last one will (always) be shorter, so pad it
  freqSpace.concepts = freqConcepts.init :+
    Concept(freqConcepts.last.tensor.padTo(freqConcepts.head.tensor.length))

  val fsquant = freqSpace.quantize
  val dis = fsquant.distinct.length
  println("Categories: " + dis)
  println("Compression: " + (dis.toFloat / fsquant.length))

  val frequencies = Trajectory(fsquant)
//  val frequencies2 = space.transform(frequencies)
//  val inverses2 = space.inverse(frequencies2)
  val inverses = frequencies.concepts.map(space.inverse)
  val outConcepts = inverses.flatMap(t => t.concepts)
  val outs = outConcepts
    .map(c => c.tensor)
    .map({case c: Complex => c.getReal})
  val samplesTrans = outs.toArray

  val ratio = 1 // samplesTrans.length.toFloat / samplesIn.length



  // Encode
  val samplesOut = (for {
    sample <- samplesTrans
//    long = scala.math.round(sample) // Double->Long
    long = (sample * fullScale).toLong // signed
    bytes = for (i <- 0 until sampleSize)
      yield ((long >>> i * 8) & 0xffL).toByte // little-endian
  } yield bytes).flatten

  // Play
  val format = new AudioFormat(       // Matches audioIn format
    AudioFormat.Encoding.PCM_SIGNED,  // Encoding
    16000.toFloat * ratio,            // Sample Rate (for slow: * 0.5F)
    16,                               // Sample Size in Bits
    1,                                // Channels
    2,                                // Frame Size
    16000.toFloat * ratio,            // FrameRate (for slow: * 0.5F)
    false)                            // Big-Endian?
  val sampleIn = new AudioInputStream(
    new ByteArrayInputStream(samplesOut), format, samplesOut.length)
  ///val sampleIn = AudioSystem.getAudioInputStream(file)
  val clip = AudioSystem.getClip(null)
  clip.open(sampleIn)
  clip.start()
  clip.drain()
  clip.close()
}
