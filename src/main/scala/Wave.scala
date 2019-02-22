import space._
import space.Implicits._
import java.io._
import javax.sound.sampled._

object Wave extends App {

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
    sample = sampleSigned / fullScale
  } yield sample).toArray

//  val samplesPadded = NdArray.padTo(samplesIn, 64*1024)
  // Transformation
  val size = samplesIn.length // Best sound at powers of 2 due to padding
  val space = new EuclidianSpace
  val segments = samplesIn.grouped(size).toList
  val times = segments.map(s => Trajectory.fromTensor(Tensor.from(s)))
  val frequencies = Trajectory.fromConcepts(times.map(space.transform).toArray)
  val quefrency = space.transform(frequencies)
  val frencyque = space.inverse(quefrency)
  val inverses = frequencies.concepts.map(space.inverse)
//  val samplesTrans = (Vector[Any]() /: inverses.map(_.tensor.toRealArray)) (_ ++ _)

  val ratio = 1//samplesTrans.length.toFloat / samplesIn.length

  val time = Trajectory.fromTensor(Tensor.from(samplesIn))
  val freq = space.transform(time)
  val freq2 = space.transform(Trajectory.fromTensor(freq.tensor))
  val inv2 = space.inverse(freq2)
  val inv = space.inverse(Concept(inv2.tensor))
  val samplesTrans = inv.tensor.toRealArray


  // Encode
  val samplesOut = (for {
    sample <- samplesTrans.toArray
    long = (sample.asInstanceOf[Double] * fullScale).toLong // signed
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
