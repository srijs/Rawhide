import java.util.{List, LinkedList}
import java.io.{InputStream, OutputStream}
import java.security.MessageDigest

// Test imports
import java.util.Arrays
import java.io.{FileOutputStream, FileInputStream}
import java.security.DigestOutputStream

type WeakHash   = Int
type StrongHash = Array[Byte]

class Adler32(size: Int, mod: Int) {

  def hash(bytes: Array[Byte]): WeakHash = {
    var a = 1
    var b = 0
    for (byte <- bytes) {
      a += byte % mod
      b += a % mod
    }
    (b << 16) | a
  }

  def roll(hash: WeakHash, added: Byte, removed: Byte, windowSize: Int): WeakHash = {
    var a = hash
    var b = (a >> 16) & 0xffff
    a &= 0xffff
    a = (a - removed + added) % mod
    b = (b - (windowSize * removed) + a) % mod
    (b << 16) | a
  }

}

abstract class Rawhide(wz: Int, ch: Int, cm: Int) {
 
  // Driver

  def createOutputStream(): OutputStream
  def finalizeOutputStream(out: OutputStream, hash: StrongHash)

  def chunkExists(hash: StrongHash): Boolean
  
  // Weak Hasher (Adler 32)

  val adler = new Adler32(wz, 64 * 1024)

  def weakHash(bytes: Array[Byte]): WeakHash = {
    adler.hash(bytes)
  }
    
  def weakRoll(hash: WeakHash, added: Byte, removed: Byte, windowSize: Int): WeakHash = {
    adler.roll(hash, added, removed, windowSize)
  }

  // Strong Hasher (SHA1)

  def createStrongHash(): MessageDigest = {
    return MessageDigest.getInstance("SHA-1")
  }

  // Stream Tools

  def readIntoBuffer(in: InputStream, buffer: Array[Byte]): Int = {
    val cap = buffer.length
    var off = 0
    var len = 0
    var read = in.read(buffer)
    println("cap:" + cap + "; len:" + len + "; read:" + read)
    while (read > -1 && len < cap) {
      len = len + read
      off = off + read
      read = in.read(buffer, off, cap - len)
      println("cap:" + cap + "; len:" + len + "; read:" + read)
    }
    len
  }

  // Pipe

  def pipe(in: InputStream): List[StrongHash] = {

    // Hash List
    var hashList = new LinkedList[StrongHash]

    // Ring Buffer State
    var buffer = new Array[Byte](wz)
    var offset = 0

    // Stream State
    var outputStream = createOutputStream()
    var streamHash = createStrongHash()

    // Pointer State
    var ws = 0
    var ss = 0

    // Init
    var read = readIntoBuffer(in, buffer)
    var roll = weakHash(buffer)

    println("read=" + read)

    while (read == wz) {

      if (roll % cm != ch) {

        val oldByte = buffer(offset)
        outputStream.write(oldByte)
        streamHash.update(oldByte)
        read = in.read(buffer, offset, 1)
        val newByte = buffer(offset)
        offset = (offset + 1) % wz
        roll = weakRoll(roll, newByte, oldByte, wz)

      } else {

        // Finalise current upload
        val streamDigest = streamHash.digest()
        finalizeOutputStream(outputStream, streamDigest)
        hashList.add(streamDigest)

        // Compute strong hash
        val chunkHash = createStrongHash()
        chunkHash.update(buffer, offset, wz - offset)
        chunkHash.update(buffer, 0, offset)
        val chunkDigest = chunkHash.digest()

        // Query existance
        if (!chunkExists(chunkDigest)) {

          // Upload chunk
          val chunkStream = createOutputStream()
          chunkStream.write(buffer, offset, wz - offset)
          chunkStream.write(buffer, 0, offset)
          finalizeOutputStream(chunkStream, chunkDigest)

        }

        // Append chunk to list
        hashList.add(chunkDigest)

        // Discard & re-init stream
        offset = 0
        read = readIntoBuffer(in, buffer)
        roll = weakHash(buffer)
        outputStream = createOutputStream()
        streamHash = createStrongHash()

      }
      println("read=" + read)
    }

    // Finalise current upload
    if (read > 0) {
      val rest = Math.min(offset + read, wz) - offset
      outputStream.write(buffer, offset, rest)
      outputStream.write(buffer, 0, read - rest)
      streamHash.update(buffer, offset, rest)
      streamHash.update(buffer, 0, read - rest)
    }
    val streamDigest = streamHash.digest()
    finalizeOutputStream(outputStream, streamDigest)
    hashList.add(streamDigest)

    hashList

  }

}

object Test {

  object R extends Rawhide(10, 0, 2) {
    def createOutputStream(): OutputStream = {
      return new DigestOutputStream(new FileOutputStream("/tmp/out", true), MessageDigest.getInstance("SHA-1"))
    }
    def finalizeOutputStream(out: OutputStream, hash: StrongHash) = {
      val eq = Arrays.equals(out.asInstanceOf[DigestOutputStream].getMessageDigest().digest(), hash)
      println("Equal: " + eq)
    }
    def chunkExists(hash: StrongHash) = {
      println("Checking for chunk")
      false
    }
  }

}
