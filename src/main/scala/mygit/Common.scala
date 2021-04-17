package mygit

import java.nio.file.Files
import java.security.MessageDigest
import java.nio.file.Path

object Common {

  def fetchHead(): Option[String] = {
    val path = Path.of(".git-in-scala", "HEAD")
    if(Files.exists(path)) {
      Some(Files.readString(path)).filter(_.nonEmpty)
    } else {
      None
    }
  }

  def hash(bytes: Array[Byte]): String = {
    MessageDigest.getInstance("SHA1").digest(bytes)
      .map(byte => String.format("%02x", byte)) // Converts a byte to its hex string representation
      .mkString
  }

}