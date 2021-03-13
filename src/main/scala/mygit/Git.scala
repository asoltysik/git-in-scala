package mygit

import java.io.File
import java.nio.file.Path
import java.nio.file.Files

import scala.collection.JavaConverters._
import java.nio.file.Paths
import java.security.MessageDigest
import scala.io.Source

case object Git {
  

  def init(): Unit = {
    println("Initializing git-in-scala repository.")

    new File(".git-in-scala/objects").mkdirs()
    new File(".git-in-scala/refs/heads").mkdirs()

    ()
  }


  def commit(message: String): Unit = {
    println("Committing changes.")

    // The first thing we do is create a git tree with root at the current directory
    val tree = createTree(Paths.get(""))

    // Fetch the current commit hash
    val currentCommit = fetchHead()

    // Next we use a hash of this new tree and a parent commit to create a commit object
    val commitObject = currentCommit match {
      case Some(parentHash) =>
        s"""tree ${tree.hash}
           |parent $parentHash
           |
           |$message""".stripMargin.getBytes
           
      case None =>  // This case happens when there are no commits in the repository
        s"""tree ${tree.hash}
        
           |$message""".stripMargin.getBytes
    }

    val commitHash = hash(commitObject)

    // Write the newly created commit object to disk, same as with blob and tree objects
    Files.write(Paths.get(".git-in-scala", "objects", commitHash), commitObject)

    // Replace the curent HEAD with the newly created commit
    Files.write(Paths.get(".git-in-scala", "HEAD"), commitHash.getBytes)
  }


  sealed abstract class EntryType(val value: String)
  case object Blob extends EntryType("blob")
  case object Tree extends EntryType("tree")

  case class TreeEntry(entryType: EntryType, hash: String, name: String)

  private def createTree(path: Path): TreeEntry = {
    val treeEntries = Files.list(path)
      .iterator()
      .asScala
      .filterNot(_.startsWith(".git-in-scala"))
      .map { path =>
        if(path.toFile.isDirectory()) {
          createTree(path)
        } else {
          val fileBytes = Files.readAllBytes(path)
          val fileHash  = hash(fileBytes)
          Files.write(Paths.get(".git-in-scala", "objects", fileHash), fileBytes)
          TreeEntry(Blob, fileHash, path.getFileName.toString)
        }
      }.toList

    // Merge tree entries into a single tree object
    val treeObject = treeEntries
      .map(entry => s"${entry.entryType.value} ${entry.hash} ${entry.name}") // Each entry becomes a single string with a format `TYPE HASH NAME`
      .mkString("\n") // Each entry string becomes a single line
      .getBytes

    val treeHash = hash(treeObject)

    // Write the tree object to disk
    Files.write(Paths.get(".git-in-scala", "objects", treeHash), treeObject)

    TreeEntry(Tree, treeHash, path.getFileName.toString)
  }

  private def fetchHead(): Option[String] = {
    val path = Path.of(".git-in-scala", "HEAD")
    if(Files.exists(path)) {
      Some(Files.readString(path))
    } else {
      None
    }
  }

  private def hash(bytes: Array[Byte]): String = {
    MessageDigest.getInstance("SHA1").digest(bytes)
      .map(byte => String.format("%02x", byte)) // Converts a byte to its hex string representation
      .mkString
  }

}