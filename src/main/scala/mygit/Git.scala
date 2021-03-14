package mygit

import java.io.File
import java.nio.file.Path
import java.nio.file.Files

import scala.collection.JavaConverters._
import java.nio.file.Paths
import java.security.MessageDigest
import scala.io.Source
import scala.annotation.tailrec

case object Git {
  

  def init(): Unit = {
    println("Initializing git-in-scala repository.")

    Paths.get(".git-in-scala", "objects").toFile.mkdirs()
    Paths.get(".git-in-scala", "refs", "heads").toFile.mkdirs()

    ()
  }


  def commit(message: String): Unit = {
    println("Committing changes.")

    // The first thing we do is create a git tree with root at the current directory
    val tree = createTree(Paths.get(""))

    // Fetch the current commit hash
    val currentCommit = fetchHead()

    // Next we use a hash of this new tree and a parent commit to create a commit object
    val commitObject = Commit("", tree.hash, currentCommit, message).serialize
    val commitHash = hash(commitObject)

    // Write the newly created commit object to disk, same as with blob and tree objects
    Files.write(Paths.get(".git-in-scala", "objects", commitHash), commitObject)

    // Replace the curent HEAD with the newly created commit
    Files.write(Paths.get(".git-in-scala", "HEAD"), commitHash.getBytes)
  }


  def log(): Unit = {
    fetchHead() match {
      case Some(currentCommit) =>
        val commits = loadCommmits(currentCommit, List.empty)
        val logStr = commits.map(commit =>
            s"""
             |commit ${commit.commitHash}
             |tree   ${commit.treeHash}
             |
             |Message:
             |    ${commit.message}
            """.stripMargin
        ).mkString("\n----------------------------------------------------------")

        println(logStr)
      case None =>
        println("There are no commits in this repository yet.")
    }
  }

  @tailrec
  private def loadCommmits(commitHash: String, result: List[Commit]): List[Commit] = {
    Commit.load(commitHash) match {
      case Some(commit @ Commit(_, _, Some(parentHash), _)) =>
        // This commit has a parent hash so we continue with the recursion
        loadCommmits(parentHash, result :+ commit)
      case Some(commit) =>
        // Commit has no parent hash so its the last one
        result :+ commit
      case None =>
        // In case of an error we let the user know and stop
        println(s"Repository is in a broken state - could not read object with hash $commitHash")
        List.empty
    }
  }


  case class Commit(commitHash: String, treeHash: String, parent: Option[String], message: String) {
    def serialize: Array[Byte] = parent match {
      case Some(parentHash) =>
        s"""tree $treeHash
           |parent $parentHash
           |
           |$message""".stripMargin.getBytes
           
      case None =>  // This case happens when there are no commits in the repository
        s"""tree $treeHash
           |
           |$message""".stripMargin.getBytes
    }
  }
  object Commit {
    def load(hash: String): Option[Commit] = {
      val regexWithParent    = raw"tree (.+)\nparent (.+)\n\n((?s).+)".r
      val regexWithoutParent = raw"tree (.+)\n\n((?s).+)".r
      val str = Files.readString(Paths.get(".git-in-scala", "objects", hash))
      str match {
        case regexWithParent(treeHash, parentHash, message) =>
          Some(Commit(hash, treeHash, Some(parentHash), message))
        case regexWithoutParent(treeHash, message) =>
          Some(Commit(hash, treeHash, None, message))
        case _ =>
          None
      }
    }
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