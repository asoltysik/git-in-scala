package mygit

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.collection.JavaConverters._


case class CommitObject(commitHash: String, treeHash: String, parent: Option[String], message: String) {
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
object CommitObject {
  def load(hash: String): Option[CommitObject] = {
    val regexWithParent    = raw"tree (.+)\nparent (.+)\n\n([\s\S]+)".r
    val regexWithoutParent = raw"tree (.+)\n\n([\s\S]+)".r
    val str = Files.readString(Paths.get(".git-in-scala", "objects", hash))
    str match {
      case regexWithParent(treeHash, parentHash, message) =>
        Some(CommitObject(hash, treeHash, Some(parentHash), message))
      case regexWithoutParent(treeHash, message) =>
        Some(CommitObject(hash, treeHash, None, message))
      case _ =>
        None
    }
  }
}



  case class ExpandedTree(name: String, blobEntries: List[TreeEntry], treeEntries: List[ExpandedTree])
  object ExpandedTree {
    def load(rootTreeHash: String, name: String = ""): ExpandedTree = {
      val entries = Files.readAllLines(Paths.get(".git-in-scala", "objects", rootTreeHash)).asScala.toList
        .flatMap { 
          case s"$entryTypeStr $hash $name" =>
            EntryType.fromString(entryTypeStr).map(entryType => TreeEntry(entryType, hash, name))
          case _ =>
            None
        }
      
      val blobEntries = entries.filter(_.entryType == Blob)

      val treeEntries = entries.filter(_.entryType == Tree)
        .map(entry => ExpandedTree.load(entry.hash, name = entry.name))

      ExpandedTree(name, blobEntries, treeEntries)
    }
  }


  sealed abstract class EntryType(val value: String)
  object EntryType {
    def fromString(str: String): Option[EntryType] = str match {
      case Blob.value => Some(Blob)
      case Tree.value => Some(Tree)
      case _ => None
    }
  }
  case object Blob extends EntryType("blob")
  case object Tree extends EntryType("tree")

  case class TreeEntry(entryType: EntryType, hash: String, name: String)

object Commit {
  def commit(message: String): Unit = {
    println("Committing changes.")

    // The first thing we do is create a git tree with root at the current directory
    val tree = createTree(Paths.get(""))

    // Fetch the current commit hash
    val currentCommit = Common.fetchHead()

    // Next we use a hash of this new tree and a parent commit to create a commit object
    val commitObject = CommitObject("", tree.hash, currentCommit, message).serialize
    val commitHash = Common.hash(commitObject)

    // Write the newly created commit object to disk, same as with blob and tree objects
    Files.write(Paths.get(".git-in-scala", "objects", commitHash), commitObject)

    // Replace the curent HEAD with the newly created commit
    Files.write(Paths.get(".git-in-scala", "HEAD"), commitHash.getBytes)
  }

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
          val fileHash  = Common.hash(fileBytes)
          Files.write(Paths.get(".git-in-scala", "objects", fileHash), fileBytes)
          TreeEntry(Blob, fileHash, path.getFileName.toString)
        }
      }.toList

    // Merge tree entries into a single tree object
    val treeObject = treeEntries
      .map(entry => s"${entry.entryType.value} ${entry.hash} ${entry.name}") // Each entry becomes a single string with a format `TYPE HASH NAME`
      .mkString("\n") // Each entry string becomes a single line
      .getBytes

    val treeHash = Common.hash(treeObject)

    // Write the tree object to disk
    Files.write(Paths.get(".git-in-scala", "objects", treeHash), treeObject)

    TreeEntry(Tree, treeHash, path.getFileName.toString)
  }
}