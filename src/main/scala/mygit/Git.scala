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
    Paths.get(".git-in-scala", "HEAD").toFile.createNewFile()

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
        // Recursively load commits, starting from the current HEAD and going back to the begginging of history
        val commits = loadCommmits(currentCommit, List.empty)

        // Create a text representation for each commit
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


  def checkout(hash: String): Unit = {
    getCurrentAndTargetTrees(hash).map { case (currentTree, targetTree) =>
      //
      deleteEverythingFromTree(currentTree, root = Path.of(""))

      rebuildEverythingFromTree(targetTree, root = Path.of(""))
    }
  }

  def status(): Unit = {
    val maybeTree = fetchHead()
      .flatMap(hash => Commit.load(hash))
      .map(commit => commit -> ExpandedTree.load(commit.treeHash))
    
    maybeTree match {
      case Some((commit, tree)) =>
        val result = getUntrackedPaths(Path.of("."), tree)
        println(s"Currently at commit ${commit.commitHash}.")
        if(result.added.isEmpty && result.changed.isEmpty) {
          println("No changes in the repository since last commit.")
        } else {
          if(result.changed.nonEmpty) {
            println("Files with changes:")
            result.changed
              .map(path => "        " ++ path)
              .foreach(println)
          }

          if(result.added.nonEmpty) {
            println("Files not yet tracked:")
            result.added
              .map(path => "        " ++ path)
              .foreach(println)
          }
        }
      case None =>
        println("No commits in this repository.")
    }
  }

  case class StatusResult(added: List[String], changed: List[String])

  private def getUntrackedPaths(path: Path, tree: ExpandedTree): StatusResult = {
    path.toFile.listFiles()
      .toList
      .filter(_.getName != ".git-in-scala")
      .foldLeft(StatusResult(List.empty, List.empty)) { (result, file) =>
        if(file.isDirectory) {
          val maybeExistingTree = tree.treeEntries.find(_.name == file.getName)
          maybeExistingTree match {
            case Some(existingTree) =>
              val innerResult = getUntrackedPaths(path.resolve(file.getName), existingTree)
              result.copy(
                added = result.added ::: innerResult.added,
                changed = result.changed ::: innerResult.changed
              )
            case None =>
              result.copy(added = result.added :+ (path.resolve(file.getName()).toString ++ "/"))
          }
        } else {
          tree.blobEntries.find(_.name == file.getName) match {
            case Some(existingEntry) => 
              val fileBytes = Files.readAllBytes(file.toPath)
              if(hash(fileBytes) != existingEntry.hash) {
                result.copy(changed = result.changed :+ (path.resolve(file.getName).toString))
              } else {
                result
              }
            case None => 
              result.copy(added = result.added :+ (path.resolve(file.getName).toString))
          }
        }
      }
  }

  private def deleteEverythingFromTree(tree: ExpandedTree, root: Path): Unit = {
    tree.blobEntries.foreach(treeEntry =>
      Files.deleteIfExists(root.resolve(treeEntry.name))
    )

    tree.treeEntries.foreach { tree =>
      val path = root.resolve(tree.name)
      deleteEverythingFromTree(tree, root = path)
      Files.deleteIfExists(path)
    }
  }

  private def rebuildEverythingFromTree(tree: ExpandedTree, root: Path): Unit = {
    tree.blobEntries.foreach { treeEntry =>
      val bytes = Files.readAllBytes(Paths.get(".git-in-scala", "objects", treeEntry.hash))
      val path = root.resolve(treeEntry.name)
      Files.write(path, bytes)
    }

    tree.treeEntries.foreach { tree =>
      val path = root.resolve(tree.name)
      Files.createDirectory(path)
      rebuildEverythingFromTree(tree, path)
    }
  }

  private def getCurrentAndTargetTrees(targetCommit: String): Option[(ExpandedTree, ExpandedTree)] = 
    for {
      currentCommitHash <- fetchHead()
      currentCommit     <- Commit.load(currentCommitHash)
      targetCommit      <- Commit.load(targetCommit)
    } yield (ExpandedTree.load(currentCommit.treeHash), ExpandedTree.load(targetCommit.treeHash))


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
      val regexWithParent    = raw"tree (.+)\nparent (.+)\n\n([\s\S]+)".r
      val regexWithoutParent = raw"tree (.+)\n\n([\s\S]+)".r
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
      Some(Files.readString(path)).filter(_.nonEmpty)
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