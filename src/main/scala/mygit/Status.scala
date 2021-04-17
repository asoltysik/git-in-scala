package mygit

import java.nio.file.Path
import java.nio.file.Files

object Status {

  def status(): Unit = {
    val maybeTree = Common.fetchHead()
      .flatMap(hash => CommitObject.load(hash))
      .map(commit => commit -> ExpandedTree.load(commit.treeHash))
    
    maybeTree match {
      case Some((commit, tree)) =>
        val result = getUntrackedPaths(Path.of("."), tree)
        println(s"Currently at commit ${commit.commitHash}.")

        if(result.added.isEmpty && result.changed.isEmpty) {
          println("No changes in the repository since last commit.")
        }

        // First print any changed files - i.e. those that exist in the current commit's tree
        // but have different hashes in the working tree lets print these first
        if(result.changed.nonEmpty) {
          println("Files with changes:")
          result.changed
            .map(path => "        " ++ path)
            .foreach(println)
        }

        // Then lets print new files - i.e. those that aren't currently present in the commit's tree
        if(result.added.nonEmpty) {
          println("Files not yet tracked:")
          result.added
            .map(path => "        " ++ path)
            .foreach(println)
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
              if(Common.hash(fileBytes) != existingEntry.hash) {
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
}