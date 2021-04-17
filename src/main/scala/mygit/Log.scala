package mygit

import scala.annotation.tailrec

object Log {

  def log(): Unit = {
    Common.fetchHead() match {
      case Some(currentCommit) =>
        // Recursively load commits, starting from the current HEAD and going back to the begginging of history
        val commits = loadCommits(currentCommit, List.empty)

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

  @tailrec
  private def loadCommits(commitHash: String, result: List[CommitObject]): List[CommitObject] = {
    CommitObject.load(commitHash) match {
      case Some(commit @ CommitObject(_, _, Some(parentHash), _)) =>
        // This commit has a parent hash so we continue with the recursion
        loadCommits(parentHash, result :+ commit)
      case Some(commit) =>
        // Commit has no parent hash so its the last one
        result :+ commit
      case None =>
        // In case of an error we let the user know and stop
        println(s"Repository is in a broken state - could not read object with hash $commitHash")
        List.empty
    }
  }
}