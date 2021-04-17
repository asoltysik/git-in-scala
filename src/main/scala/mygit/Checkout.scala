package mygit

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

object Checkout {

  def checkout(hash: String): Unit = {
    getCurrentAndTargetTrees(hash).map { case (currentTree, targetTree) =>
      // We first delete everything from the working tree based on current commit tree,
      // then recreate the working tree based on the target commit tree.
      // This is a simple but inefficient way of doing checkouts, but it shows the point nicely.
      // One idea to speed this up would be to compute a difference of the current and target trees and change
      // only the files that are present in the difference.
      deleteEverythingFromTree(currentTree, root = Path.of(""))
      rebuildEverythingFromTree(targetTree, root = Path.of(""))
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
      currentCommitHash <- Common.fetchHead()
      currentCommit     <- CommitObject.load(currentCommitHash)
      targetCommit      <- CommitObject.load(targetCommit)
    } yield (ExpandedTree.load(currentCommit.treeHash), ExpandedTree.load(targetCommit.treeHash))
}