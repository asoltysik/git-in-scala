package mygit

import java.nio.file.Paths

object Init {
  def init(): Unit = {
    println("Initializing git-in-scala repository.")

    Paths.get(".git-in-scala", "objects").toFile.mkdirs()
    Paths.get(".git-in-scala", "refs", "heads").toFile.mkdirs()
    Paths.get(".git-in-scala", "HEAD").toFile.createNewFile()

    ()
  }
}