package mygit

import com.monovore.decline.Command
import com.monovore.decline.Opts
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.Path

object Commands {

  val init = Command("init", "Initialize git-in-scala repository in the current directory")(Opts {
    Git.init()
  })


  val commitMessage = Opts.option[String]("message", "Message attached to your commit", "m")
  def commit = Command("commit", "Create a commit with all the changes you've made.")(commitMessage.map { message =>
    Git.commit(message)
  })


  def log = Command("log", "Display a log of commits, starting with the current HEAD")( Opts {
    Git.log()
  })

}